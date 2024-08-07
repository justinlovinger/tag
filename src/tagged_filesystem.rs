use std::{collections::BTreeSet, path::PathBuf, sync::Arc};

use filesystem::{DirEntry, FileSystem};
use itertools::Itertools;
use rayon::prelude::*;
use rustc_hash::FxHashSet;

use crate::{organize::organize, tagged_file::NewError, types::MoveOp, Tag, TagRef, TaggedFile};

#[derive(Debug)]
pub struct TaggedFilesystemBuilder<F> {
    fs: F,
    dry_run: bool,
    verbose: bool,
}

#[derive(Debug)]
pub struct TaggedFilesystem<F> {
    fs: F,
    dry_run: bool,
    verbose: bool,
}

#[derive(Clone, Debug, PartialEq)]
enum Op {
    EnsureDirectory(PathBuf),
    Move(MoveOp),
    DeleteDirectoryIfEmpty(PathBuf),
}

impl From<MoveOp> for Op {
    fn from(value: MoveOp) -> Self {
        Op::Move(value)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum ModError {
    HasTag(#[from] HasTagError),
    LacksTag(#[from] LacksTagError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}`")]
pub struct HasTagError(TaggedFile, String);

impl HasTagError {
    fn new<T>(file: TaggedFile, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(file, tag.as_ref().to_string())
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` lacks `{1}`")]
pub struct LacksTagError(TaggedFile, String);

impl LacksTagError {
    fn new<T>(file: TaggedFile, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(file, tag.as_ref().to_string())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum PathError {
    NonUniqueTag(#[from] NonUniqueTagError),
    New(#[from] NewError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("Tag is not unique")]
pub struct NonUniqueTagError;

impl<F> TaggedFilesystemBuilder<F> {
    pub fn new(fs: F) -> Self {
        Self {
            fs,
            dry_run: false,
            verbose: false,
        }
    }

    pub fn dry_run(mut self, value: bool) -> Self {
        self.dry_run = value;
        self
    }

    pub fn verbose(mut self, value: bool) -> Self {
        self.verbose = value;
        self
    }

    pub fn build(self) -> TaggedFilesystem<F> {
        TaggedFilesystem {
            fs: self.fs,
            dry_run: self.dry_run,
            verbose: self.verbose,
        }
    }
}

impl<F> TaggedFilesystem<F>
where
    F: FileSystem + Clone + Send + Sync + 'static,
{
    pub fn new(fs: F) -> Self {
        Self {
            fs,
            dry_run: false,
            verbose: false,
        }
    }

    pub fn add(&self, tag: Tag, files: FxHashSet<TaggedFile>) -> Result<Vec<PathBuf>, ModError> {
        self.modify([tag].into_iter().collect(), FxHashSet::default(), files)
    }

    pub fn del(&self, tag: Tag, files: FxHashSet<TaggedFile>) -> Result<Vec<PathBuf>, ModError> {
        self.modify(FxHashSet::default(), [tag].into_iter().collect(), files)
    }

    pub fn modify(
        &self,
        add: FxHashSet<Tag>,
        del: FxHashSet<Tag>,
        files: FxHashSet<TaggedFile>,
    ) -> Result<Vec<PathBuf>, ModError> {
        if add.is_empty() && del.is_empty() {
            return Ok(vec![]);
        }

        let files = Arc::new(files);
        let mut relevant_files = relevant_files(
            files
                .iter()
                .flat_map(|file| file.tags().map(|tag| tag.to_owned()))
                .chain(add.iter().cloned())
                .collect::<FxHashSet<_>>(),
            self.filtered_tagged_files({
                let files = Arc::clone(&files);
                move |file| !files.contains(file)
            })
            .collect(),
        );
        let files = Arc::try_unwrap(files).unwrap_or_else(|arc| (*arc).clone());

        let mut move_ops = Vec::new();
        for file in files {
            for tag in file.tags() {
                if add.contains(tag) {
                    return Err(HasTagError::new(file.clone(), tag).into());
                }
            }

            let mut deleted_count = 0;
            // This method is safe
            // because `tags` were already checked for duplicates,
            // and `name` is from a valid `TaggedFile`.
            let new_file = unsafe {
                TaggedFile::from_tags_unchecked(
                    file.tags()
                        .filter(|tag| {
                            if del.contains(*tag) {
                                deleted_count += 1;
                                false
                            } else {
                                true
                            }
                        })
                        .chain(add.iter().map(|tag| tag.as_ref())),
                    file.name(),
                )
            };
            if deleted_count != del.len() {
                let mut del = del;
                for tag in file.tags() {
                    del.remove(tag);
                }
                return Err(LacksTagError::new(file, del.into_iter().next().unwrap()).into());
            }

            move_ops.push(MoveOp {
                from: file.into_path(),
                to: new_file.as_path().to_owned(),
            });
            relevant_files.push(new_file);
        }

        let mut organized_move_ops = organize(&relevant_files);
        let mut new_paths = Vec::new();
        for op in move_ops {
            match organized_move_ops
                .iter_mut()
                .find(|other| other.from == op.to)
            {
                Some(other) => {
                    other.from = op.from;
                    new_paths.push(other.to.clone());
                }
                None => {
                    let to = op.to.clone();
                    organized_move_ops.push(op);
                    new_paths.push(to);
                }
            }
        }

        self.apply_all(from_move_ops(organized_move_ops).collect())?;
        Ok(new_paths)
    }

    pub fn path<S>(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: S,
    ) -> Result<PathBuf, PathError>
    where
        S: AsRef<str> + Send + Sync,
    {
        let mut tag_set = FxHashSet::default();
        for tag in tags.into_iter() {
            if !tag_set.insert(tag) {
                return Err(NonUniqueTagError.into());
            }
        }

        let mut files = relevant_files(tag_set.clone(), self.tagged_files().collect());
        let (file_is_fake, from) = match files.par_iter().find_any(|file| {
            file.tags_len() == tag_set.len()
                && file.tags().all(|tag| tag_set.contains(tag))
                && file.name() == name.as_ref()
        }) {
            Some(file) => (false, file.as_path().to_owned()),
            None => {
                let file = TaggedFile::from_tags(tag_set.iter(), name)?;
                let from = file.as_path().to_owned();
                files.push(file);
                (true, from)
            }
        };

        let mut organized_move_ops = organize(&files);

        let enumerated_to = organized_move_ops
            .iter()
            .enumerate()
            .find(|(_, op)| op.from == from)
            .map(|(i, op)| (i, op.to.clone()));
        if file_is_fake {
            if let Some((i, _)) = enumerated_to {
                organized_move_ops.swap_remove(i);
            }
        }
        self.apply_all(from_move_ops(organized_move_ops).collect())?;

        Ok(enumerated_to.map_or(from, |(_, to)| to))
    }

    pub fn organize(&self) -> std::io::Result<()> {
        self.apply_all(from_move_ops(organize(&self.tagged_files().collect_vec())).collect())?;
        Ok(())
    }

    pub fn find(
        &self,
        include: Vec<Tag>,
        exclude: Vec<Tag>,
    ) -> std::io::Result<impl Iterator<Item = TaggedFile>> {
        Ok(self.filtered_tagged_files(move |file| {
            include
                .iter()
                .all(|tag| file.tags().contains(&tag.as_ref()))
                && !exclude
                    .iter()
                    .any(|tag| file.tags().contains(&tag.as_ref()))
        }))
    }

    fn tagged_files(&self) -> impl Iterator<Item = TaggedFile> {
        let (sender, receiver) = crossbeam_channel::unbounded();
        self.for_each_tagged_file(move |file| sender.send(file).unwrap());
        receiver.into_iter()
    }

    fn filtered_tagged_files(
        &self,
        predicate: impl Fn(&TaggedFile) -> bool + Send + Sync + 'static,
    ) -> impl Iterator<Item = TaggedFile> {
        let (sender, receiver) = crossbeam_channel::unbounded();
        self.for_each_tagged_file(move |file| {
            if predicate(&file) {
                sender.send(file).unwrap()
            }
        });
        receiver.into_iter()
    }

    fn for_each_tagged_file<C>(&self, callback: C)
    where
        C: Fn(TaggedFile) + Send + Sync + 'static,
    {
        let fs = self.fs.clone();
        rayon::spawn(move || {
            rayon::scope(|s| Self::for_each_tagged_file_(&fs, s, &callback, read_cwd(&fs).unwrap()))
        });
    }

    fn for_each_tagged_file_<'a, 'scope, C>(
        fs: &'a F,
        scope: &rayon::Scope<'scope>,
        callback: &'scope C,
        paths: impl Iterator<Item = std::io::Result<PathBuf>>,
    ) where
        'a: 'scope,
        C: Fn(TaggedFile) + Send + Sync,
    {
        for path in paths {
            scope.spawn(|scope| {
                let path = path.unwrap();
                match TaggedFile::from_path(path) {
                    Ok(file) => callback(file),
                    Err(e) => {
                        let path = e.into_path();
                        if fs.is_dir(&path) {
                            Self::for_each_tagged_file_(
                                fs,
                                scope,
                                callback,
                                read_dir(fs, path).unwrap(),
                            )
                        }
                    }
                }
            })
        }
    }

    fn apply_all(&self, ops: Vec<Op>) -> std::io::Result<()> {
        if let Err(e) = self.apply_all_(&ops) {
            if self.verbose {
                println!("Error occured: {e}");
                println!("Cleaning up");
            }
            ops.into_iter()
                .filter_map(|op| match op {
                    Op::EnsureDirectory(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                    Op::Move(MoveOp { .. }) => None,
                    Op::DeleteDirectoryIfEmpty(path) => Some(Op::DeleteDirectoryIfEmpty(path)),
                })
                .for_each(|op| {
                    if let Err(e) = self.apply_(&op) {
                        eprintln!("{e}");
                    }
                });
            Err(e)
        } else {
            Ok(())
        }
    }

    fn apply_all_<'a>(&self, ops: impl IntoIterator<Item = &'a Op>) -> std::io::Result<()> {
        for op in ops.into_iter() {
            self.apply_(op)?
        }
        Ok(())
    }

    fn apply_(&self, op: &Op) -> std::io::Result<()> {
        if self.verbose {
            match op {
                Op::EnsureDirectory(path) => {
                    println!("Ensuring directory `{}` exists", path.display());
                }
                Op::Move(MoveOp { from, to }) => {
                    println!("Moving `{}` to `{}`", from.display(), to.display());
                }
                Op::DeleteDirectoryIfEmpty(path) => {
                    println!("Deleting directory `{}` if empty", path.display());
                }
            }
        }

        if !self.dry_run {
            match op {
                Op::EnsureDirectory(path) => self.fs.create_dir_all(path),
                Op::Move(MoveOp { from, to }) => {
                    // This utility should only organize data,
                    // never delete it.
                    if self.fs.is_file(to) {
                        Err(std::io::Error::new(
                            std::io::ErrorKind::AlreadyExists,
                            format!(
                                "cannot move `{}` to `{}`, destination already exists",
                                from.display(),
                                to.display()
                            ),
                        ))
                    } else {
                        self.fs.rename(from, to)
                    }
                }
                Op::DeleteDirectoryIfEmpty(path) => {
                    // Note,
                    // we can do the following with nightly Rust,
                    // which may be more efficient than `self.fs.read_dir(&path)?.next().is_none()`:
                    // ```
                    // if let Err(e) = self.fs.remove_dir(path) {
                    //     if e.kind() != std::io::ErrorKind::DirectoryNotEmpty {
                    //         return Err(e);
                    //     }
                    // }
                    // ```
                    if self.fs.read_dir(path)?.next().is_none() {
                        self.fs.remove_dir(path)
                    } else {
                        Ok(())
                    }
                }
            }
        } else {
            Ok(())
        }
    }
}

fn relevant_files(mut tags: FxHashSet<Tag>, mut files: Vec<TaggedFile>) -> Vec<TaggedFile> {
    let mut relevant_files = Vec::new();
    loop {
        let prev_tags_len = tags.len();
        let mut i = 0;
        while i < files.len() {
            if files[i].tags().any(|tag| tags.contains(tag)) {
                let file = files.swap_remove(i);
                tags.extend(file.tags().map(|tag| tag.to_owned()));
                relevant_files.push(file);
            } else {
                i += 1;
            }
        }
        if tags.len() == prev_tags_len {
            return relevant_files;
        }
    }
}

fn from_move_ops(ops: Vec<MoveOp>) -> impl Iterator<Item = Op> {
    // We know a directory exists
    // if a file is moving from it.
    let from_ancestors = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| from.ancestors().skip(1)),
    );
    let ensure_dirs = BTreeSet::from_iter(
        ops.iter()
            .filter_map(|MoveOp { from: _, to }| to.parent())
            .filter(|to| !from_ancestors.contains(to)),
    );
    // We do not need to ensure a parent exists
    // if its child is already being ensured.
    let ensure_dir_ancestors =
        BTreeSet::from_iter(ensure_dirs.iter().flat_map(|path| path.ancestors().skip(1)));
    let ensure_dirs = BTreeSet::from_iter(
        ensure_dirs
            .into_iter()
            .filter(|to| !ensure_dir_ancestors.contains(to))
            .map(|x| x.to_owned()),
    );

    // We know a directory will not be empty
    // if a file is moving to it.
    let to_ancestors = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from: _, to }| to.ancestors().skip(1)),
    );
    let del_dirs = BTreeSet::from_iter(
        ops.iter()
            .flat_map(|MoveOp { from, to: _ }| {
                // If a child will not be empty,
                // neither will its parents.
                from.ancestors()
                    .skip(1)
                    .take_while(|from| !to_ancestors.contains(from))
            })
            .map(|x| x.to_owned()),
    );

    ensure_dirs
        .into_iter()
        .map(Op::EnsureDirectory)
        .chain(ops.into_iter().map_into())
        // `rev()` to check child directories before parents.
        .chain(del_dirs.into_iter().rev().map(Op::DeleteDirectoryIfEmpty))
}

// The rust standard library has unexpected behavior for `std::io::read_dir("")`,
// see <https://github.com/rust-lang/rust/issues/114149>.
fn read_cwd<F>(fs: &F) -> std::io::Result<impl Iterator<Item = std::io::Result<PathBuf>>>
where
    F: FileSystem,
{
    Ok(fs.read_dir(".")?.map(|res| {
        res.map(|x| {
            x.path()
                .strip_prefix("./")
                .expect("file from `.` should start with `./`")
                .to_owned()
        })
    }))
}

fn read_dir<F>(
    fs: &F,
    dir: PathBuf,
) -> std::io::Result<impl Iterator<Item = std::io::Result<PathBuf>>>
where
    F: FileSystem,
{
    Ok(fs.read_dir(dir)?.map(|res| res.map(|x| x.path())))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use filesystem::FakeFileSystem;
    use proptest::prelude::{prop::collection::vec, *};
    use test_strategy::proptest;

    use crate::{
        testing::{
            fake_filesystem_with, make_file_and_parent, tagged_files_strategy, TagSetTaggedFile,
            TaggedFileSystemWithMetadata, TaggedFilesParams,
        },
        Tag,
    };

    use super::*;

    #[test]
    fn mod_renames_file() {
        let file = TaggedFile::new("foo-_baz".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .modify(
                [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                [].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["bar-foo-_baz"].map(PathBuf::from)
        );

        let file = TaggedFile::new("foo-_baz".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .modify(
                [].into_iter().collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        assert_eq!(list_files(&filesystem.fs), ["_baz"].map(PathBuf::from));

        let file = TaggedFile::new("foo-_baz".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .modify(
                [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        assert_eq!(list_files(&filesystem.fs), ["bar-_baz"].map(PathBuf::from));
    }

    #[test]
    fn mod_renames_all_files() {
        let files = ["foo/_bar", "foo/_foo"];
        let filesystem = fake_filesystem_with(files);
        filesystem
            .modify(
                [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                files
                    .into_iter()
                    .map(|file| TaggedFile::new(file.to_owned()).unwrap())
                    .collect(),
            )
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["bar/_bar", "bar/_foo"].map(PathBuf::from)
        );
    }

    #[test]
    fn mod_adds_multiple_tags() {
        let file = TaggedFile::new("foo-_baz".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .modify(
                [
                    Tag::new("bar".to_owned()).unwrap(),
                    Tag::new("baz".to_owned()).unwrap(),
                ]
                .into_iter()
                .collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["bar-baz-_baz"].map(PathBuf::from)
        );
    }

    #[test]
    fn mod_deletes_multiple_tags() {
        let file = TaggedFile::new("foo-baz-_baz".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .modify(
                [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                [
                    Tag::new("foo".to_owned()).unwrap(),
                    Tag::new("baz".to_owned()).unwrap(),
                ]
                .into_iter()
                .collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        assert_eq!(list_files(&filesystem.fs), ["bar-_baz"].map(PathBuf::from));
    }

    #[proptest(cases = 20)]
    fn mod_does_not_change_file_if_no_tags_given(
        #[strategy(TaggedFilesystem::<FakeFileSystem>::arbitrary_with(TaggedFilesParams {
            min_files: 1, ..TaggedFilesParams::default()
        }))]
        filesystem: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
    ) {
        filesystem.organize().unwrap();

        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();

        filesystem
            .modify(
                [].into_iter().collect(),
                [].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), files)
    }

    #[proptest(cases = 20)]
    fn mod_organizes_files(
        #[strategy(TaggedFileSystemWithMetadata::arbitrary_with(TaggedFilesParams {
            min_tags: 1, min_files: 1, ..TaggedFilesParams::default()
        }))]
        args: TaggedFileSystemWithMetadata,
        file_index: usize,
        tag: Tag,
        tag_index: usize,
    ) {
        let filesystem = args.filesystem;
        let tags = args.tags;

        filesystem.organize().unwrap();

        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let del_tag = file
            .tags()
            .nth(tag_index % file.tags_len())
            .unwrap()
            .to_owned();
        let add_tag = tags
            .into_iter()
            .find(|tag| !file.tags().contains(&tag.as_ref()))
            .unwrap_or({
                prop_assume!(!file.tags().contains(&tag.as_ref()));
                tag
            });

        let expected = TaggedFilesystem::new(clone_fake_fs(&filesystem.fs));
        prop_assume!(expected
            .apply_all(
                from_move_ops(vec![MoveOp {
                    from: file.as_path().to_owned(),
                    to: TaggedFile::from_tags(
                        file.tags()
                            .filter(|tag| *tag != del_tag.as_ref())
                            .chain([add_tag.as_ref()]),
                        file.name()
                    )
                    .unwrap()
                    .into_path()
                }])
                .collect()
            )
            .is_ok());
        prop_assume!(expected.organize().is_ok());

        filesystem
            .modify(
                [add_tag].into_iter().collect(),
                [del_tag].into_iter().collect(),
                [file].into_iter().collect(),
            )
            .unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), list_files(&expected.fs))
    }

    #[test]
    fn mod_returns_error_if_file_already_has_tag() {
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        assert!(filesystem
            .modify(
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [].into_iter().collect(),
                [file].into_iter().collect()
            )
            .is_err());
    }

    #[test]
    fn mod_returns_error_if_file_lacks_tag() {
        let file = TaggedFile::new("_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        assert!(filesystem
            .modify(
                [].into_iter().collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [file].into_iter().collect()
            )
            .is_err());
    }

    #[test]
    fn mod_returns_error_if_file_does_not_exist() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        assert!(filesystem
            .modify(
                [Tag::new("bar".to_owned()).unwrap()].into_iter().collect(),
                [Tag::new("foo".to_owned()).unwrap()].into_iter().collect(),
                [TaggedFile::new("foo-_baz".to_owned()).unwrap()]
                    .into_iter()
                    .collect()
            )
            .is_err());
    }

    #[test]
    fn path_returns_path_if_valid() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        assert_eq!(
            filesystem
                .path(
                    [
                        Tag::new("foo".to_owned()).unwrap(),
                        Tag::new("bar".to_owned()).unwrap()
                    ],
                    "baz"
                )
                .unwrap(),
            PathBuf::from("bar-foo-_baz")
        );
    }

    #[proptest(cases = 20)]
    fn path_organizes_files(
        #[strategy(TaggedFilesystem::<FakeFileSystem>::arbitrary_with(TaggedFilesParams {
            min_files: 1, ..TaggedFilesParams::default()
        }))]
        expected: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
    ) {
        let files = list_files(&expected.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let filesystem = TaggedFilesystem::new(clone_fake_fs(&expected.fs));
        filesystem.fs.remove_file(&file).unwrap();
        for path in file
            .as_path()
            .ancestors()
            .skip(1)
            // `FakeFileSystem` allows removing `/`,
            // which results in unexpected behavior.
            .take_while(|path| !path.as_os_str().is_empty())
        {
            let _ = filesystem.fs.remove_dir(path); // Only remove if empty.
        }

        expected.organize().unwrap();

        filesystem.organize().unwrap();
        let path = filesystem
            .path(file.tags().map(|tag| tag.to_owned()), file.name())
            .unwrap();
        filesystem.fs.create_file(path, "").unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), list_files(&expected.fs))
    }

    #[proptest(cases = 20)]
    fn path_returns_path_if_file_already_exists(
        #[strategy(TaggedFilesystem::<FakeFileSystem>::arbitrary_with(TaggedFilesParams {
            min_files: 1, ..TaggedFilesParams::default()
        }))]
        filesystem: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
    ) {
        filesystem.organize().unwrap();
        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        prop_assert_eq!(
            filesystem
                .path(file.tags().map(|tag| tag.to_owned()), file.name())
                .unwrap(),
            file.as_path()
        )
    }

    #[test]
    fn path_returns_err_if_tag_is_duplicate() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        assert!(filesystem
            .path(
                [
                    Tag::new("foo".to_owned()).unwrap(),
                    Tag::new("foo".to_owned()).unwrap()
                ],
                "baz"
            )
            .is_err());
    }

    #[test]
    fn organize_moves_files_into_optimal_tag_directories() {
        let filesystem =
            fake_filesystem_with(["a/b/c/_foo", "a-b-_bar", "d/e-_baz", "🙂/🙁/_fez", "_fiz"]);
        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["_fiz", "a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙁-🙂-_fez"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_moves_files_into_optimal_tag_directories_when_all_have_same_tags() {
        let filesystem = fake_filesystem_with(["a-b-c-_bar", "a/b/c/_foo"]);
        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-b-c/_bar", "a-b-c/_foo"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_breaks_ties_in_favor_of_increasing_length() {
        let filesystem = fake_filesystem_with(["a/bb/_1", "bb/_2", "a/_3", "dd-ccc-_4"]);
        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-_3", "bb/_2", "bb/a-_1", "ccc-dd-_4"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_ignores_untagged_files() {
        let filesystem = fake_filesystem_with(["a/_foo", "a/not-tagged"]);
        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a/not-tagged", "a-_foo"].map(PathBuf::from),
        )
    }

    #[proptest(cases = 20)]
    fn organize_ignores_nested_tagged_files(
        #[strategy(
            tagged_files_strategy(TaggedFilesParams { max_tag_set: 10, max_files: 10, ..TaggedFilesParams::default() })
                .prop_flat_map(|files| {
                    (
                        vec(tagged_files_strategy(TaggedFilesParams { max_tag_set: 10, max_files: 10, ..TaggedFilesParams::default() }),
                        files.len()), Just(files)
                    )
                })
                .prop_map(|(x, y)| (y, x))
        )]
        args: (Vec<TaggedFile>, Vec<Vec<TaggedFile>>),
    ) {
        let (dirs, dir_files) = args;

        let fs = FakeFileSystem::new();
        for dir in dirs.iter() {
            fs.create_dir_all(dir.as_path()).unwrap();
        }
        let filesystem = TaggedFilesystem::new(fs);

        filesystem.organize().unwrap();
        for (dir, files) in list_dirs(&filesystem.fs)
            .into_iter()
            .filter_map(|path| TaggedFile::from_path(path).ok())
            .zip(dir_files)
        {
            for file in files {
                make_file_and_parent(&filesystem.fs, dir.as_path().join(file.as_path()))
            }
        }
        let files = list_files(&filesystem.fs);

        filesystem.organize().unwrap();
        prop_assert_eq!(files, list_files(&filesystem.fs))
    }

    #[proptest(cases = 20)]
    fn organize_is_idempotent(filesystem: TaggedFilesystem<FakeFileSystem>) {
        filesystem.organize().unwrap();
        let first_pass_files = list_files(&filesystem.fs);
        filesystem.organize().unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), first_pass_files);
    }

    #[proptest(cases = 20)]
    fn organize_does_not_change_tags_or_names(filesystem: TaggedFilesystem<FakeFileSystem>) {
        let files = list_files(&filesystem.fs)
            .into_iter()
            .map(|file| TaggedFile::from_path(file).unwrap())
            .collect_vec();
        filesystem.organize().unwrap();
        let organized_files = list_files(&filesystem.fs)
            .into_iter()
            .map(|file| TaggedFile::from_path(file).unwrap())
            .collect_vec();
        prop_assert_eq!(
            BTreeSet::from_iter(organized_files.iter().map(TagSetTaggedFile::new)),
            BTreeSet::from_iter(files.iter().map(TagSetTaggedFile::new))
        );
    }

    #[proptest(cases = 20)]
    fn organize_ignores_hidden_files(filesystem: TaggedFilesystem<FakeFileSystem>) {
        for path in filesystem.fs.read_dir("").unwrap() {
            let path = path.unwrap().path();
            filesystem
                .fs
                .rename(&path, format!(".{}", path.display()))
                .unwrap();
        }

        let files = list_files(&filesystem.fs);
        filesystem.organize().unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), files);
    }

    #[test]
    fn organize_returns_err_if_files_have_same_tags_and_name() {
        // These files cannot be organized
        // without deleting one or the other.
        let filesystem = fake_filesystem_with(["foo/_bar", "foo-_bar"]);
        assert!(filesystem.organize().is_err());
    }

    #[test]
    #[ignore] // This test is slow.
    fn organize_does_not_panic_on_many_non_unique_tags() {
        // `organize` is prone to stack-overflow
        // if recursion is not handled carefully.
        // This tests we support at least `BREADTH * DEPTH` non-unique tags.
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        const BREADTH: usize = 200;
        const DEPTH: usize = 50;
        let files = (0..BREADTH)
            .flat_map(|n| {
                let x = n * DEPTH;
                let prefix = PathBuf::from((x..x + DEPTH).map(|x| format!("{x:04}")).join("-"));
                [prefix.join("_bar"), prefix.join("_foo")]
            })
            .collect_vec();
        for path in &files {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(list_files(&filesystem.fs), files)
    }

    #[test]
    fn organize_uses_directories_if_filename_gets_too_long() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);

        // We want to test both unique and non-unique tags.
        // They may be handled differently.
        {
            let filesystem = fake_filesystem_with([format!("{a}/{b}/{c}/_foo")]);
            filesystem.organize().unwrap();
            assert_eq!(
                list_files(&filesystem.fs),
                [format!("{a}-{b}/{c}-_foo")].map(PathBuf::from),
            );
        }
        {
            let filesystem =
                fake_filesystem_with([format!("{a}/{b}/{c}-_bar"), format!("{a}/{b}-{c}-_foo")]);
            filesystem.organize().unwrap();
            assert_eq!(
                list_files(&filesystem.fs),
                [format!("{a}-{b}/{c}/_bar"), format!("{a}-{b}/{c}/_foo")].map(PathBuf::from),
            );
        }

        // The name could make the last inline tag too long.
        {
            let filesystem = fake_filesystem_with([format!("{a}/{b}/_{c}")]);
            filesystem.organize().unwrap();
            assert_eq!(
                list_files(&filesystem.fs),
                [format!("{a}-{b}/_{c}")].map(PathBuf::from),
            );
        }
    }

    #[proptest(cases = 20)]
    fn add_inverses_del(
        #[strategy(TaggedFilesystem::<FakeFileSystem>::arbitrary_with(TaggedFilesParams {
            min_tags: 1, min_files: 1, ..TaggedFilesParams::default()
        }))]
        filesystem: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
        tag_index: usize,
    ) {
        filesystem.organize().unwrap();

        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let tag = file
            .tags()
            .nth(tag_index % file.tags_len())
            .unwrap()
            .to_owned();

        let new_file = filesystem
            .del(tag.clone(), [file].into_iter().collect())
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        filesystem
            .add(
                tag,
                [TaggedFile::from_path(new_file).unwrap()]
                    .into_iter()
                    .collect(),
            )
            .unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), files)
    }

    #[proptest(cases = 20)]
    fn del_inverses_add(
        #[strategy(TaggedFileSystemWithMetadata::arbitrary_with(TaggedFilesParams {
            min_files: 1, ..TaggedFilesParams::default()
        }))]
        args: TaggedFileSystemWithMetadata,
        file_index: usize,
        tag: Tag,
    ) {
        let filesystem = args.filesystem;
        let tags = args.tags;

        filesystem.organize().unwrap();

        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let tag = tags
            .into_iter()
            .find(|tag| !file.tags().contains(&tag.as_ref()))
            .unwrap_or({
                prop_assume!(!file.tags().contains(&tag.as_ref()));
                tag
            });

        let new_file = filesystem
            .add(tag.clone(), [file].into_iter().collect())
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        filesystem
            .del(
                tag,
                [TaggedFile::from_path(new_file).unwrap()]
                    .into_iter()
                    .collect(),
            )
            .unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), files)
    }

    #[test]
    fn find_returns_files_with_tag() {
        let filesystem = fake_filesystem_with(["foo-_1", "bar-_2"]);
        assert_eq!(
            filesystem
                .find(vec![Tag::new("foo".into()).unwrap()], vec![])
                .unwrap()
                .collect_vec(),
            [TaggedFile::new("foo-_1".into()).unwrap()]
        );
    }

    #[test]
    fn find_returns_files_with_all_tags() {
        let filesystem = fake_filesystem_with(["foo/bar-_1", "foo/_2"]);
        assert_eq!(
            filesystem
                .find(
                    vec![
                        Tag::new("foo".into()).unwrap(),
                        Tag::new("bar".into()).unwrap()
                    ],
                    vec![],
                )
                .unwrap()
                .collect_vec(),
            [TaggedFile::new("foo/bar-_1".into()).unwrap()]
        );
    }

    #[test]
    fn find_does_not_return_files_with_excluded_tags() {
        let filesystem = fake_filesystem_with(["foo/bar-_1", "foo/_2"]);
        assert_eq!(
            filesystem
                .find(
                    vec![Tag::new("foo".into()).unwrap(),],
                    vec![Tag::new("bar".into()).unwrap()],
                )
                .unwrap()
                .collect_vec(),
            [TaggedFile::new("foo/_2".into()).unwrap()]
        );
    }

    fn clone_fake_fs(fs: &FakeFileSystem) -> FakeFileSystem {
        let new_fs = FakeFileSystem::new();
        for file in list_files(fs) {
            make_file_and_parent(&new_fs, file)
        }
        new_fs
    }

    fn list_files(fs: &FakeFileSystem) -> Vec<PathBuf> {
        let mut queue = vec![PathBuf::from("")];
        let mut files = Vec::new();
        while let Some(dir) = queue.pop() {
            if fs.read_dir(&dir).unwrap().next().is_none() && !dir.as_os_str().is_empty() {
                files.push(dir);
            } else {
                for file in fs.read_dir(dir).unwrap() {
                    let file = file.unwrap().path();
                    if fs.is_dir(&file) {
                        queue.push(file);
                    } else {
                        files.push(file);
                    }
                }
            }
        }
        files.sort();
        files
    }

    fn list_dirs(fs: &FakeFileSystem) -> Vec<PathBuf> {
        let mut queue = vec![PathBuf::from("")];
        let mut dirs = queue.clone();
        while let Some(dir) = queue.pop() {
            for file in fs.read_dir(dir).unwrap() {
                let file = file.unwrap().path();
                if fs.is_dir(&file) {
                    queue.push(file.clone());
                    dirs.push(file);
                }
            }
        }
        dirs.sort();
        dirs
    }
}
