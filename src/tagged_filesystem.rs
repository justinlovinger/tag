use std::{
    collections::{BTreeSet, HashSet},
    path::{Path, PathBuf},
};

use auto_enums::auto_enum;
use filesystem::{DirEntry, FileSystem};
use itertools::Itertools;

use crate::{
    organize::organize,
    tagged_file::{HasTagError, LacksTagError},
    types::MoveOp,
    Tag, TaggedFile, INLINE_SEPARATOR,
};

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
pub enum AddError {
    HasTagError(#[from] HasTagError),
    MoveAndOrganizeError(#[from] MoveAndOrganizeError),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum DelError {
    LacksTagError(#[from] LacksTagError),
    MoveAndOrganizeError(#[from] MoveAndOrganizeError),
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum MoveAndOrganizeError {
    FileNotFoundError(#[from] FileNotFoundError),
    FilesystemError(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("File '{0}' not found")]
pub struct FileNotFoundError(PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum PathError {
    NonUniqueTagError(#[from] NonUniqueTagError),
    FilesystemError(#[from] std::io::Error),
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
    F: FileSystem + 'static,
{
    pub fn new(fs: F) -> Self {
        Self {
            fs,
            dry_run: false,
            verbose: false,
        }
    }

    pub fn add(
        &self,
        tag: Tag,
        files: impl IntoIterator<Item = TaggedFile>,
    ) -> Result<Vec<PathBuf>, AddError> {
        Ok(self.move_and_organize(
            files
                .into_iter()
                .map(|file| file.add_inline_tag(&tag))
                .collect::<Result<Vec<_>, _>>()?,
        )?)
    }

    pub fn del(
        &self,
        tag: Tag,
        files: impl IntoIterator<Item = TaggedFile>,
    ) -> Result<Vec<PathBuf>, DelError> {
        Ok(self.move_and_organize(
            files
                .into_iter()
                .map(|file| file.del_tag(&tag))
                .collect::<Result<Vec<_>, _>>()?,
        )?)
    }

    pub fn path<S>(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: S,
    ) -> Result<PathBuf, PathError>
    where
        S: AsRef<str>,
    {
        let mut tag_set = HashSet::new();
        for tag in tags.into_iter() {
            if !tag_set.insert(tag) {
                return Err(NonUniqueTagError.into());
            }
        }

        let mut files = self.find_tagged_files("".into())?;
        let (file_is_fake, from) = match files.iter().find(|other| {
            other.tags_len() == tag_set.len()
                && other.tags().all(|tag| tag_set.contains(tag))
                && other.name() == name.as_ref()
        }) {
            Some(file) => (false, file.as_path().to_owned()),
            None => {
                let file = TaggedFile::new(format!(
                    "{}_{}",
                    tag_set.into_iter().format_with("", |tag, f| {
                        f(&tag)?;
                        f(&INLINE_SEPARATOR)?;
                        Ok(())
                    }),
                    name.as_ref(),
                ))
                .expect("file should be valid");
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
        self.apply_all(from_move_ops(organized_move_ops))?;

        Ok(enumerated_to.map_or(from, |(_, to)| to))
    }

    pub fn organize(&self) -> std::io::Result<()> {
        let files = self.find_tagged_files("".into())?;
        self.apply_all(from_move_ops(organize(&files)))?;
        Ok(())
    }

    fn move_and_organize(
        &self,
        move_ops: Vec<MoveOp>,
    ) -> Result<Vec<PathBuf>, MoveAndOrganizeError> {
        let mut files = self.find_tagged_files("".into())?;
        for op in move_ops.iter() {
            match files.iter_mut().find(|file| file.as_path() == op.from) {
                Some(f) => {
                    *f = TaggedFile::from_path(op.to.clone())
                        .expect("file should be valid after adding tag");
                }
                None => return Err(FileNotFoundError(op.from.clone()).into()),
            }
        }

        let mut organized_move_ops = organize(&files);
        let new_paths = move_ops
            .into_iter()
            .map(|op| {
                match organized_move_ops
                    .iter_mut()
                    .find(|other| other.from == op.to)
                {
                    Some(other) => {
                        other.from = op.from;
                        other.to.clone()
                    }
                    None => {
                        let to = op.to.clone();
                        organized_move_ops.push(op);
                        to
                    }
                }
            })
            .collect();

        self.apply_all(from_move_ops(organized_move_ops))?;
        Ok(new_paths)
    }

    fn find_tagged_files(&self, root: PathBuf) -> std::io::Result<Vec<TaggedFile>> {
        let mut dirs = vec![root];
        let mut files = Vec::new();
        while let Some(dir) = dirs.pop() {
            for file in self.sane_read_dir(dir)? {
                match TaggedFile::from_path(file?) {
                    Ok(x) => files.push(x),
                    Err(x) => {
                        let x = x.into_path();
                        if self.fs.is_dir(&x) {
                            dirs.push(x);
                        }
                    }
                }
            }
        }
        Ok(files)
    }

    fn apply_all<O>(&self, ops: impl Iterator<Item = O>) -> std::io::Result<()>
    where
        O: Into<Op>,
    {
        for op in ops {
            self.apply(op)?
        }
        Ok(())
    }

    fn apply<O>(&self, op: O) -> std::io::Result<()>
    where
        O: Into<Op>,
    {
        match op.into() {
            Op::EnsureDirectory(path) => {
                if self.verbose {
                    println!("Ensuring directory `{}` exists", path.display());
                }

                if self.dry_run {
                    Ok(())
                } else {
                    self.fs.create_dir_all(path)
                }
            }
            Op::Move(MoveOp { from, to }) => {
                if self.verbose {
                    println!("Moving `{}` to `{}`", from.display(), to.display());
                }

                // This utility should only organize data,
                // never delete it.
                if self.fs.is_file(&to) {
                    Err(std::io::Error::new(
                        std::io::ErrorKind::AlreadyExists,
                        format!(
                            "cannot move `{}` to `{}`, destination already exists",
                            from.display(),
                            to.display()
                        ),
                    ))
                } else if self.dry_run {
                    Ok(())
                } else {
                    self.fs.rename(from, to)
                }
            }
            Op::DeleteDirectoryIfEmpty(path) => {
                if self.verbose {
                    println!("Deleting directory `{}` if empty", path.display());
                }

                // Note,
                // we can do the following with nightly Rust,
                // which may be more efficient than `self.sane_read_dir(&path)?.next().is_none()`:
                // ```
                // if let Err(e) = self.fs.remove_dir(path) {
                //     if e.kind() != std::io::ErrorKind::DirectoryNotEmpty {
                //         return Err(e);
                //     }
                // }
                // ```
                if self.dry_run {
                    Ok(())
                } else if self.sane_read_dir(&path)?.next().is_none() {
                    self.fs.remove_dir(path)
                } else {
                    Ok(())
                }
            }
        }
    }

    // The rust standard library has unexpected behavior for `std::io::read_dir("")`,
    // see <https://github.com/rust-lang/rust/issues/114149>.
    #[auto_enum]
    fn sane_read_dir<P>(
        &self,
        path: P,
    ) -> std::io::Result<impl Iterator<Item = std::io::Result<PathBuf>> + 'static>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        #[auto_enum(Iterator)]
        let it = if path.as_os_str().is_empty() {
            self.fs.read_dir(".")?.map(|res| {
                res.map(|x| {
                    x.path()
                        .strip_prefix("./")
                        .expect("file from `.` should start with `./`")
                        .to_owned()
                })
            })
        } else {
            self.fs.read_dir(path)?.map(|res| res.map(|x| x.path()))
        };
        Ok(it)
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
    fn add_renames_file_if_file_does_not_have_tag() {
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .add(Tag::new("baz".to_owned()).unwrap(), [file])
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["baz-foo-_bar"].map(PathBuf::from)
        );
    }

    #[test]
    fn add_renames_all_files() {
        let files = ["foo-_bar", "bar-_foo"];
        let filesystem = fake_filesystem_with(files);
        filesystem
            .add(
                Tag::new("baz".to_owned()).unwrap(),
                files.map(|file| TaggedFile::new(file.to_owned()).unwrap()),
            )
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["baz/bar-_foo", "baz/foo-_bar"].map(PathBuf::from)
        );
    }

    #[proptest(cases = 20)]
    fn add_organizes_files(
        #[strategy(TaggedFileSystemWithMetadata::arbitrary_with(TaggedFilesParams {
            min_files: 1, ..TaggedFilesParams::default()
        }))]
        args: TaggedFileSystemWithMetadata,
        file_index: usize,
        tag: Tag,
    ) {
        let filesystem = args.filesystem;
        let tags = args.tags;

        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let tag = tags
            .into_iter()
            .find(|tag| !file.tags().contains(&tag.as_ref()))
            .unwrap_or(tag);

        let with_inline = TaggedFilesystem::new(clone_fake_fs(&filesystem.fs));
        with_inline
            .fs
            .rename(&file, file.clone().add_inline_tag(&tag).unwrap().to)
            .unwrap();
        with_inline.organize().unwrap();

        filesystem.add(tag, [file]).unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), list_files(&with_inline.fs))
    }

    #[test]
    fn add_returns_error_if_file_already_has_tag() {
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        assert!(filesystem
            .add(Tag::new("foo".to_owned()).unwrap(), [file])
            .is_err());
    }

    #[test]
    fn add_returns_error_if_file_does_not_exist() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        assert!(filesystem
            .add(
                Tag::new("foo".to_owned()).unwrap(),
                [TaggedFile::new("_bar".to_owned()).unwrap()]
            )
            .is_err());
    }

    #[test]
    fn del_renames_file_if_file_has_tag() {
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        filesystem
            .del(Tag::new("foo".to_owned()).unwrap(), [file])
            .unwrap();
        assert_eq!(list_files(&filesystem.fs), ["_bar"].map(PathBuf::from));
    }

    #[test]
    fn del_renames_all_files() {
        let files = ["foo/_bar", "foo/_foo"];
        let filesystem = fake_filesystem_with(files);
        filesystem
            .del(
                Tag::new("foo".to_owned()).unwrap(),
                files.map(|file| TaggedFile::new(file.to_owned()).unwrap()),
            )
            .unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["_bar", "_foo"].map(PathBuf::from)
        );
    }

    #[proptest(cases = 20)]
    fn del_organizes_files(
        #[strategy(TaggedFilesystem::<FakeFileSystem>::arbitrary_with(TaggedFilesParams {
            min_tags: 1, min_files: 1, ..TaggedFilesParams::default()
        }))]
        filesystem: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
        tag_index: usize,
    ) {
        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();
        let tag = file
            .tags()
            .nth(tag_index % file.tags_len())
            .unwrap()
            .to_owned();

        let without_tag = TaggedFilesystem::new(clone_fake_fs(&filesystem.fs));
        without_tag
            .apply_all(from_move_ops(vec![file.clone().del_tag(&tag).unwrap()]))
            .unwrap();
        without_tag.organize().unwrap();

        filesystem.del(tag, [file]).unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), list_files(&without_tag.fs))
    }

    #[test]
    fn del_returns_error_if_file_lacks_tag() {
        let file = TaggedFile::new("_bar".to_owned()).unwrap();
        let filesystem = fake_filesystem_with([&file]);
        assert!(filesystem
            .del(Tag::new("foo".to_owned()).unwrap(), [file])
            .is_err());
    }

    #[test]
    fn del_returns_error_if_file_does_not_exist() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        assert!(filesystem
            .del(
                Tag::new("foo".to_owned()).unwrap(),
                [TaggedFile::new("foo-_bar".to_owned()).unwrap()]
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
        filesystem: TaggedFilesystem<FakeFileSystem>,
        file_index: usize,
    ) {
        let files = list_files(&filesystem.fs);
        let file = TaggedFile::from_path(files[file_index % files.len()].clone()).unwrap();

        let without_file = TaggedFilesystem::new(clone_fake_fs(&filesystem.fs));
        without_file.fs.remove_file(&file).unwrap();
        for path in file.as_path().ancestors().skip(1) {
            let _ = without_file.fs.remove_dir(path); // Only remove if empty.
        }

        filesystem.organize().unwrap();

        let path = without_file
            .path(file.tags().map(|tag| tag.to_owned()), file.name())
            .unwrap();
        without_file.fs.create_file(path, "").unwrap();
        prop_assert_eq!(list_files(&without_file.fs), list_files(&filesystem.fs))
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
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for path in ["a/b/c/_foo", "a-b-_bar", "d/e-_baz", "🙂/🙁/_fez", "_fiz"] {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["_fiz", "a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙁-🙂-_fez"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_moves_files_into_optimal_tag_directories_when_all_have_same_tags() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for path in ["a-b-c-_bar", "a/b/c/_foo"] {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-b-c/_bar", "a-b-c/_foo"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_breaks_ties_in_favor_of_increasing_length() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for path in ["a/bb/_1", "bb/_2", "a/_3", "dd-ccc-_4"] {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-_3", "bb/_2", "bb/a-_1", "ccc-dd-_4"].map(PathBuf::from),
        )
    }

    #[test]
    fn organize_ignores_untagged_files() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for path in ["a/_foo", "a/not-tagged"] {
            make_file_and_parent(&filesystem.fs, path);
        }

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

    #[test]
    fn organize_returns_err_if_files_have_same_tags_and_name() {
        // These files cannot be organized
        // without deleting one or the other.
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for path in ["foo/_bar", "foo-_bar"] {
            make_file_and_parent(&filesystem.fs, path);
        }
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
            .del(tag.clone(), [file])
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        filesystem
            .add(tag, [TaggedFile::from_path(new_file).unwrap()])
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
            .unwrap_or(tag);

        let new_file = filesystem
            .add(tag.clone(), [file])
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        filesystem
            .del(tag, [TaggedFile::from_path(new_file).unwrap()])
            .unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), files)
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
