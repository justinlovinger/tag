use std::{
    collections::BTreeSet,
    fmt,
    iter::once,
    path::{Path, PathBuf},
};

use auto_enums::auto_enum;
use filesystem::{DirEntry, FileSystem};
use itertools::Itertools;

use crate::{
    tagged_file::{HasTagError, LacksTagError, MoveOp},
    Tag, TagRef, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END,
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
pub enum Op {
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
pub enum AddError<T> {
    HasTagError(#[from] HasTagError<T>),
    FilesystemError(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum DelError<T> {
    LacksTagError(#[from] LacksTagError<T>),
    FilesystemError(#[from] std::io::Error),
}

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

    pub fn add_tag<T>(&self, tag: T, file: TaggedFile) -> Result<(), AddError<T>>
    where
        T: fmt::Debug + AsRef<TagRef>,
    {
        if let Some(parent) = file.as_path().parent() {
            let dir_tag = parent.join(tag.as_ref().as_path());
            let to_path = dir_tag.join(
                file.as_path()
                    .file_name()
                    .expect("file with parent should have file name"),
            );

            if self.fs.is_dir(&dir_tag) {
                self.apply(MoveOp {
                    to: to_path,
                    from: file.into(),
                })?;
                return Ok(());
            }

            if let Some(other_file) = (|| -> std::io::Result<_> {
                for other_file in self.sane_read_dir(parent)? {
                    if let Ok(other_file) = TaggedFile::from_path(other_file?) {
                        if other_file.tags().any(|x| x == tag.as_ref()) {
                            return Ok(Some(other_file));
                        }
                    }
                }
                Ok(None)
            })()? {
                self.apply_all(from_move_ops(vec![
                    other_file
                        .uninline_tag(tag)
                        .expect("other file should have tag inline"),
                    MoveOp {
                        to: to_path,
                        from: file.into(),
                    },
                ]))?;
                return Ok(());
            }
        }

        self.apply(file.add_inline_tag(tag)?)?;
        Ok(())
    }

    pub fn del_tag<T>(&self, tag: T, file: TaggedFile) -> Result<(), DelError<T>>
    where
        T: AsRef<TagRef>,
    {
        self.apply_all(from_move_ops(vec![file.del_tag(tag)?]))?;
        Ok(())
    }

    pub fn organize(&self) -> std::io::Result<()> {
        let files = self.find_tagged_files("".into())?;
        let ops = _organize(files, String::new(), Vec::new(), 0);
        self.apply_all(from_move_ops(
            ops.filter(|MoveOp { from, to }| from != to).collect(),
        ))?;
        Ok(())
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

#[auto_enum]
fn _organize(
    files: Vec<TaggedFile>,
    prefix: String,
    tags: Vec<Tag>,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> {
    #[auto_enum(Iterator)]
    if let Some((tag, count)) = files
        .iter()
        .flat_map(|file| file.tags())
        .counts()
        .into_iter()
        .filter(|(tag, _)| !tags.iter().map(|x| x.as_ref()).contains(tag))
        .max_by(|(tag, count), (other_tag, other_count)| {
            count
                .cmp(other_count)
                .then_with(|| tag.len().cmp(&other_tag.len()))
                .then_with(|| tag.cmp(other_tag).reverse())
        })
    {
        #[auto_enum(Iterator)]
        if count == 1 {
            files.into_iter().map(move |file| MoveOp {
                to: format!(
                    "{}{}{TAG_END}{}",
                    FmtPrefix(&prefix),
                    file.tags()
                        .filter(|tag| !tags.iter().map(|x| x.as_ref()).contains(tag))
                        .format_with("", |tag, f| {
                            f(&tag)?;
                            f(&INLINE_SEPARATOR)?;
                            Ok(())
                        }),
                    file.name()
                )
                .into(),
                from: file.into(),
            })
        } else if count == parent_count {
            let tag = tag.to_owned();
            Box::new(_organize(
                files,
                format!("{}{tag}", FmtPrefixInline(&prefix)),
                tags.into_iter().chain(once(tag)).collect(),
                count,
            )) as Box<dyn Iterator<Item = _>>
        } else {
            let tag = tag.to_owned();
            let (with_tag, without_tag) = files
                .into_iter()
                .partition::<Vec<_>, _>(|file| file.tags().contains(&tag.as_ref()));
            Box::new(
                _organize(
                    with_tag,
                    format!("{}{tag}", FmtPrefix(&prefix)),
                    tags.iter().cloned().chain(once(tag)).collect(),
                    count,
                )
                .chain(_organize(without_tag, prefix, tags, parent_count)),
            ) as Box<dyn Iterator<Item = _>>
        }
    } else {
        files.into_iter().map(move |file| MoveOp {
            to: format!("{}{TAG_END}{}", FmtPrefix(&prefix), file.name()).into(),
            from: file.into(),
        })
    }
}

struct FmtPrefix<'a>(&'a str);

impl fmt::Display for FmtPrefix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            self.0.fmt(f)?;
            DIR_SEPARATOR.fmt(f)?;
        }
        Ok(())
    }
}

struct FmtPrefixInline<'a>(&'a str);

impl fmt::Display for FmtPrefixInline<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            self.0.fmt(f)?;
            INLINE_SEPARATOR.fmt(f)?;
        }
        Ok(())
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
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::{
        testing::{make_file_and_parent, TagSetTaggedFile},
        Tag,
    };

    use super::*;

    #[test]
    fn add_tag_renames_file_if_file_does_not_have_tag() {
        test_add_tag(|_| {}, "foo-_bar", "baz", "foo-baz-_bar", |_| {});
        test_add_tag(|_| {}, "foo/_bar", "baz", "foo/baz-_bar", |_| {});
        test_add_tag(|_| {}, "🙂/_bar", "🙁", "🙂/🙁-_bar", |_| {});
    }

    #[test]
    fn add_tag_uses_directory_if_it_exists() {
        test_add_tag(
            |fs| fs.create_dir("foo").unwrap(),
            "_bar",
            "foo",
            "foo/_bar",
            |_| {},
        );
        test_add_tag(
            |fs| fs.create_dir("baz").unwrap(),
            "foo-_bar",
            "baz",
            "baz/foo-_bar",
            |_| {},
        );
        test_add_tag(
            |fs| fs.create_dir_all("a/foo").unwrap(),
            "a/_bar",
            "foo",
            "a/foo/_bar",
            |_| {},
        );
        test_add_tag(
            |fs| fs.create_dir_all("a/🙂").unwrap(),
            "a/_bar",
            "🙂",
            "a/🙂/_bar",
            |_| {},
        );
    }

    #[test]
    fn add_tag_does_not_use_directory_if_not_parent_and_file_has_other_tags() {
        let dir = "a/foo";
        test_add_tag(
            |fs| fs.create_dir_all(dir).unwrap(),
            "a/b/_bar",
            "foo",
            "a/b/foo-_bar",
            |fs| assert!(fs.is_dir(dir)),
        );
    }

    #[test]
    fn add_tag_uninlines_existing_file_with_tag() {
        let existing_file = "foo-_bar";
        test_add_tag(
            |fs| fs.create_file(existing_file, "").unwrap(),
            "_baz",
            "foo",
            "foo/_baz",
            |fs| {
                assert!(fs.is_file("foo/_bar"));
                assert!(!fs.is_file(existing_file));
            },
        );
    }

    // `add_tag` has several more conditions
    // we could check for
    // and implement:
    //
    // ```
    // #[test]
    // fn add_tag_uses_non_parent_directory_if_it_exists_and_uninline_tag_matches() {
    //     test_add_tag(
    //         |fs| {
    //             fs.create_dir_all("foo/a").unwrap();
    //             fs.create_file("a/_baz", "").unwrap();
    //         },
    //         "a/_bar",
    //         "foo",
    //         "foo/a/_bar",
    //         |fs| {
    //             assert!(fs.is_dir("a"));
    //             assert!(fs.is_file("a/_baz"));
    //         },
    //     );
    //     test_add_tag(
    //         |fs| {
    //             fs.create_dir_all("a/foo/b").unwrap();
    //             fs.create_file("a/b/_baz", "").unwrap();
    //         },
    //         "a/b/_bar",
    //         "foo",
    //         "a/foo/b/_bar",
    //         |fs| {
    //             assert!(fs.is_dir("a/b"));
    //             assert!(fs.is_file("a/b/_baz"));
    //         },
    //     );
    // }
    //
    // #[test]
    // fn add_tag_uses_non_parent_directory_if_it_exists_and_uninline_tag_matches_and_deletes_empty_dir(
    // ) {
    //     test_add_tag(
    //         |fs| fs.create_dir_all("foo/a").unwrap(),
    //         "a/_bar",
    //         "foo",
    //         "foo/a/_bar",
    //         |fs| assert!(!fs.is_dir("a")),
    //     );
    //     test_add_tag(
    //         |fs| fs.create_dir_all("a/foo/b").unwrap(),
    //         "a/b/_bar",
    //         "foo",
    //         "a/foo/b/_bar",
    //         |fs| assert!(!fs.is_dir("a/b")),
    //     );
    // }
    //
    // #[test]
    // fn add_tag_uses_non_parent_directory_if_it_exists_and_uninlines_tag() {
    //     test_add_tag(
    //         |fs| fs.create_dir_all("foo/a").unwrap(),
    //         "a-_bar",
    //         "foo",
    //         "foo/a/_bar",
    //         |_| {},
    //     );
    // }
    //
    // #[test]
    // fn add_tag_uninlines_existing_parent_dir_with_tag() {
    //     let existing_dir = "foo-bar";
    //     let existing_file = "foo-bar/_baz";
    //     test_add_tag(
    //         |fs| {
    //             fs.create_dir(existing_dir).unwrap();
    //             fs.create_file(existing_file, "").unwrap();
    //         },
    //         "_baz",
    //         "foo",
    //         "foo/_baz",
    //         |fs| {
    //             assert!(fs.is_dir("foo/bar"));
    //             assert!(!fs.is_dir(existing_dir));
    //             assert!(fs.is_file("foo/bar/_baz"));
    //             assert!(!fs.is_file(existing_file));
    //         },
    //     );
    // }
    // ```

    fn test_add_tag<F, FP>(pre: F, file: &str, tag: &str, expected: &str, post: FP)
    where
        F: FnOnce(&FakeFileSystem),
        FP: FnOnce(&FakeFileSystem),
    {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        let file = TaggedFile::new(file.to_owned()).unwrap();
        let tag = Tag::new(tag.to_owned()).unwrap();

        pre(&filesystem.fs);

        make_file_and_parent(&filesystem.fs, &file);

        assert!(filesystem.add_tag(&tag, file.clone()).is_ok());

        assert!(filesystem.fs.is_file(expected));
        assert!(!filesystem.fs.is_file(file));

        post(&filesystem.fs);
    }

    #[test]
    fn add_tag_returns_error_if_file_already_has_tag() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());

        let tag = Tag::new("foo".to_owned()).unwrap();
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();

        filesystem.fs.create_file(&file, "").unwrap();

        assert!(filesystem.add_tag(&tag, file).is_err());
    }

    #[test]
    fn del_tag_moves_file_if_file_has_tag() {
        test_del_tag("foo-_bar", "foo", "_bar");
        test_del_tag("foo/_bar", "foo", "_bar");
        test_del_tag("🙂/foo-_bar", "foo", "🙂/_bar");
    }

    fn test_del_tag(file: &str, tag: &str, expected: &str) {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        let file = TaggedFile::new(file.to_owned()).unwrap();
        let tag = Tag::new(tag.to_owned()).unwrap();

        make_file_and_parent(&filesystem.fs, &file);

        assert!(filesystem.del_tag(&tag, file.clone()).is_ok());

        assert!(filesystem.fs.is_file(expected));
        assert!(!filesystem.fs.is_file(file));
    }

    #[test]
    fn del_tag_returns_error_if_file_lacks_tag() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());

        let tag = Tag::new("foo".to_owned()).unwrap();
        let file = TaggedFile::new("_bar".to_owned()).unwrap();

        filesystem.fs.create_file(&file, "").unwrap();

        assert!(filesystem.del_tag(&tag, file).is_err());
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
            ["_fiz", "a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙂-🙁-_fez"].map(PathBuf::from),
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
        for path in ["a/bb/_foo", "bb/_bar", "a/_baz"] {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-_baz", "bb/_bar", "bb/a-_foo"].map(PathBuf::from),
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

    #[proptest(cases = 20, failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn organize_is_idempotent(filesystem: TaggedFilesystem<FakeFileSystem>) {
        filesystem.organize().unwrap();
        let first_pass_files = list_files(&filesystem.fs);
        filesystem.organize().unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), first_pass_files);
    }

    #[proptest(cases = 20, failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
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

    #[proptest(cases = 20, failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn organize_results_in_unique_tags_in_directories(
        filesystem: TaggedFilesystem<FakeFileSystem>,
    ) {
        filesystem.organize().unwrap();

        for dir in list_dirs(&filesystem.fs) {
            for (x, y) in filesystem
                .fs
                .read_dir(dir)
                .unwrap()
                .filter_map(|x| x.unwrap().path().file_name().map(|x| x.to_os_string()))
                .map(|name| {
                    // Tags within a dir will not be split by `/`.
                    let name = name.into_string().unwrap();
                    match TaggedFile::new(name) {
                        Ok(file) => file.tags().map(|x| x.to_owned()).collect_vec(),
                        Err(e) => e
                            .into_string()
                            .split('-')
                            .map(|x| Tag::new(x.to_owned()).unwrap())
                            .collect_vec(),
                    }
                })
                .collect_vec()
                .into_iter()
                .tuple_combinations()
            {
                for tag in &x {
                    for other in &y {
                        prop_assert_ne!(tag, other);
                    }
                }
            }
        }
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
