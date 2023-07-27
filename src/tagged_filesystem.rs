use std::{collections::BTreeSet, fmt, iter::once, path::PathBuf};

use auto_enums::auto_enum;
use filesystem::{DirEntry, FileSystem};
use itertools::Itertools;

use crate::{
    tagged_file::{HasTagError, LacksTagError, MoveOp, Op},
    Tag, TagRef, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END,
};

#[derive(Debug)]
pub struct TaggedFilesystem<F> {
    fs: F,
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

impl<F> TaggedFilesystem<F>
where
    F: FileSystem + 'static,
{
    pub fn new(fs: F) -> Self {
        Self { fs }
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

            for other_file in self.fs.read_dir(parent)? {
                if let Ok(other_file) = TaggedFile::from_path(other_file?.path()) {
                    if other_file.tags().any(|x| x == tag.as_ref()) {
                        self.apply_all(
                            other_file
                                .uninline_tag(tag)
                                .expect("other file should have tag inline")
                                .chain(once(Op::Move(MoveOp {
                                    to: to_path,
                                    from: file.into(),
                                }))),
                        )?;
                        return Ok(());
                    }
                }
            }
        }

        self.apply_all(file.add_inline_tag(tag)?)?;
        Ok(())
    }

    pub fn del_tag<T>(&self, tag: T, file: TaggedFile) -> Result<(), DelError<T>>
    where
        T: AsRef<TagRef>,
    {
        self.apply_all(file.del_tag(tag)?)?;
        Ok(())
    }

    pub fn organize(&self) -> std::io::Result<()> {
        let files = self.find_tagged_files("".into())?;
        let ops = _organize(files, String::new(), Vec::new());
        self.apply_all(from_move_ops(
            ops.filter(|MoveOp { from, to }| from != to).collect(),
        ))?;
        Ok(())
    }

    fn find_tagged_files(&self, root: PathBuf) -> std::io::Result<Vec<TaggedFile>> {
        let mut dirs = vec![root];
        let mut files = Vec::new();
        while let Some(dir) = dirs.pop() {
            for file in self.fs.read_dir(dir)? {
                match TaggedFile::from_path(file?.path()) {
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
            Op::EnsureDirectory(path) => self.fs.create_dir_all(path),
            Op::Move(MoveOp { from, to }) => {
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
                } else {
                    self.fs.rename(from, to)
                }
            }
            Op::DeleteDirectoryIfEmpty(path) => {
                if self.fs.read_dir(&path)?.next().is_none() {
                    self.fs.remove_dir(path)
                } else {
                    Ok(())
                }

                // Note,
                // we can do the following with nightly Rust,
                // which may be more efficient:
                // ```
                // if let Err(e) = self.fs.remove_dir(path) {
                //     if e.kind() != std::io::ErrorKind::DirectoryNotEmpty {
                //         return Err(e);
                //     }
                // };
                // Ok(())
                // ```
            }
        }
    }
}

#[auto_enum]
fn _organize(
    files: Vec<TaggedFile>,
    prefix: String,
    tags: Vec<Tag>,
) -> impl Iterator<Item = MoveOp> {
    #[auto_enum(Iterator)]
    if let Some((tag, count)) = files
        .iter()
        .flat_map(|file| file.tags())
        .counts()
        .into_iter()
        .filter(|(k, _)| !tags.iter().map(|x| x.as_ref()).contains(k))
        .max_by(|(xk, xv), (yk, yv)| {
            xv.cmp(yv)
                .then_with(|| xk.len().cmp(&yk.len()))
                .then_with(|| xk.cmp(yk).reverse())
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
        } else {
            let tag = tag.to_owned();
            let (with_tag, without_tag) = files
                .into_iter()
                .partition::<Vec<_>, _>(|file| file.tags().contains(&tag.as_ref()));
            if without_tag.is_empty() {
                // Note,
                // we could also check if count equals parent count,
                // to avoid calling `partition`,
                // but that would require tracking parent count.
                Box::new(_organize(
                    with_tag,
                    format!("{}{tag}", FmtPrefixInline(&prefix)),
                    tags.iter().cloned().chain(once(tag)).collect(),
                )) as Box<dyn Iterator<Item = _>>
            } else {
                Box::new(
                    _organize(
                        with_tag,
                        format!("{}{tag}", FmtPrefix(&prefix)),
                        tags.iter().cloned().chain(once(tag)).collect(),
                    )
                    .chain(_organize(without_tag, prefix, tags)),
                ) as Box<dyn Iterator<Item = _>>
            }
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
    use std::{collections::BTreeSet, path::Path};

    use filesystem::FakeFileSystem;
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::Tag;

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

    #[proptest(cases = 10, failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn organize_is_idempotent(files: Vec<TaggedFile>) {
        prop_assume!(files.iter().map(TagSetTaggedFile::new).all_unique());

        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for file in files.iter() {
            make_file_and_parent(&filesystem.fs, file.as_path());
        }

        filesystem.organize().unwrap();
        let first_pass_files = list_files(&filesystem.fs);
        filesystem.organize().unwrap();
        prop_assert_eq!(list_files(&filesystem.fs), first_pass_files);
    }

    #[proptest(cases = 10, failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn organize_does_not_change_tags_or_names(files: Vec<TaggedFile>) {
        prop_assume!(files.iter().map(TagSetTaggedFile::new).all_unique());

        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for file in files.iter() {
            make_file_and_parent(&filesystem.fs, file.as_path());
        }

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

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct TagSetTaggedFile<'a> {
        tags: BTreeSet<&'a TagRef>,
        name: &'a str,
    }

    impl<'a> TagSetTaggedFile<'a> {
        pub fn new(file: &'a TaggedFile) -> Self {
            Self {
                tags: file.tags().collect(),
                name: file.name(),
            }
        }
    }

    fn make_file_and_parent<P>(fs: &FakeFileSystem, path: P)
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            fs.create_dir_all(parent).unwrap();
        }
        fs.create_file(path, "").unwrap();
    }

    fn list_files(fs: &FakeFileSystem) -> Vec<PathBuf> {
        let mut dirs = vec![PathBuf::from("")];
        let mut files = Vec::new();
        while let Some(dir) = dirs.pop() {
            if fs.read_dir(&dir).unwrap().next().is_none() && !dir.as_os_str().is_empty() {
                files.push(dir);
            } else {
                for file in fs.read_dir(dir).unwrap() {
                    let file = file.unwrap().path();
                    if fs.is_dir(&file) {
                        dirs.push(file);
                    } else {
                        files.push(file);
                    }
                }
            }
        }
        files.sort();
        files
    }
}
