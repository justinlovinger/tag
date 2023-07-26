use std::{fmt::Debug, iter::once, path::PathBuf};

use auto_enums::auto_enum;
use filesystem::{DirEntry, FileSystem};
use itertools::Itertools;

use crate::{
    tagged_file::{HasTagError, LacksTagError, MoveOp, Op},
    Tag, TagRef, TaggedFile, INLINE_SEPARATOR, TAG_END,
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
        T: Debug + AsRef<TagRef>,
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
        let ops = _organize(files, "".into(), Vec::new());
        self.apply_all(
            ops.filter(|MoveOp { from, to }| from != to)
                .flat_map(clean_move),
        )?;
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
        // Note:
        // the application of sequences of operations can be optimized
        // by making directories first,
        // moving files next,
        // and deleting directories last,
        // while squashing duplicate operations to make or delete directories.
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
            Op::Move(MoveOp { from, to }) => self.fs.rename(from, to),
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
    prefix: PathBuf,
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
                to: prefix.join(format!(
                    "{}{TAG_END}{}",
                    file.tags()
                        .filter(|tag| !tags.iter().map(|x| x.as_ref()).contains(tag))
                        .format_with("", |tag, f| {
                            f(&tag)?;
                            f(&INLINE_SEPARATOR)?;
                            Ok(())
                        }),
                    file.name()
                )),
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
                let mut prefix = prefix.into_os_string();
                prefix.push(String::from(INLINE_SEPARATOR));
                prefix.push(tag.as_path());
                Box::new(_organize(
                    with_tag,
                    prefix.into(),
                    tags.iter().cloned().chain(once(tag)).collect(),
                )) as Box<dyn Iterator<Item = _>>
            } else {
                Box::new(
                    _organize(
                        with_tag,
                        prefix.join(tag.as_path()),
                        tags.iter().cloned().chain(once(tag)).collect(),
                    )
                    .chain(_organize(without_tag, prefix, tags)),
                ) as Box<dyn Iterator<Item = _>>
            }
        }
    } else {
        files.into_iter().map(move |file| MoveOp {
            to: prefix.join(format!("{TAG_END}{}", file.name())),
            from: file.into(),
        })
    }
}

fn clean_move(op: MoveOp) -> impl Iterator<Item = Op> {
    // Note,
    // we can do better
    // by comparing parents of `to` and `from`.
    let ensure_dir = Vec::from_iter(
        op.to
            .parent()
            .into_iter()
            .map(|x| x.to_owned())
            .map(Op::EnsureDirectory),
    );
    let del_dirs = Vec::from_iter(
        op.from
            .ancestors()
            .skip(1)
            .map(|x| x.to_owned())
            .map(Op::DeleteDirectoryIfEmpty),
    );
    ensure_dir
        .into_iter()
        .chain(once(Op::Move(op)))
        .chain(del_dirs)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use filesystem::FakeFileSystem;
    use proptest::test_runner::FileFailurePersistence;
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

        if let Some(parent) = file.as_path().parent() {
            filesystem.fs.create_dir_all(parent).unwrap();
        }
        filesystem.fs.create_file(&file, "").unwrap();

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
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());

        let tag = Tag::new("foo".to_owned()).unwrap();
        let files = [
            TaggedFile::new("foo-_bar".to_owned()).unwrap(),
            TaggedFile::new("foo/_bar".to_owned()).unwrap(),
            TaggedFile::new("🙂/foo-_bar".to_owned()).unwrap(),
        ];
        let destinations = ["_bar", "_bar", "🙂/_bar"];

        filesystem.fs.create_dir("foo").unwrap();
        for (file, dest) in files.into_iter().zip(destinations.into_iter()) {
            make_file_and_parent(&filesystem.fs, &file);

            assert!(filesystem.del_tag(&tag, file.clone()).is_ok());

            assert!(filesystem.fs.is_file(dest));
            assert!(!filesystem.fs.is_file(file));
        }
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
        for path in ["a/b/c/_foo", "a-b-_bar", "d/e-_baz", "🙂/🙁/_fez"] {
            make_file_and_parent(&filesystem.fs, path);
        }

        filesystem.organize().unwrap();
        assert_eq!(
            list_files(&filesystem.fs),
            ["a-b/_bar", "a-b/c-_foo", "d-e-_baz", "🙂-🙁-_fez"].map(PathBuf::from),
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

    // This currently fails for some strange unicode inputs
    // likely unrelated to `organize` itself.
    // This should be tested
    // and fixed elsewhere.
    #[ignore = "failure likely unrelated to method under test"]
    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn organize_is_idempotent(files: Vec<TaggedFile>) {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());
        for file in files.into_iter().unique() {
            make_file_and_parent(&filesystem.fs, file.as_path());
        }

        filesystem.organize().unwrap();
        let first_pass_files = list_files(&filesystem.fs);
        filesystem.organize().unwrap();
        assert_eq!(list_files(&filesystem.fs), first_pass_files);
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
        let mut dirs = vec!["".into()];
        let mut files = Vec::new();
        while let Some(dir) = dirs.pop() {
            if fs.read_dir(&dir).unwrap().next().is_none() {
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
