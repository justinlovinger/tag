use std::{fmt::Debug, iter::once};

use filesystem::{DirEntry, FileSystem};

use crate::{
    tagged_file::{HasTagError, LacksTagError, Op},
    TagRef, TaggedFile,
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
    F: FileSystem,
{
    pub fn new(fs: F) -> Self {
        Self { fs }
    }

    pub fn add_tag<T>(&self, tag: T, file: TaggedFile) -> Result<(), AddError<T>>
    where
        T: Debug + AsRef<TagRef>,
    {
        if let Some(parent) = file.as_path().parent() {
            let dir_tag = parent.join(tag.as_ref());
            let to_path = dir_tag.join(
                file.as_path()
                    .file_name()
                    .expect("file with parent should have file name"),
            );

            if self.fs.is_dir(&dir_tag) {
                self.apply(Op::Move {
                    to: to_path,
                    from: file.into(),
                })?;
                return Ok(());
            }

            for other_file in self.fs.read_dir(parent)? {
                if let Ok(other_file) = TaggedFile::new(
                    other_file?
                        .path()
                        .into_os_string()
                        .into_string()
                        .expect("path should contain valid unicode"),
                ) {
                    if other_file.tags().any(|x| x == tag.as_ref()) {
                        self.apply_all(
                            other_file
                                .uninline_tag(tag)
                                .expect("other file should have tag inline")
                                .chain(once(Op::Move {
                                    to: to_path,
                                    from: file.into(),
                                })),
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

    fn apply_all(&self, ops: impl Iterator<Item = Op>) -> std::io::Result<()> {
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

    fn apply(&self, op: Op) -> std::io::Result<()> {
        match op {
            Op::EnsureDirectory(path) => self.fs.create_dir_all(path),
            Op::Move { from, to } => self.fs.rename(from, to),
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

#[cfg(test)]
mod tests {
    use filesystem::FakeFileSystem;

    use crate::Tag;

    use super::*;

    #[test]
    fn add_tag_renames_file_if_file_does_not_have_tag() {
        test_add_tag(|_| {}, "foo-_bar", "baz", "foo-baz-_bar", |_| {});
        test_add_tag(|_| {}, "foo/_bar", "baz", "foo/baz-_bar", |_| {});
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
        ];
        let destinations = ["_bar", "_bar"];

        filesystem.fs.create_dir("foo").unwrap();
        for (file, dest) in files.into_iter().zip(destinations.into_iter()) {
            filesystem.fs.create_file(&file, "").unwrap();

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
}
