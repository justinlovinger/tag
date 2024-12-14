use super::{build::BuildError, *};

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum NewFileError {
    FileExists(#[from] FileExistsError),
    MetadataExists(#[from] MetadataExistsError),
    Build(#[from] BuildError),
    Filesystem(#[from] std::io::Error),
}

enum NewFileType {
    File,
    Dir,
}

impl TaggedFilesystem {
    pub fn touch(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<(), NewFileError> {
        self.new_file(NewFileType::File, tags, name)
    }

    pub fn mkdir(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<(), NewFileError> {
        self.new_file(NewFileType::Dir, tags, name)
    }

    fn new_file(
        &self,
        ty: NewFileType,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<(), NewFileError> {
        let file_path = self.root.file(&name);
        if file_path.try_exists()? {
            return Err(FileExistsError.into());
        }

        let file_tags_path = self.root.file_tags(&name);
        if file_tags_path.try_exists()? {
            return Err(MetadataExistsError(file_tags_path).into());
        }

        create_dir(&file_tags_path)?;
        let program_tags_path = file_tags_path.join(PROGRAM_TAGS_DIR);
        create_dir(&program_tags_path)?;
        for tag in tags {
            File::create(program_tags_path.join(tag.as_path()))?;
        }

        match ty {
            NewFileType::File => {
                File::create(&file_path)?;
            }
            NewFileType::Dir => create_dir(&file_path)?,
        }

        self.build_some(vec![name])?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env::{current_dir, set_current_dir},
        fs::{read_link, File},
        io::Read,
    };

    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{
            name, tag, tagged_filesystem, tagged_filesystem_with, with_temp_cwd, with_temp_dir,
            TaggedPaths,
        },
    };

    use super::*;

    #[test]
    fn touch_creates_an_empty_file_with_metadata_and_a_link_if_name_is_unique() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            filesystem.touch([tag("foo")], name("bar")).unwrap();
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
            assert_eq!(
                File::open(filesystem.root.file(name("bar")))
                    .unwrap()
                    .bytes()
                    .map(|x| x.unwrap())
                    .collect_vec(),
                "".as_bytes()
            );
            assert_eq!(
                read_link(filesystem.root.join("foo-_bar")).unwrap(),
                filesystem.root.file(name("bar"))
            );
        })
    }

    #[test]
    fn touch_works_in_subdir_of_root() {
        with_temp_cwd(|| {
            let _ = tagged_filesystem_with(current_dir().unwrap(), ["foo-_bar", "foo-_baz"]);

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            filesystem.touch([tag("foo")], name("biz")).unwrap();
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/files/biz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    ".tag/tags/biz/tag/foo",
                    "foo/_bar",
                    "foo/_baz",
                    "foo/_biz",
                ]
                .map(PathBuf::from)
            );
            assert_eq!(
                File::open(filesystem.root.file(name("biz")))
                    .unwrap()
                    .bytes()
                    .map(|x| x.unwrap())
                    .collect_vec(),
                "".as_bytes()
            );
            assert_eq!(
                read_link(filesystem.root.join("foo/_biz")).unwrap(),
                filesystem.root.file(name("biz"))
            );
        })
    }

    #[proptest(cases = 20)]
    fn touch_builds(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);

            filesystem
                .touch(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let expected = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn touch_errors_if_file_with_name_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo-_bar"]);
            assert!(filesystem.touch([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_dir_with_name_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo-_bar"]);
            assert!(filesystem.touch([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_metadata_exists_for_file() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            create_dir(filesystem.root.file_tags(name("bar"))).unwrap();
            assert!(filesystem.touch([], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_creates_an_empty_directory_with_metadata_and_a_link_if_name_is_unique() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);

            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
            assert!(filesystem.root.file(name("bar")).is_dir());
            assert!(std::fs::read_dir(filesystem.root.file(name("bar")))
                .unwrap()
                .next()
                .is_none());
            assert_eq!(
                read_link(filesystem.root.join("foo-_bar")).unwrap(),
                filesystem.root.file(name("bar"))
            );
        })
    }

    #[test]
    fn mkdir_works_in_subdir_of_root() {
        with_temp_cwd(|| {
            let filesystem = tagged_filesystem(current_dir().unwrap());
            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            filesystem.mkdir([tag("foo")], name("baz")).unwrap();

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            filesystem.mkdir([tag("foo")], name("biz")).unwrap();
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/files/biz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    ".tag/tags/biz/tag/foo",
                    "foo/_bar",
                    "foo/_baz",
                    "foo/_biz",
                ]
                .map(PathBuf::from)
            );
            assert!(filesystem.root.file(name("biz")).is_dir());
            assert!(std::fs::read_dir(filesystem.root.file(name("biz")))
                .unwrap()
                .next()
                .is_none());
            assert_eq!(
                read_link(filesystem.root.join("foo/_biz")).unwrap(),
                filesystem.root.file(name("biz"))
            );
        })
    }

    #[proptest(cases = 20)]
    fn mkdir_builds(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths);

            filesystem
                .mkdir(
                    path.tags().map(|tag| tag.to_owned()),
                    path.name().to_owned(),
                )
                .unwrap();
            let actual = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let expected = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn mkdir_errors_if_file_with_name_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo-_bar"]);
            assert!(filesystem.mkdir([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_dir_with_name_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            assert!(filesystem.mkdir([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_metadata_exists_for_file() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            create_dir(filesystem.root.file_tags(name("bar"))).unwrap();
            assert!(filesystem.mkdir([], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }
}
