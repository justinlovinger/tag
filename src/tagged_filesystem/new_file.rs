use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum NewFileError {
    FileExists(#[from] FileExistsError),
    MetadataExists(#[from] MetadataExistsError),
    NonUniqueTag(#[from] NonUniqueTagError),
    New(#[from] TaggedPathError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn touch(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<PathBuf, NewFileError> {
        let (tagged_path, file_path, path_to_print) = self.new_file(tags, name)?;
        symlink_file(&file_path, tagged_path)?;
        File::create(&file_path)?;
        Ok(path_to_print)
    }

    pub fn mkdir(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<PathBuf, NewFileError> {
        let (tagged_path, file_path, path_to_print) = self.new_file(tags, name)?;
        crate::fs::symlink_dir(&file_path, tagged_path)?;
        create_dir(&file_path)?;
        Ok(path_to_print)
    }

    /// Return (tagged_path, file_path, path_to_print)
    fn new_file(
        &self,
        tags: impl IntoIterator<Item = Tag>,
        name: Name,
    ) -> Result<(PathBuf, PathBuf, PathBuf), NewFileError> {
        let file_path = self.root.file(&name);
        if file_path.try_exists()? {
            return Err(FileExistsError.into());
        }

        let file_tags_path = self.root.file_tags(&name);
        if file_tags_path.try_exists()? {
            return Err(MetadataExistsError(file_tags_path).into());
        }

        let tag_set = unique_tags(tags)?;
        let tagged_path = TaggedPath::from_tags(&tag_set, name);

        create_dir(&file_tags_path)?;
        let tags_path = file_tags_path.join(PROGRAM_TAGS_DIR);
        create_dir(&tags_path)?;
        for tag in tag_set.iter() {
            File::create(tags_path.join(tag.as_path()))?;
        }

        let mut paths = relevant_paths(tag_set, self.tagged_paths().collect());
        let tagged_path_buf = tagged_path.as_path().to_owned();
        paths.push(tagged_path);

        let ops = from_move_ops(organize(&paths));

        let mut to_path = None;
        self.apply_all(ops.filter(|op| match op {
            Op::EnsureDirectory(_) => true,
            Op::Move(MoveOp { from, to }) => {
                if from == &tagged_path_buf {
                    to_path = Some(to.clone());
                    false
                } else {
                    true
                }
            }
            Op::DeleteDirectoryIfEmpty(_) => true,
        }))?;

        let to_path = self.root.join(match to_path {
            Some(to_path) => to_path,
            None => tagged_path_buf,
        });
        let path_to_print = to_path
            .strip_prefix(current_dir()?)
            .map(|path| path.to_owned())
            .unwrap_or_else(|_| to_path.clone());
        Ok((to_path, file_path, path_to_print))
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env::set_current_dir,
        fs::{read_link, File},
        io::Read,
    };

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tag, tagged_filesystem, tagged_filesystem_with, with_tempdir},
    };

    use super::*;

    #[test]
    fn touch_creates_an_empty_file_with_metadata_and_a_link_if_name_is_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert_eq!(
                filesystem.touch([tag("foo")], name("bar")).unwrap(),
                PathBuf::from("foo-_bar")
            );
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
        with_tempdir(|| {
            let _ = tagged_filesystem_with(["foo-_bar", "foo-_baz"]);

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            assert_eq!(
                filesystem.touch([tag("foo")], name("biz")).unwrap(),
                PathBuf::from("_biz")
            );
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

    #[test]
    fn touch_errors_if_file_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem.touch([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_dir_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem.touch([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_metadata_exists_for_file() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            create_dir(filesystem.root.file_tags(name("bar"))).unwrap();
            assert!(filesystem.touch([], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn touch_errors_if_tags_are_not_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .touch([tag("foo"), tag("foo")], name("bar"))
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_creates_an_empty_directory_with_metadata_and_a_link_if_name_is_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert_eq!(
                filesystem.mkdir([tag("foo")], name("bar")).unwrap(),
                PathBuf::from("foo-_bar")
            );
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
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            filesystem.mkdir([tag("foo")], name("baz")).unwrap();

            set_current_dir("foo").unwrap();
            let filesystem = TaggedFilesystemBuilder::new(current_dir().unwrap())
                .build()
                .unwrap()
                .unwrap();

            assert_eq!(
                filesystem.mkdir([tag("foo")], name("biz")).unwrap(),
                PathBuf::from("_biz")
            );
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

    #[test]
    fn mkdir_errors_if_file_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert!(filesystem.mkdir([tag("baz")], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/bar", ".tag/tags/bar/tag/foo", "foo-_bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_dir_with_name_exists() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
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
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            create_dir(PathBuf::from(METADATA_DIR).join(TAGS_DIR).join("bar")).unwrap();
            assert!(filesystem.mkdir([], name("bar")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags/bar"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn mkdir_errors_if_tags_are_not_unique() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem
                .mkdir([tag("foo"), tag("foo")], name("bar"))
                .is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }
}
