use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RenameError {
    #[error("New and old name must be different.")]
    SameName,
    NoFile(#[from] NoFileError),
    FileExists(#[from] FileExistsError),
    NoMetadataExists(#[from] NoMetadataExistsError),
    MetadataExists(#[from] MetadataExistsError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn rename(&self, old_name: Name, new_name: Name) -> Result<PathBuf, RenameError> {
        if old_name == new_name {
            return Err(RenameError::SameName);
        }

        let old_file_path = self.root.file(&old_name);
        if !old_file_path.try_exists()? {
            return Err(NoFileError.into());
        }

        let new_file_path = self.root.file(&new_name);
        if new_file_path.try_exists()? {
            return Err(FileExistsError.into());
        }

        let old_file_tags_path = self.root.file_tags(&old_name);
        if !old_file_tags_path.try_exists()? {
            return Err(NoMetadataExistsError.into());
        }

        let new_file_tags_path = self.root.file_tags(&new_name);
        if new_file_tags_path.try_exists()? {
            return Err(MetadataExistsError(new_file_tags_path).into());
        }

        // Copying ensures no data is lost if the rename fails.
        copy_dir(&old_file_tags_path, new_file_tags_path)?;
        rename(old_file_path, &new_file_path)?;
        std::fs::remove_dir_all(old_file_tags_path)?;

        if let Some(old_tagged_path) = self
            .filtered_tagged_paths(move |path| path.name() == old_name.as_ref())
            .next()
        {
            let new_tagged_path = TaggedPath::from_tags(
                &old_tagged_path.tags().map(|tag| tag.to_owned()).collect(),
                new_name,
            );
            remove_file(old_tagged_path.as_path())?;
            if new_file_path.is_dir() {
                symlink_dir(new_file_path, &new_tagged_path)?;
            } else {
                symlink_file(new_file_path, &new_tagged_path)?;
            }
            Ok(new_tagged_path.into_path())
        } else {
            Ok(PathBuf::new())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read_link;

    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tagged_filesystem, tagged_filesystem_with, with_tempdir, TaggedPaths},
    };

    use super::*;

    #[test]
    fn rename_renames_file_and_tagged_path_if_file_exists_and_no_file_with_new_name() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["foo-_bar"]);
            assert_eq!(
                filesystem.rename(name("bar"), name("baz")).unwrap(),
                PathBuf::from("foo-_baz")
            );
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files/baz", ".tag/tags/baz/tag/foo", "foo-_baz"].map(PathBuf::from)
            );
            assert_eq!(
                read_link(filesystem.root.join("foo-_baz")).unwrap(),
                filesystem.root.file(name("baz"))
            );
        })
    }

    #[proptest]
    fn rename_errors_if_names_are_same(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_tempdir(|| {
            let filesystem = tagged_filesystem_with(paths.iter().chain([&path]));

            let expected = list_files(&filesystem.root);

            assert!(filesystem
                .rename(path.name().to_owned(), path.name().to_owned())
                .is_err());
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn rename_errors_if_file_does_not_exist() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem();
            assert!(filesystem.rename(name("bar"), name("baz")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn rename_errors_if_file_exists_with_new_name() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with(["fiz-_bar", "foo-_baz"]);
            assert!(filesystem.rename(name("bar"), name("baz")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/tags/bar/tag/fiz",
                    ".tag/tags/baz/tag/foo",
                    "fiz-_bar",
                    "foo-_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }
}
