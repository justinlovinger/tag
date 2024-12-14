use super::{build::BuildError, *};

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RenameError {
    NoFile(#[from] NoFileError),
    FileExists(#[from] FileExistsError),
    NoMetadataExists(#[from] NoMetadataExistsError),
    MetadataExists(#[from] MetadataExistsError),
    Build(#[from] BuildError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn rename(&self, old_name: Name, new_name: Name) -> Result<(), RenameError> {
        if old_name == new_name {
            return Ok(());
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

        self.build_some(vec![old_name, new_name])?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read_link;

    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tagged_filesystem, tagged_filesystem_with, with_temp_dir, TaggedPaths},
    };

    use super::*;

    #[test]
    fn rename_renames_file_and_tagged_path_if_file_exists_and_no_file_with_new_name() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo-_bar"]);

            filesystem.rename(name("bar"), name("baz")).unwrap();
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
    fn rename_does_nothing_if_names_are_same(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain([&path]));

            let expected = list_files(&filesystem.root);

            filesystem
                .rename(path.name().to_owned(), path.name().to_owned())
                .unwrap();
            let actual = list_files(&filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn rename_errors_if_file_does_not_exist() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            assert!(filesystem.rename(name("bar"), name("baz")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn rename_errors_if_file_exists_with_new_name() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["fiz-_bar", "foo-_baz"]);
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
