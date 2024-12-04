use crate::TAG_END;

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
            let absolute_new_tagged_path = self.root.join(
                TaggedPath::new(format!("{}{TAG_END}{new_name}", old_tagged_path.tags_str()))
                    .unwrap(),
            );
            remove_file(self.root.join(old_tagged_path))?;
            if new_file_path.is_dir() {
                symlink_dir(new_file_path, &absolute_new_tagged_path)?;
            } else {
                symlink_file(new_file_path, &absolute_new_tagged_path)?;
            }
            Ok(absolute_new_tagged_path
                .strip_prefix(std::env::current_dir().unwrap_or_default())
                .map(|path| path.to_owned())
                .unwrap_or_else(|_| absolute_new_tagged_path.clone()))
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
        testing::{name, tagged_filesystem, tagged_filesystem_with, with_temp_dir, TaggedPaths},
    };

    use super::*;

    #[test]
    fn rename_renames_file_and_tagged_path_if_file_exists_and_no_file_with_new_name() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["foo-_bar"]);
            assert_eq!(
                filesystem.rename(name("bar"), name("baz")).unwrap(),
                filesystem.root.join("foo-_baz")
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
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain([&path]));

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
