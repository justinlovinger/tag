use super::{build::BuildError, *};

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RmError {
    Build(#[from] BuildError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn rm(&self, name: Name) -> Result<(), RmError> {
        let file_path = self.root.file(&name);
        if file_path.is_dir() {
            remove_dir_all(file_path)?;
        } else {
            remove_file(file_path)?;
        }

        self.build_some(vec![name])?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tagged_filesystem, tagged_filesystem_with, with_temp_dir, TaggedPaths},
    };

    use super::*;

    #[test]
    fn rm_removes_file_if_it_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, ["_foo"]);
            assert!(filesystem.rm(name("foo")).is_ok());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }

    #[test]
    fn rm_removes_dir_if_it_exists() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            filesystem.mkdir([], name("foo")).unwrap();

            assert!(filesystem.rm(name("foo")).is_ok());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }

    #[proptest(cases = 20)]
    fn rm_builds(paths: TaggedPaths, path: TaggedPath) {
        let (actual, expected) = with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(dir, paths.iter().chain([&path]));

            filesystem.rm(path.name().to_owned()).unwrap();
            let actual = list_files(&filesystem.root);

            filesystem.build().unwrap();
            let expected = list_files(filesystem.root);

            (actual, expected)
        });

        prop_assert_eq!(actual, expected)
    }

    #[test]
    fn rm_errors_on_missing_file() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem(dir);
            assert!(filesystem.rm(name("foo")).is_err());
            assert_eq!(
                list_files(&filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        });
    }
}
