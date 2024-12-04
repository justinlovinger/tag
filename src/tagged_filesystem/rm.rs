use super::*;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum RmError {
    NoFile(#[from] NoFileError),
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    pub fn rm(&self, name: Name) -> Result<(), RmError> {
        let file_path = self.root.file(&name);

        if !file_path.try_exists()? {
            return Err(NoFileError.into());
        }
        if file_path.is_file() {
            remove_file(file_path)?;
        } else {
            remove_dir_all(file_path)?
        }

        remove_dir_all(self.root.file_tags(&name))?;

        let tagged_path = self
            .filtered_tagged_paths(move |path| path.name() == name.as_ref())
            .next()
            .expect("a tagged path for the file should exist");
        let tags = tagged_path.tags().map(|tag| tag.to_owned()).collect();
        remove_file(self.root.join(tagged_path))?;
        self.apply_all(from_move_ops(organize(&relevant_paths(
            tags,
            self.tagged_paths().collect(),
        ))))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tagged_filesystem, tagged_filesystem_with, with_temp_dir},
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
