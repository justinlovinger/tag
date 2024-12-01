use super::*;

#[derive(Debug, thiserror::Error)]
pub enum InitError {
    #[error(
    "The working directory already has a `{}` directory. It may already be the root of a tagging system. If not, please move the directory before initializing.",
    METADATA_DIR
)]
    AlreadyTagged,
    #[error(
        "The working directory has a file named `{}`. Please move it before initializing.",
        METADATA_DIR
    )]
    FileExists,
    #[error(
        "The working directory is part of a tagged directory, `{}`.",
        METADATA_DIR
    )]
    InTagged(PathBuf),
    #[error("The working directory has files. Please move them before initializing.")]
    NotEmpty,
    #[error("{0}")]
    Filesystem(#[from] std::io::Error),
}

impl TaggedFilesystem {
    #[allow(clippy::new_without_default)]

    pub fn init() -> Result<Self, InitError> {
        if PathBuf::from(METADATA_DIR).is_dir() {
            Err(InitError::AlreadyTagged)
        } else if PathBuf::from(METADATA_DIR).is_file() {
            Err(InitError::FileExists)
        } else if let Some(root) = Root::from_child(current_dir()?)? {
            Err(InitError::InTagged(root.into_path()))
        } else if std::fs::read_dir(".")?.next().is_some() {
            Err(InitError::NotEmpty)
        } else {
            create_dir(METADATA_DIR)?;
            create_dir(PathBuf::from(METADATA_DIR).join(FILES_DIR))?;
            create_dir(PathBuf::from(METADATA_DIR).join(TAGS_DIR))?;
            Ok(TaggedFilesystemBuilder::new(current_dir()?)
                .build()?
                .unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{env::set_current_dir, fs::File};

    use crate::{
        tagged_filesystem::tests::list_files,
        testing::{name, tag, tagged_filesystem_with, with_tempdir},
    };

    use super::*;

    #[test]
    fn init_initializes_an_empty_directory() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_files_dir() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            set_current_dir(".tag/files/bar").unwrap();
            assert!(TaggedFilesystem::init().is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags",
                    ".tag/tags/bar/tag/foo",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_tagged_path() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            filesystem.mkdir([tag("foo")], name("bar")).unwrap();
            set_current_dir("foo-_bar").unwrap();
            assert!(TaggedFilesystem::init().is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags",
                    ".tag/tags/bar/tag/foo",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_errors_on_already_initialized_directory() {
        with_tempdir(|| {
            let filesystem = TaggedFilesystem::init().unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_dot_tag_file() {
        with_tempdir(|| {
            File::create(METADATA_DIR).unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(current_dir().unwrap()),
                [".tag"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_sub_directory_of_tagged_filesystem() {
        with_tempdir(|| {
            let filesystem = tagged_filesystem_with([
                TaggedPath::new("foo/_bar".to_owned()).unwrap(),
                TaggedPath::new("foo/_baz".to_owned()).unwrap(),
            ]);
            set_current_dir("foo").unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    "foo/_bar",
                    "foo/_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_files() {
        with_tempdir(|| {
            File::create("foo").unwrap();
            assert!(TaggedFilesystem::init().is_err());
            assert_eq!(
                list_files(current_dir().unwrap()),
                ["foo"].map(PathBuf::from)
            );
        })
    }
}
