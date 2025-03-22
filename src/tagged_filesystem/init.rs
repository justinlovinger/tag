use std::{fs::File, io::Write};

use crate::fs::set_executable;

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

    pub fn init<P>(dir: P) -> Result<Self, InitError>
    where
        P: AsRef<Path>,
    {
        let dir = dir.as_ref();
        if dir.join(METADATA_DIR).is_dir() {
            Err(InitError::AlreadyTagged)
        } else if dir.join(METADATA_DIR).is_file() {
            Err(InitError::FileExists)
        } else if let Some(root) = Root::from_child(dir)? {
            Err(InitError::InTagged(root.into_path()))
        } else if std::fs::read_dir(dir)?.next().is_some() {
            Err(InitError::NotEmpty)
        } else {
            create_dir(dir.join(METADATA_DIR))?;

            create_dir(dir.join(METADATA_DIR).join(FILES_DIR))?;

            let mut tags_script = File::create(dir.join(METADATA_DIR).join(TAGS_SCRIPT))?;
            tags_script.write_all(indoc::indoc! {br#"
                #!/bin/sh

                for tag in .tag/tags/"$1"/*/*; do
                    [ -e "$tag" ] && echo "${tag##*/}"
                done
            "#})?;
            set_executable(&tags_script)?;

            Ok(TaggedFilesystemBuilder::new(dir.to_owned())
                .build()?
                .unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use crate::{
        tagged_filesystem::testing::{list_files, tagged_filesystem_with},
        testing::{name, tag, with_temp_dir},
    };

    use super::*;

    #[test]
    fn init_initializes_an_empty_directory() {
        with_temp_dir(|dir| {
            let filesystem = TaggedFilesystem::init(dir).unwrap();
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags.sh"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_files_dir() {
        with_temp_dir(|dir| {
            let filesystem = TaggedFilesystem::init(dir).unwrap();
            filesystem.mkdir([tag("foo")], name("bar"));
            assert!(TaggedFilesystem::init(filesystem.root.file(name("bar"))).is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags.sh",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags.sh",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags.sh",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_initializes_a_nested_tagged_filesystem_from_tagged_path() {
        with_temp_dir(|dir| {
            let filesystem = TaggedFilesystem::init(dir).unwrap();
            filesystem.mkdir([tag("foo")], name("bar"));
            assert!(TaggedFilesystem::init(filesystem.root.join("foo-_bar")).is_ok());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar/.tag/files",
                    ".tag/files/bar/.tag/tags.sh",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags.sh",
                    "foo-_bar/.tag/files",
                    "foo-_bar/.tag/tags.sh",
                ]
                .map(PathBuf::from)
            );
        });
    }

    #[test]
    fn init_errors_on_already_initialized_directory() {
        with_temp_dir(|dir| {
            let filesystem = TaggedFilesystem::init(dir).unwrap();
            assert!(TaggedFilesystem::init(dir).is_err());
            assert_eq!(
                list_files(filesystem.root),
                [".tag/files", ".tag/tags.sh"].map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_dot_tag_file() {
        with_temp_dir(|dir| {
            File::create(dir.join(METADATA_DIR)).unwrap();
            assert!(TaggedFilesystem::init(dir).is_err());
            assert_eq!(list_files(dir), [".tag"].map(PathBuf::from));
        })
    }

    #[test]
    fn init_errors_on_sub_directory_of_tagged_filesystem() {
        with_temp_dir(|dir| {
            let filesystem = tagged_filesystem_with(
                dir,
                [
                    TaggedPath::new("foo/_bar".to_owned()).unwrap(),
                    TaggedPath::new("foo/_baz".to_owned()).unwrap(),
                ],
            );
            assert!(TaggedFilesystem::init(filesystem.root.join("foo")).is_err());
            assert_eq!(
                list_files(filesystem.root),
                [
                    ".tag/files/bar",
                    ".tag/files/baz",
                    ".tag/tags/bar/tag/foo",
                    ".tag/tags/baz/tag/foo",
                    ".tag/tags.sh",
                    "foo/_bar",
                    "foo/_baz"
                ]
                .map(PathBuf::from)
            );
        })
    }

    #[test]
    fn init_errors_on_directory_with_files() {
        with_temp_dir(|dir| {
            File::create(dir.join("foo")).unwrap();
            assert!(TaggedFilesystem::init(dir).is_err());
            assert_eq!(list_files(dir), ["foo"].map(PathBuf::from));
        })
    }
}
