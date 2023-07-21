use filesystem::FileSystem;

use crate::{
    tagged_file::{FileAlreadyHasTagError, MoveInstruction},
    TagRef, TaggedFile,
};

#[derive(Debug)]
pub struct TaggedFilesystem<F> {
    fs: F,
}

#[derive(Debug, thiserror::Error)]
pub enum AddError<T> {
    FileAlreadyHasTag(#[from] FileAlreadyHasTagError<T>),
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
        T: AsRef<TagRef>,
    {
        let MoveInstruction { from, to } = file.add_tag(tag)?;
        Ok(self.fs.rename(from, to)?)
    }
}

#[cfg(test)]
mod tests {
    use filesystem::FakeFileSystem;

    use crate::Tag;

    use super::*;

    #[test]
    fn add_tag_moves_file_if_file_does_not_have_tag() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());

        let tag = Tag::new("baz".to_owned()).unwrap();
        let files = [
            TaggedFile::new("foo-_bar".to_owned()).unwrap(),
            TaggedFile::new("foo/_bar".to_owned()).unwrap(),
        ];
        let destinations = ["foo-baz-_bar", "foo/baz-_bar"];

        filesystem.fs.create_dir("foo").unwrap();
        for (file, dest) in files.into_iter().zip(destinations.into_iter()) {
            filesystem.fs.create_file(&file, "").unwrap();

            assert!(filesystem.add_tag(&tag, file.clone()).is_ok());

            assert!(filesystem.fs.is_file(dest));
            assert!(!filesystem.fs.is_file(file));
        }
    }

    #[test]
    fn add_tag_returns_error_if_file_already_has_tag() {
        let filesystem = TaggedFilesystem::new(FakeFileSystem::new());

        let tag = Tag::new("foo".to_owned()).unwrap();
        let file = TaggedFile::new("foo-_bar".to_owned()).unwrap();

        filesystem.fs.create_file(&file, "").unwrap();

        assert!(filesystem.add_tag(&tag, file).is_err());
    }
}
