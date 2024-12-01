use super::*;

#[derive(Debug, thiserror::Error)]
#[error("No file or directory with this name exists.")]
pub struct NoFileError;

#[derive(Debug, thiserror::Error)]
#[error("A file or directory with this name already exists.")]
pub struct FileExistsError;

#[derive(Debug, thiserror::Error)]
#[error("No metadata for this name exists. Please run `tag build`.")]
pub struct NoMetadataExistsError;

#[derive(Debug, thiserror::Error)]
#[error("Metadata for this name already exists. An error may have previously occured. Please move `{0}` or run `tag build`.")]
pub struct MetadataExistsError(pub(crate) PathBuf);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}`")]
pub struct HasTagError(TaggedPath, String);

impl HasTagError {
    pub(crate) fn new<T>(path: TaggedPath, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(path, tag.as_ref().to_string())
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` lacks `{1}`")]
pub struct LacksTagError(TaggedPath, String);

impl LacksTagError {
    pub(crate) fn new<T>(path: TaggedPath, tag: T) -> Self
    where
        T: AsRef<TagRef>,
    {
        Self(path, tag.as_ref().to_string())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Tag is not unique")]
pub struct NonUniqueTagError;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub enum TagsError {
    InvalidString(#[from] InvalidStringError),
    InvalidTag(#[from] InvalidTagError),
    Filesystem(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid Unicode string")]
pub struct InvalidStringError(pub(crate) PathBuf);

#[derive(Debug, thiserror::Error)]
#[error("`{0}` is not a valid tag")]
pub struct InvalidTagError(pub(crate) PathBuf);
