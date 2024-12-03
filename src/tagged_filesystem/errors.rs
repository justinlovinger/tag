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
