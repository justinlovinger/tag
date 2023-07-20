mod tag;
mod tagged_file;
mod tagged_filesystem;

#[cfg(test)]
mod testing;

pub use crate::{
    tag::{Tag, TagRef},
    tagged_file::TaggedFile,
    tagged_filesystem::{AddError, TaggedFilesystem},
};

pub const INLINE_SEPARATOR: char = '-';
pub const SEPARATORS: [char; 2] = [INLINE_SEPARATOR, '/'];
pub const TAG_END: char = '_';
