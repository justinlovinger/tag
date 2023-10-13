#![allow(clippy::needless_borrow)] // False positives

mod organize;
mod tag;
mod tagged_file;
mod tagged_filesystem;
mod types;

#[cfg(test)]
mod testing;

pub use crate::{
    tag::{Tag, TagRef},
    tagged_file::TaggedFile,
    tagged_filesystem::{AddError, DelError, TaggedFilesystem, TaggedFilesystemBuilder},
};

pub const INLINE_SEPARATOR: char = '-';
pub const DIR_SEPARATOR: char = '/'; // This should depend on platform
pub const SEPARATORS: [char; 2] = [INLINE_SEPARATOR, DIR_SEPARATOR];
pub const TAG_END: char = '_';
