mod tag;
mod tagged_file;

#[cfg(test)]
mod testing;

pub use crate::{
    tag::{Tag, TagRef},
    tagged_file::TaggedFile,
};

pub const INLINE_SEPARATOR: char = '-';
pub const SEPARATORS: [char; 2] = [INLINE_SEPARATOR, '/'];
pub const TAG_END: char = '_';
