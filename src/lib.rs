mod tag;
mod tagged_file;

#[cfg(test)]
mod testing;

pub use crate::{
    tag::{Tag, TagRef},
    tagged_file::TaggedFile,
};

pub const SEPARATORS: [char; 2] = ['-', '/'];
pub const TAG_END: char = '_';
