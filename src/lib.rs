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
    tagged_filesystem::{TaggedFilesystem, TaggedFilesystemBuilder},
};

pub const INLINE_SEPARATOR: char = '-';
pub const DIR_SEPARATOR: char = '/';
pub const SEPARATORS: [char; 2] = [INLINE_SEPARATOR, DIR_SEPARATOR];
pub const TAG_END: char = '_';

// We _could_ examine the actual filesystem
// to find a more accurate limit.
// However,
// then tagged directories may be less portable,
// and most filesystems have the below limit anyway.
pub const FILENAME_MAX_LEN: usize = 255;
