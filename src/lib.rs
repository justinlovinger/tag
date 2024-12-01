mod fs;
mod name;
mod organize;
mod root;
mod tag;
mod tagged_filesystem;
mod tagged_path;

#[cfg(test)]
mod testing;

pub use crate::{
    name::{Name, NameRef},
    root::Root,
    tag::{Tag, TagRef},
    tagged_filesystem::{TaggedFilesystem, TaggedFilesystemBuilder},
    tagged_path::TaggedPath,
};

pub const METADATA_DIR: &str = ".tag";
pub const FILES_DIR: &str = "files";
pub const TAGS_DIR: &str = "tags";
pub const PROGRAM_TAGS_DIR: &str = "tag";

pub const INLINE_SEPARATOR: char = '-';
pub const DIR_SEPARATOR: char = '/';
pub const SEPARATORS: [char; 2] = [INLINE_SEPARATOR, DIR_SEPARATOR];
pub const TAG_END: char = '_';

// We _could_ examine the actual filesystem
// to find a more accurate limit.
// However,
// then tagged directories may be less portable,
// and most filesystems have the below limit anyway.
pub const PATH_PART_MAX_LEN: usize = 255;
