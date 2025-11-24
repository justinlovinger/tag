mod combine;
mod ext;
mod find;
mod root;
mod sort;
mod tag;
mod tagged_path;

#[cfg(test)]
mod testing;

pub use crate::{
    combine::{combine, uncombine},
    ext::{Ext, ExtRef},
    find::find,
    root::Root,
    sort::sort_tags_by_subfrequency,
    tag::{Tag, TagRef},
    tagged_path::{TaggedPath, TaggedPathRef},
};

pub const METADATA_DIR: &str = ".tag";

pub const INLINE_SEPARATOR: char = '-';
pub const DIR_SEPARATOR: char = '/';
pub const EXT_SEPARATOR: char = '.';
pub const SEPARATORS: [char; 3] = [INLINE_SEPARATOR, DIR_SEPARATOR, EXT_SEPARATOR];

// We _could_ examine the actual filesystem
// to find a more accurate limit.
// However,
// then tagged directories may be less portable,
// and most filesystems have the below limit anyway.
pub const PATH_PART_MAX_LEN: usize = 255;
