use std::collections::{BTreeSet, HashMap};

use smallvec::SmallVec;

use crate::{types::MoveOp, TagRef, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

pub fn organize(files: &[TaggedFile]) -> impl Iterator<Item = MoveOp> + '_ {
    let mut tags_files: TagsFiles = HashMap::new();
    for file in files {
        for tag in file.tags() {
            tags_files.entry(tag).or_default().insert(file);
        }
    }
    _organize(
        Files {
            tags_files,
            untagged_files: SmallVec::new(), // Files with no tags will never move.
        },
        String::new(),
        0,
    )
}

fn _organize(
    mut files: Files,
    prefix: String,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> + '_ {
    let mut moves = Vec::new();
    loop {
        if let Some((tag, count)) = tag_to_split(&files.tags_files) {
            let tag = tag.to_owned();
            let (with_tag, without_tag) = files.partition(&tag);
            debug_assert_ne!(count, 0);
            moves.extend(_organize(
                with_tag,
                if parent_count == 0 {
                    format!("{}{tag}", &prefix)
                } else if count == parent_count {
                    format!("{}{INLINE_SEPARATOR}{tag}", &prefix)
                } else {
                    format!("{}{DIR_SEPARATOR}{tag}", &prefix)
                },
                count,
            ));
            files = without_tag;
        } else {
            let prefix = if parent_count == 0 {
                prefix
            } else if parent_count == 1 {
                format!("{}{INLINE_SEPARATOR}", &prefix)
            } else {
                format!("{}{DIR_SEPARATOR}", &prefix)
            };
            return moves
                .into_iter()
                .chain(files.untagged_files.into_iter().map(move |file| MoveOp {
                    to: format!("{}{TAG_END}{}", &prefix, file.name()).into(),
                    from: file.as_path().into(),
                }));
        }
    }
}

type TagsFiles<'a> = HashMap<&'a TagRef, BTreeSet<&'a TaggedFile>>;

#[derive(Debug)]
struct Files<'a> {
    tags_files: TagsFiles<'a>,
    untagged_files: SmallVec<[&'a TaggedFile; 1]>,
}

impl<'a> Files<'a> {
    /// Return (with_tag, without_tag).
    pub fn partition(mut self, tag: &TagRef) -> (Self, Self) {
        let files = self.tags_files.remove(tag).unwrap();
        let mut tags_files: TagsFiles = HashMap::new();
        let mut untagged_files = SmallVec::new();
        for file in files {
            let mut added = false;
            for tag in file.tags() {
                if let Some(self_tag_files) = self.tags_files.get_mut(tag) {
                    tags_files.entry(tag).or_default().insert(file);
                    added = true;
                    if self_tag_files.len() == 1 {
                        self.tags_files.remove(tag);
                    } else {
                        self_tag_files.remove(file);
                    }
                }
            }
            if !added {
                untagged_files.push(file);
            }
        }
        (
            Self {
                tags_files,
                untagged_files,
            },
            self,
        )
    }
}

fn tag_to_split<'a>(tags_files: &'a TagsFiles) -> Option<(&'a TagRef, usize)> {
    tags_files
        .iter()
        .map(|(tag, tag_files)| (tag, tag_files.len()))
        .max_by(|(tag, count), (other_tag, other_count)| {
            count
                .cmp(other_count)
                .then_with(|| tag.len().cmp(&other_tag.len()))
                .then_with(|| tag.cmp(other_tag).reverse())
        })
        .map(|(tag, count)| (*tag, count))
}
