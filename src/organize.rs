use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use by_address::ByThinAddress;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::{types::MoveOp, Tag, TagRef, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

type Prefix = Vec<(Rc<Tag>, usize)>; // (tag, count)

pub fn organize(files: &[TaggedFile]) -> impl Iterator<Item = MoveOp> + '_ {
    let mut tags_files: TagsFiles = HashMap::new();
    for file in files.iter().map(ByThinAddress) {
        for tag in file.tags() {
            tags_files.entry(tag).or_default().insert(file);
        }
    }
    _organize(
        Files {
            inline_tags: extract_inline_tags(&mut tags_files),
            tags_files,
            untagged_files: Vec::new(), // Files with no tags will never move.
        },
        Default::default(),
    )
}

fn _organize(mut files: Files, prefix: Prefix) -> impl Iterator<Item = MoveOp> + '_ {
    let mut moves = Vec::new();
    loop {
        if let Some((tag, count)) = tag_to_split(&files.tags_files) {
            debug_assert_ne!(count, 0);

            let tag = tag.to_owned();
            let (with_tag, without_tag) = files.partition(&tag);
            files = without_tag;

            moves.extend(_organize(
                with_tag,
                prefix
                    .iter()
                    .cloned()
                    .chain([(Rc::new(tag), count)])
                    .collect(),
            ));
        } else {
            debug_assert_eq!(files.tags_files.len(), 0);
            return moves.into_iter().chain(to_move_ops(files, prefix));
        }
    }
}

fn to_move_ops(files: Files, prefix: Prefix) -> impl Iterator<Item = MoveOp> + '_ {
    let separators = prefix
        .iter()
        .map(|(_, count)| count)
        .tuple_windows()
        .map(|(count, next_count)| {
            if count == next_count {
                INLINE_SEPARATOR
            } else {
                DIR_SEPARATOR
            }
        })
        .chain([DIR_SEPARATOR]);
    let prefix = Rc::new(format!(
        "{}",
        prefix
            .iter()
            .map(|(tag, _)| tag)
            .zip(separators)
            .format_with("", |(tag, sep), f| {
                f(&tag)?;
                f(&sep)?;
                Ok(())
            }),
    ));
    files
        .untagged_files
        .into_iter()
        .map({
            let prefix = Rc::clone(&prefix);
            move |file| MoveOp {
                to: format!("{}{TAG_END}{}", &prefix, file.name()).into(),
                from: file.as_path().into(),
            }
        })
        .chain(files.inline_tags.into_iter().map(move |(file, mut tags)| {
            tags.sort_unstable_by(|tag, other| {
                tag.len()
                    .cmp(&other.len())
                    .reverse()
                    .then_with(|| tag.cmp(other))
            });
            MoveOp {
                to: format!(
                    "{}{}{TAG_END}{}",
                    &prefix,
                    tags.into_iter().format_with("", |tag, f| {
                        f(&tag)?;
                        f(&INLINE_SEPARATOR)?;
                        Ok(())
                    }),
                    file.name()
                )
                .into(),
                from: file.as_path().into(),
            }
        }))
        .filter(|MoveOp { from, to }| from != to)
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

#[derive(Debug)]
struct Files<'a> {
    tags_files: TagsFiles<'a>,
    untagged_files: UntaggedFiles<'a>,
    inline_tags: InlineTags<'a>,
}

type TagsFiles<'a> = HashMap<&'a TagRef, HashSet<ByThinAddress<&'a TaggedFile>>>;

type UntaggedFiles<'a> = Vec<ByThinAddress<&'a TaggedFile>>;

type InlineTags<'a> = HashMap<ByThinAddress<&'a TaggedFile>, SmallVec<[&'a TagRef; 1]>>;

impl<'a> Files<'a> {
    /// Return (with_tag, without_tag).
    pub fn partition(mut self, tag: &TagRef) -> (Self, Self) {
        let files = self.tags_files.remove(tag).unwrap();
        let mut tags_files: TagsFiles = HashMap::new();
        let mut untagged_files = Vec::new();
        let mut inline_tags = HashMap::new();
        for file in files {
            let mut added = false;
            for tag in file.tags() {
                if let Some(self_tag_files) = self.tags_files.get_mut(tag) {
                    tags_files.entry(tag).or_default().insert(file);
                    added = true;
                    if self_tag_files.len() == 1 {
                        self.tags_files.remove(tag);
                    } else {
                        self_tag_files.remove(&file);
                    }
                }
            }
            match self.inline_tags.remove(&file) {
                Some(tags) => {
                    inline_tags.insert(file, tags);
                }
                None => {
                    if !added {
                        untagged_files.push(file)
                    }
                }
            }
        }
        extract_inline_tags_into(&mut tags_files, &mut inline_tags);
        extract_inline_tags_into(&mut self.tags_files, &mut self.inline_tags);
        (
            Self {
                tags_files,
                untagged_files,
                inline_tags,
            },
            self,
        )
    }
}

fn extract_inline_tags<'a>(tags_files: &mut TagsFiles<'a>) -> InlineTags<'a> {
    let mut inline_tags: InlineTags = HashMap::new();
    extract_inline_tags_into(tags_files, &mut inline_tags);
    inline_tags
}

fn extract_inline_tags_into<'a>(tags_files: &mut TagsFiles<'a>, inline_tags: &mut InlineTags<'a>) {
    tags_files.retain(|tag, tag_files| {
        if tag_files.len() == 1 {
            let file = *tag_files.iter().next().unwrap();
            inline_tags.entry(file).or_default().push(tag);
            false
        } else {
            true
        }
    });
}
