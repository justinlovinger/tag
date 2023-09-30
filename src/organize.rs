use std::collections::{HashMap, HashSet};

use auto_enums::auto_enum;
use itertools::Itertools;

use crate::{types::MoveOp, TagRef, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

pub fn organize(files: &[TaggedFile]) -> impl Iterator<Item = MoveOp> + '_ {
    let mut tags_files: TagsFiles = HashMap::new();
    for file in files.iter().enumerate().map(|(i, file)| {
        // This is safe
        // because every `i` is unique.
        unsafe { WithID::new(file, i) }
    }) {
        for tag in file.tags() {
            tags_files.entry(tag).or_default().insert(file);
        }
    }
    _organize(
        Files {
            tags_files,
            untagged_files: Vec::new(), // Files with no tags will never move.
        },
        String::new(),
        0,
    )
}

#[auto_enum(Iterator)]
fn _organize(
    mut files: Files,
    prefix: String,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> + '_ {
    let mut moves = Vec::new();
    loop {
        if let Some((tag, count)) = tag_to_split(&files.tags_files) {
            debug_assert_ne!(count, 0);
            // Singleton tags are common,
            // making optimizing for them worthwhile.
            if count == 1 {
                debug_assert_eq!(
                    files.tags_files.values().map(|xs| xs.len()).max().unwrap(),
                    1
                );
                return moves
                    .into_iter()
                    .chain(to_move_ops(
                        files.untagged_files,
                        final_prefix(&prefix, parent_count),
                    ))
                    .chain(organize_singleton_tags(
                        files.tags_files,
                        prefix,
                        parent_count,
                    ));
            } else {
                let tag = tag.to_owned();
                let (with_tag, without_tag) = files.partition(&tag);
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
            }
        } else {
            return moves.into_iter().chain(to_move_ops(
                files.untagged_files,
                final_prefix(prefix, parent_count),
            ));
        }
    }
}

fn organize_singleton_tags(
    tags_files: TagsFiles,
    prefix: String,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> + '_ {
    let unused_tags: HashSet<_> = tags_files.keys().cloned().collect();
    let prefix = final_prefix(prefix, parent_count);
    tags_files
        .into_values()
        .flatten()
        .unique()
        .map(move |file| MoveOp {
            to: format!(
                "{}{}{TAG_END}{}",
                &prefix,
                file.tags()
                    .filter(|tag| unused_tags.contains(tag))
                    .sorted_by(|tag, other| tag
                        .len()
                        .cmp(&other.len())
                        .reverse()
                        .then_with(|| tag.cmp(other)))
                    .format_with("", |tag, f| {
                        f(&tag)?;
                        f(&INLINE_SEPARATOR)?;
                        Ok(())
                    }),
                file.name()
            )
            .into(),
            from: file.as_path().into(),
        })
}

fn to_move_ops(files: UntaggedFiles, prefix: String) -> impl Iterator<Item = MoveOp> + '_ {
    files.into_iter().map(move |file| MoveOp {
        to: format!("{}{TAG_END}{}", &prefix, file.name()).into(),
        from: file.as_path().into(),
    })
}

fn final_prefix(prefix: impl std::fmt::Display, parent_count: usize) -> String {
    if parent_count == 0 {
        prefix.to_string()
    } else if parent_count == 1 {
        format!("{}{INLINE_SEPARATOR}", prefix)
    } else {
        format!("{}{DIR_SEPARATOR}", prefix)
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

#[derive(Debug)]
struct Files<'a> {
    tags_files: TagsFiles<'a>,
    untagged_files: UntaggedFiles<'a>,
}

type TagsFiles<'a> = HashMap<&'a TagRef, HashSet<WithID<&'a TaggedFile>>>;

type UntaggedFiles<'a> = Vec<WithID<&'a TaggedFile>>;

impl<'a> Files<'a> {
    /// Return (with_tag, without_tag).
    pub fn partition(mut self, tag: &TagRef) -> (Self, Self) {
        let files = self.tags_files.remove(tag).unwrap();
        let mut tags_files: TagsFiles = HashMap::new();
        let mut untagged_files = Vec::new();
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

/// More efficiently hash and compare
/// when given unique IDs.
#[derive(Clone, Copy, Debug)]
struct WithID<T> {
    inner: T,
    id: usize,
}

impl<T> Eq for WithID<T> {}

impl<T> PartialEq for WithID<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> std::hash::Hash for WithID<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> AsRef<T> for WithID<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::Deref for WithID<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> WithID<T> {
    /// This function is safe
    /// when given a unique ID.
    pub unsafe fn new(inner: T, id: usize) -> Self {
        Self { inner, id }
    }
}
