use std::{collections::HashSet, iter::once};

use auto_enums::auto_enum;
use itertools::Itertools;

use crate::{types::MoveOp, Tag, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

pub fn organize(files: Vec<TaggedFile>) -> impl Iterator<Item = MoveOp> {
    _organize(files, String::new(), HashSet::new(), 0)
}

#[auto_enum(Iterator)]
fn _organize(
    files: Vec<TaggedFile>,
    prefix: String,
    used: HashSet<Tag>,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> {
    if let Some(tag) = tag_to_split(&files, &used) {
        let (with_tag, without_tag) = files
            .into_iter()
            .partition::<Vec<_>, _>(|file| file.tags().contains(&tag.as_ref()));
        let count = with_tag.len();
        Box::new(
            _organize(
                with_tag,
                if parent_count == 0 {
                    format!("{}{tag}", &prefix)
                } else if count == parent_count {
                    format!("{}{INLINE_SEPARATOR}{tag}", &prefix)
                } else {
                    format!("{}{DIR_SEPARATOR}{tag}", &prefix)
                },
                used.iter().cloned().chain(once(tag)).collect(),
                count,
            )
            .chain(_organize(without_tag, prefix, used, parent_count)),
        ) as Box<dyn Iterator<Item = _>>
    } else {
        let prefix = if parent_count == 0 {
            prefix
        } else if parent_count == 1 {
            format!("{}{INLINE_SEPARATOR}", &prefix)
        } else {
            format!("{}{DIR_SEPARATOR}", &prefix)
        };
        files.into_iter().map(move |file| MoveOp {
            to: format!("{}{TAG_END}{}", &prefix, file.name()).into(),
            from: file.into(),
        })
    }
}

fn tag_to_split(files: &[TaggedFile], used: &HashSet<Tag>) -> Option<Tag> {
    files
        .iter()
        .flat_map(|file| file.tags())
        .counts()
        .into_iter()
        .filter(|(tag, _)| !used.contains(*tag))
        .max_by(|(tag, count), (other_tag, other_count)| {
            count
                .cmp(other_count)
                .then_with(|| tag.len().cmp(&other_tag.len()))
                .then_with(|| tag.cmp(other_tag).reverse())
        })
        .map(|(tag, _)| tag.to_owned())
}
