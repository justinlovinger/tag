use std::{fmt, iter::once};

use auto_enums::auto_enum;
use itertools::Itertools;

use crate::{types::MoveOp, Tag, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

pub fn organize(files: Vec<TaggedFile>) -> impl Iterator<Item = MoveOp> {
    _organize(files, String::new(), Vec::new(), 0)
}

#[auto_enum]
fn _organize(
    files: Vec<TaggedFile>,
    prefix: String,
    tags: Vec<Tag>,
    parent_count: usize,
) -> impl Iterator<Item = MoveOp> {
    #[auto_enum(Iterator)]
    if let Some((tag, count)) = files
        .iter()
        .flat_map(|file| file.tags())
        .counts()
        .into_iter()
        .filter(|(tag, _)| !tags.iter().map(|x| x.as_ref()).contains(tag))
        .max_by(|(tag, count), (other_tag, other_count)| {
            count
                .cmp(other_count)
                .then_with(|| tag.len().cmp(&other_tag.len()))
                .then_with(|| tag.cmp(other_tag).reverse())
        })
    {
        #[auto_enum(Iterator)]
        if count == 1 {
            files.into_iter().map(move |file| MoveOp {
                to: format!(
                    "{}{}{TAG_END}{}",
                    FmtPrefix(&prefix),
                    file.tags()
                        .filter(|tag| !tags.iter().map(|x| x.as_ref()).contains(tag))
                        .format_with("", |tag, f| {
                            f(&tag)?;
                            f(&INLINE_SEPARATOR)?;
                            Ok(())
                        }),
                    file.name()
                )
                .into(),
                from: file.into(),
            })
        } else if count == parent_count {
            let tag = tag.to_owned();
            Box::new(_organize(
                files,
                format!("{}{tag}", FmtPrefixInline(&prefix)),
                tags.into_iter().chain(once(tag)).collect(),
                count,
            )) as Box<dyn Iterator<Item = _>>
        } else {
            let tag = tag.to_owned();
            let (with_tag, without_tag) = files
                .into_iter()
                .partition::<Vec<_>, _>(|file| file.tags().contains(&tag.as_ref()));
            Box::new(
                _organize(
                    with_tag,
                    format!("{}{tag}", FmtPrefix(&prefix)),
                    tags.iter().cloned().chain(once(tag)).collect(),
                    count,
                )
                .chain(_organize(without_tag, prefix, tags, parent_count)),
            ) as Box<dyn Iterator<Item = _>>
        }
    } else {
        files.into_iter().map(move |file| MoveOp {
            to: format!("{}{TAG_END}{}", FmtPrefix(&prefix), file.name()).into(),
            from: file.into(),
        })
    }
}

struct FmtPrefix<'a>(&'a str);

impl fmt::Display for FmtPrefix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            self.0.fmt(f)?;
            DIR_SEPARATOR.fmt(f)?;
        }
        Ok(())
    }
}

struct FmtPrefixInline<'a>(&'a str);

impl fmt::Display for FmtPrefixInline<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            self.0.fmt(f)?;
            INLINE_SEPARATOR.fmt(f)?;
        }
        Ok(())
    }
}
