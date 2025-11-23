use std::path::PathBuf;

use itertools::Itertools;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    TagRef, TaggedPath, DIR_SEPARATOR, EXT_SEPARATOR, INLINE_SEPARATOR, PATH_PART_MAX_LEN,
};

const FILLER_TAG: char = '_';

/// Return paths with optimal directory separators
///
/// Paths are returned in the order given.
///
/// At most one tag of the form `_[0-9]*` may be added
/// to each path
/// to avoid files starting with `.` and being hidden in Linux
/// or to differentiate paths.
pub fn combine(paths: &[TaggedPath]) -> impl Iterator<Item = PathBuf> {
    let mut res = combine_(
        paths
            .iter()
            .map(|path| {
                let tags = path.tags().collect();
                (path, tags)
            })
            .enumerate()
            .collect(),
    );
    res.sort_by_key(|(i, _)| *i);
    res.into_iter().map(|(_, path)| path)
}

fn combine_<T>(mut paths: Vec<(usize, (&TaggedPath, Vec<T>))>) -> Vec<(usize, PathBuf)>
where
    T: AsRef<TagRef>,
{
    fn combine_inner<T>(
        sorted: &[(usize, (&TaggedPath, Vec<T>))],
        prefix: PathBuf,
        tag_index: usize,
    ) -> Vec<(usize, PathBuf)>
    where
        T: AsRef<TagRef>,
    {
        let mut res = Vec::new();
        let mut i = 0;
        let mut without_tags: FxHashMap<_, Vec<_>> = FxHashMap::default();
        while let Some((orig_i, (path, tags))) = sorted.get(i) {
            match tags.get(tag_index) {
                Some(tag) => {
                    let j = sorted
                        .iter()
                        .enumerate()
                        .skip(i + 1)
                        .find(|(_, (_, (_, tags)))| {
                            tags.get(tag_index)
                                .is_none_or(|other| other.as_ref() != tag.as_ref())
                        })
                        .map(|(j, _)| j)
                        .unwrap_or(sorted.len());
                    if j == i + 1 {
                        let inline_tags = &tags[tag_index..];
                        let mut len = 0;
                        #[allow(clippy::needless_collect)] // `collect` is needed for `len`.
                        let separators = inline_tags
                            .iter()
                            .map(|tag| tag.as_ref().len())
                            .tuple_windows()
                            .map({
                                let len = &mut len;
                                |(tag_len, next_len)| {
                                    if *len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len
                                        <= PATH_PART_MAX_LEN
                                    {
                                        *len += tag_len + INLINE_SEPARATOR.len_utf8();
                                        INLINE_SEPARATOR
                                    } else {
                                        *len = 0;
                                        DIR_SEPARATOR
                                    }
                                }
                            })
                            .collect::<SmallVec<[char; 8]>>(); // `SmallVec` should handle most cases without heap-allocation.
                        res.push((
                            *orig_i,
                            prefix.join(format!(
                                "{}{}{}",
                                inline_tags
                                    .iter()
                                    .zip(separators.into_iter().map(Some).chain([None]))
                                    .format_with("", |(tag, sep), f| {
                                        f(&tag.as_ref())?;
                                        if let Some(sep) = sep {
                                            f(&sep)?;
                                        }
                                        Ok(())
                                    }),
                                if len
                                    + inline_tags.last().map_or(0, |tag| tag.as_ref().len())
                                    + EXT_SEPARATOR.len_utf8()
                                    + path.ext().len()
                                    <= PATH_PART_MAX_LEN
                                {
                                    EXT_SEPARATOR.to_string()
                                } else {
                                    format!("{DIR_SEPARATOR}{FILLER_TAG}{EXT_SEPARATOR}")
                                },
                                path.ext(),
                            )),
                        ));
                    } else {
                        let (_, (_, tags_of_last)) = &sorted[j - 1];
                        let next_tag_index = tags
                            .iter()
                            .zip(tags_of_last)
                            .enumerate()
                            .skip(tag_index + 1)
                            .find(|(_, (tag, other))| tag.as_ref() != other.as_ref())
                            .map(|(offset, _)| offset)
                            .unwrap_or_else(|| tags.len().min(tags_of_last.len()));
                        let inline_tags = &tags[tag_index..next_tag_index];
                        let separators = inline_tags
                            .iter()
                            .map(|tag| tag.as_ref().len())
                            .tuple_windows()
                            .map({
                                let mut len = 0;
                                move |(tag_len, next_len)| {
                                    if len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len
                                        <= PATH_PART_MAX_LEN
                                    {
                                        len += tag_len + INLINE_SEPARATOR.len_utf8();
                                        INLINE_SEPARATOR.to_string()
                                    } else {
                                        len = 0;
                                        DIR_SEPARATOR.to_string()
                                    }
                                }
                            })
                            .chain(["".to_string()]);
                        res.extend(combine_inner(
                            &sorted[i..j],
                            prefix.join(
                                inline_tags
                                    .iter()
                                    .zip(separators)
                                    .format_with("", |(tag, sep), f| {
                                        f(&tag.as_ref())?;
                                        f(&sep)?;
                                        Ok(())
                                    })
                                    // Creating a string here is wasteful,
                                    // but `PathBuf` does not support joining a `Format`.
                                    .to_string(),
                            ),
                            next_tag_index,
                        ));
                    }
                    i = j;
                }
                None => {
                    without_tags
                        .entry(path.ext())
                        .or_default()
                        .push((*orig_i, *path));
                    i += 1;
                }
            }
        }

        for paths in without_tags.into_values() {
            if paths.len() == 1 {
                let (orig_i, path) = paths.into_iter().next().unwrap();
                res.push((
                    orig_i,
                    prefix.join(format!("{FILLER_TAG}{EXT_SEPARATOR}{}", path.ext())),
                ));
            } else {
                for (id, (orig_i, path)) in (1..).zip(paths) {
                    res.push((
                        orig_i,
                        prefix.join(format!("{FILLER_TAG}{id}{EXT_SEPARATOR}{}", path.ext())),
                    ));
                }
            }
        }

        res
    }

    paths.sort_by(|(_, (_, tags)), (_, (_, other))| {
        tags.iter()
            .map(|tag| tag.as_ref())
            .cmp(other.iter().map(|tag| tag.as_ref()))
            // Reversing the order places items with fewer tags at the end,
            // which simplifies checking how many items have no more tags
            // at a given recursive step.
            .reverse()
    });
    combine_inner(&paths, PathBuf::new(), 0)
}

#[cfg(test)]
mod tests {
    use std::sync::LazyLock;

    use proptest::prelude::*;
    use regex::Regex;
    use test_strategy::proptest;

    use crate::testing::{tagged_path, TaggedPaths};

    use super::*;

    #[test]
    fn combine_separates_different_prefixes_by_inline() {
        assert_eq!(
            combine(&[tagged_path("foo-bar.x"), tagged_path("baz-bin.x")]).collect::<Vec<_>>(),
            [PathBuf::from("foo-bar.x"), PathBuf::from("baz-bin.x")]
        );
    }

    #[test]
    fn combine_separates_common_prefixes_by_dir() {
        assert_eq!(
            combine(&[tagged_path("foo-bar.x"), tagged_path("foo-baz.x")]).collect::<Vec<_>>(),
            [PathBuf::from("foo/bar.x"), PathBuf::from("foo/baz.x")]
        );
    }

    #[test]
    fn combine_separates_by_dir_and_inline() {
        assert_eq!(
            combine(&[tagged_path("foo-bar-baz.x"), tagged_path("foo-baz-bar.x")])
                .collect::<Vec<_>>(),
            [
                PathBuf::from("foo/bar-baz.x"),
                PathBuf::from("foo/baz-bar.x")
            ]
        );
    }

    #[test]
    fn combine_uses_inline_in_common_prefixes() {
        assert_eq!(
            combine(&[tagged_path("foo-bar-baz.x"), tagged_path("foo-bar-bin.x")])
                .collect::<Vec<_>>(),
            [
                PathBuf::from("foo-bar/baz.x"),
                PathBuf::from("foo-bar/bin.x"),
            ]
        );
    }

    #[test]
    fn combine_does_not_use_inline_in_common_prefix_if_not_all_common() {
        assert_eq!(
            combine(&[
                tagged_path("foo-bar-baz.x"),
                tagged_path("foo-bar-bin.x"),
                tagged_path("foo-bin.x"),
            ])
            .collect::<Vec<_>>(),
            [
                PathBuf::from("foo/bar/baz.x"),
                PathBuf::from("foo/bar/bin.x"),
                PathBuf::from("foo/bin.x"),
            ]
        );
    }

    #[test]
    fn combine_uses_dir_in_long_common_prefixes() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        assert_eq!(
            combine(&[
                tagged_path(format!("{a}-{b}-{c}-baz.x")),
                tagged_path(format!("{a}-{b}-{c}-bin.x")),
            ])
            .collect::<Vec<_>>(),
            [
                PathBuf::from(format!("{a}-{b}/{c}/baz.x")),
                PathBuf::from(format!("{a}-{b}/{c}/bin.x")),
            ]
        );
    }

    #[test]
    fn combine_uses_dir_in_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        assert_eq!(
            combine(&[
                tagged_path("foo.x"),
                tagged_path(format!("foo-{a}-{b}-{c}.x")),
            ])
            .collect::<Vec<_>>(),
            [
                PathBuf::from("foo/_.x"),
                PathBuf::from(format!("foo/{a}-{b}/{c}.x")),
            ]
        );
    }

    #[test]
    fn combine_adds_filler_tag() {
        assert_eq!(
            combine(&[tagged_path(".x")]).collect::<Vec<_>>(),
            [PathBuf::from("_.x")]
        );
    }

    #[test]
    fn combine_adds_different_filler_tags_to_multiple_paths_with_same_extension() {
        assert_eq!(
            combine(&[tagged_path(".x"), tagged_path(".x")]).collect::<Vec<_>>(),
            [PathBuf::from("_1.x"), PathBuf::from("_2.x")]
        );
    }

    #[test]
    fn combine_adds_same_filler_tag_to_multiple_paths_with_different_extensions() {
        assert_eq!(
            combine(&[tagged_path(".x"), tagged_path(".y")]).collect::<Vec<_>>(),
            [PathBuf::from("_.x"), PathBuf::from("_.y")]
        );
    }

    #[test]
    fn combine_adds_filler_tag_after_slash() {
        assert_eq!(
            combine(&[tagged_path("a.x"), tagged_path("a-b.x"),]).collect::<Vec<_>>(),
            [PathBuf::from("a/_.x"), PathBuf::from("a/b.x")]
        );
    }

    #[test]
    fn combine_adds_filler_tag_after_slash_from_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        assert_eq!(
            combine(&[tagged_path(format!("{a}-{b}.{c}"))]).collect::<Vec<_>>(),
            [PathBuf::from(format!("{a}-{b}/_.{c}"))]
        );
    }

    #[proptest]
    fn combine_does_not_change_tags_except_for_adding_filler_tags(paths: TaggedPaths) {
        static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^_[0-9]*$").unwrap());
        prop_assert_eq!(
            combine(&paths.0)
                .map(|path| TaggedPath::from_path(path)
                    .unwrap()
                    .tags()
                    .filter(|tag| !RE.is_match(tag.as_str()))
                    .map(|tag| tag.to_owned())
                    .collect::<Vec<_>>())
                .collect::<Vec<_>>(),
            paths
                .into_iter()
                .map(|path| path.tags().map(|tag| tag.to_owned()).collect::<Vec<_>>())
                .collect::<Vec<_>>()
        );
    }

    #[proptest]
    fn combine_does_not_change_extensions(paths: TaggedPaths) {
        prop_assert_eq!(
            combine(&paths.0)
                .map(|path| TaggedPath::from_path(path).unwrap().ext().to_owned())
                .collect::<Vec<_>>(),
            paths
                .into_iter()
                .map(|path| path.ext().to_owned())
                .collect::<Vec<_>>()
        );
    }
}
