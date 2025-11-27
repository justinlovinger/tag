use std::{
    marker::Sync,
    path::{Path, PathBuf},
    sync::{mpsc, LazyLock},
};

use itertools::Itertools;
use rayon::prelude::*;
use regex::Regex;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    organize::into_iter_rayon, TagRef, TaggedPath, TaggedPathRef, DIR_SEPARATOR, EXT_SEPARATOR,
    INLINE_SEPARATOR, PATH_PART_MAX_LEN,
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
pub fn combine<P>(paths: &[P]) -> Vec<PathBuf>
where
    P: AsRef<TaggedPathRef> + Sync + Send,
{
    combine_(
        paths
            .iter()
            .map(|path| {
                let tags = path.as_ref().tags().collect();
                (path, tags)
            })
            .enumerate()
            .collect(),
    )
}

/// Return paths without directory separators or `_[0-9]*` tags
///
/// Paths are returned in the order given.
pub fn uncombine<I, P>(paths: I) -> Vec<TaggedPath>
where
    I: IntoParallelIterator<Item = P>,
    I::Iter: IndexedParallelIterator,
    P: AsRef<TaggedPathRef>,
{
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^_[0-9]*$").unwrap());
    let mut res = Vec::new();
    paths
        .into_par_iter()
        .map(|path| {
            let path = path.as_ref();
            TaggedPath::from_tags(
                path.tags().filter(|tag| !RE.is_match(tag.as_str())),
                path.ext(),
            )
        })
        .collect_into_vec(&mut res);
    res
}

fn combine_<P, T>(mut paths: Vec<(usize, (P, Vec<T>))>) -> Vec<PathBuf>
where
    P: AsRef<TaggedPathRef> + Sync + Send,
    T: AsRef<TagRef> + Sync + Send,
{
    paths.par_sort_unstable_by(|(_, (_, tags)), (_, (_, other))| {
        tags.iter()
            .map(|tag| tag.as_ref())
            .cmp(other.iter().map(|tag| tag.as_ref()))
            // Reversing the order places items with fewer tags at the end.
            .reverse()
    });

    let (sender, receiver) = mpsc::channel();
    let len = paths.len();
    rayon::join(
        move || {
            combine_inner(&sender, &paths, &PathBuf::new(), 0);
        },
        move || {
            let mut res = Vec::with_capacity(len);
            let uninit = res.spare_capacity_mut();
            into_iter_rayon(receiver, |(i, path)| {
                uninit[i].write(path);
            });
            // SAFETY: `receiver` contains an item for each index, which was just used to initialize the vector.
            unsafe {
                res.set_len(len);
            }
            res
        },
    )
    .1
}

fn combine_inner<P, T>(
    sender: &mpsc::Sender<(usize, PathBuf)>,
    mut sorted: &[(usize, (P, Vec<T>))],
    prefix: &Path,
    tag_index: usize,
) where
    P: AsRef<TaggedPathRef> + Sync,
    T: AsRef<TagRef> + Sync,
{
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        while let Some((orig_i, (path, tags))) = sorted.first() {
            match tags.get(tag_index) {
                Some(tag) => {
                    let j = sorted
                        .iter()
                        .enumerate()
                        .skip(1)
                        .find(|(_, (_, (_, tags)))| {
                            tags.get(tag_index)
                                .is_none_or(|other| other.as_ref() != tag.as_ref())
                        })
                        .map(|(j, _)| j)
                        .unwrap_or(sorted.len());
                    if j == 1 {
                        // Note,
                        // Using `rayon::join` here is a net slowdown.
                        sender
                            .send((*orig_i, combine_inline(prefix, path, &tags[tag_index..])))
                            .unwrap();
                        sorted = &sorted[1..];
                    } else {
                        rayon::join(
                            || {
                                combine_next_tag_index(sender, &sorted[..j], prefix, tag_index);
                            },
                            || {
                                combine_inner(sender, &sorted[j..], prefix, tag_index);
                            },
                        );
                        return;
                    }
                }
                None => {
                    return combine_without_tags(sender, sorted, prefix, tag_index);
                }
            }
        }
    })
}

fn combine_inline<P, T>(prefix: &Path, path: P, inline_tags: &[T]) -> PathBuf
where
    P: AsRef<TaggedPathRef>,
    T: AsRef<TagRef>,
{
    let path = path.as_ref();
    let mut len = 0;
    #[allow(clippy::needless_collect)]
    // `collect` is needed for `len`.
    let separators = inline_tags
        .iter()
        .map(|tag| tag.as_ref().len())
        .tuple_windows()
        .map({
            let len = &mut len;
            |(tag_len, next_len)| {
                if *len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len <= PATH_PART_MAX_LEN {
                    *len += tag_len + INLINE_SEPARATOR.len_utf8();
                    INLINE_SEPARATOR
                } else {
                    *len = 0;
                    DIR_SEPARATOR
                }
            }
        })
        .collect::<SmallVec<[char; 8]>>();
    // `SmallVec` should handle most cases without heap-allocation.
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
    ))
}

fn combine_next_tag_index<P, T>(
    sender: &mpsc::Sender<(usize, PathBuf)>,
    sorted: &[(usize, (P, Vec<T>))],
    prefix: &Path,
    tag_index: usize,
) where
    P: AsRef<TaggedPathRef> + Sync,
    T: AsRef<TagRef> + Sync,
{
    let (_, (_, tags)) = &sorted.first().unwrap();
    let (_, (_, tags_of_last)) = &sorted.last().unwrap();
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
                if len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len <= PATH_PART_MAX_LEN {
                    len += tag_len + INLINE_SEPARATOR.len_utf8();
                    INLINE_SEPARATOR.to_string()
                } else {
                    len = 0;
                    DIR_SEPARATOR.to_string()
                }
            }
        })
        .chain(["".to_string()]);
    combine_inner(
        sender,
        sorted,
        &prefix.join(
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
    );
}

fn combine_without_tags<P, T>(
    sender: &mpsc::Sender<(usize, PathBuf)>,
    sorted: &[(usize, (P, Vec<T>))],
    prefix: &Path,
    tag_index: usize,
) where
    P: AsRef<TaggedPathRef>,
    T: AsRef<TagRef>,
{
    let mut without_tags: FxHashMap<_, Vec<_>> = FxHashMap::default();
    for (orig_i, (path, tags)) in sorted {
        debug_assert!(tags.get(tag_index).is_none());
        without_tags
            .entry(path.as_ref().ext())
            .or_default()
            .push(*orig_i);
    }
    for (ext, paths) in without_tags.into_iter() {
        if paths.len() == 1 {
            let orig_i = paths.into_iter().next().unwrap();
            sender
                .send((
                    orig_i,
                    prefix.join(format!("{FILLER_TAG}{EXT_SEPARATOR}{}", ext)),
                ))
                .unwrap();
        } else {
            for (id, orig_i) in (1..).zip(paths) {
                sender
                    .send((
                        orig_i,
                        prefix.join(format!("{FILLER_TAG}{id}{EXT_SEPARATOR}{}", ext)),
                    ))
                    .unwrap();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::testing::{tagged_path, TaggedPaths};

    use super::*;

    #[test]
    fn combine_separates_different_prefixes_by_inline() {
        assert_eq!(
            combine(&[tagged_path("foo-bar.x"), tagged_path("baz-bin.x")]),
            [PathBuf::from("foo-bar.x"), PathBuf::from("baz-bin.x")]
        );
    }

    #[test]
    fn combine_separates_common_prefixes_by_dir() {
        assert_eq!(
            combine(&[tagged_path("foo-bar.x"), tagged_path("foo-baz.x")]),
            [PathBuf::from("foo/bar.x"), PathBuf::from("foo/baz.x")]
        );
    }

    #[test]
    fn combine_separates_by_dir_and_inline() {
        assert_eq!(
            combine(&[tagged_path("foo-bar-baz.x"), tagged_path("foo-baz-bar.x")]),
            [
                PathBuf::from("foo/bar-baz.x"),
                PathBuf::from("foo/baz-bar.x")
            ]
        );
    }

    #[test]
    fn combine_uses_inline_in_common_prefixes() {
        assert_eq!(
            combine(&[tagged_path("foo-bar-baz.x"), tagged_path("foo-bar-bin.x")]),
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
            ]),
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
            ]),
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
            ]),
            [
                PathBuf::from("foo/_.x"),
                PathBuf::from(format!("foo/{a}-{b}/{c}.x")),
            ]
        );
    }

    #[test]
    fn combine_adds_filler_tag() {
        assert_eq!(combine(&[tagged_path(".x")]), [PathBuf::from("_.x")]);
    }

    #[test]
    fn combine_adds_different_filler_tags_to_multiple_paths_with_same_extension() {
        assert_eq!(
            combine(&[tagged_path(".x"), tagged_path(".x")]),
            [PathBuf::from("_1.x"), PathBuf::from("_2.x")]
        );
    }

    #[test]
    fn combine_adds_same_filler_tag_to_multiple_paths_with_different_extensions() {
        assert_eq!(
            combine(&[tagged_path(".x"), tagged_path(".y")]),
            [PathBuf::from("_.x"), PathBuf::from("_.y")]
        );
    }

    #[test]
    fn combine_adds_filler_tag_after_slash() {
        assert_eq!(
            combine(&[tagged_path("a.x"), tagged_path("a-b.x")]),
            [PathBuf::from("a/_.x"), PathBuf::from("a/b.x")]
        );
    }

    #[test]
    fn combine_adds_filler_tag_after_slash_from_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        assert_eq!(
            combine(&[tagged_path(format!("{a}-{b}.{c}"))]),
            [PathBuf::from(format!("{a}-{b}/_.{c}"))]
        );
    }

    #[proptest]
    fn uncombine_inverses_combine(paths: TaggedPaths) {
        // `uncombine` doesn't return directory separators.
        let paths = paths
            .into_iter()
            .map(|path| {
                TaggedPath::new(
                    path.as_str()
                        .replace(DIR_SEPARATOR, &INLINE_SEPARATOR.to_string()),
                )
                .unwrap()
            })
            .collect::<Vec<_>>();
        prop_assert_eq!(
            uncombine(
                combine(&paths)
                    .into_iter()
                    .map(|path| TaggedPath::from_path(path).unwrap())
                    .collect::<Vec<_>>()
            ),
            paths
        )
    }
}
