use std::{collections::BTreeSet, path::PathBuf};

use internment::Intern;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    ExtRef, Tag, TagRef, TaggedPath, DIR_SEPARATOR, EXT_SEPARATOR, INLINE_SEPARATOR,
    PATH_PART_MAX_LEN, TAG_IGNORE,
};

use self::partition::{Partition, TagsPaths};

pub fn sort_tags_by_subfrequency(paths: &[TaggedPath]) -> impl Iterator<Item = TaggedPath> + '_ {
    let mut res = sort_tags_by_subfrequency_(paths);
    res.sort_by_key(|(i, (_, _))| *i);
    res.into_iter().map(|(_, (orig, tags))| {
        from_tags_with_ignored(&tags, &orig.ignored_tags().collect::<Vec<_>>(), orig.ext())
    })
}

fn from_tags_with_ignored<T, J, E>(tags: &[T], ignored_tags: &[J], ext: E) -> TaggedPath
where
    T: AsRef<TagRef>,
    J: AsRef<str>,
    E: AsRef<ExtRef>,
{
    let sep = if tags.is_empty() || ignored_tags.is_empty() {
        String::new()
    } else {
        INLINE_SEPARATOR.to_string()
    };
    TaggedPath::new(format!(
        "{}{sep}{}{EXT_SEPARATOR}{}",
        tags.iter()
            .map(|tag| tag.as_ref())
            .format(INLINE_SEPARATOR.to_string().as_str()),
        ignored_tags
            .iter()
            .format_with(INLINE_SEPARATOR.to_string().as_str(), |s, f| {
                f(&TAG_IGNORE)?;
                f(&s.as_ref())
            }),
        ext.as_ref(),
    ))
    .unwrap()
}

pub fn combine(paths: &[TaggedPath]) -> impl Iterator<Item = PathBuf> + '_ {
    let mut res = combine_(
        paths
            .iter()
            .map(|path| {
                let tags = path.tags().map(|tag| tag.to_owned()).collect();
                (path, tags)
            })
            .enumerate()
            .collect(),
    );
    res.sort_by_key(|(i, (_, _))| *i);
    res.into_iter().map(|(_, (_, path))| path)
}

type SortedTags<'a> = Vec<(usize, (&'a TaggedPath, Vec<Intern<Tag>>))>;
fn sort_tags_by_subfrequency_(paths: &[TaggedPath]) -> SortedTags<'_> {
    fn sort_inner(paths: Partition<'_>, prefix: Vec<Intern<Tag>>) -> SortedTags<'_> {
        stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
            if let Some(tag) = tag_to_split(paths.tags_paths()) {
                let (with_tag, without_tag) = paths.partition(tag);
                debug_assert_ne!(with_tag.len(), 0);

                let with_tag_prefix = prefix.iter().cloned().chain([tag]).collect();
                let (mut xs, ys) = rayon::join(
                    || sort_inner(with_tag, with_tag_prefix),
                    || sort_inner(without_tag, prefix),
                );
                xs.extend(ys);
                xs
            } else {
                debug_assert_eq!(paths.tags_paths().len(), 0);
                paths
                    .finalize()
                    .map(|(i, path, mut inline_tags)| {
                        // Unstable sort is fine because every tag should be unique.
                        inline_tags.sort_unstable_by(|tag, other| {
                            tag.len()
                                .cmp(&other.len())
                                .reverse()
                                .then_with(|| tag.cmp(other))
                        });
                        (
                            i,
                            (path, prefix.iter().copied().chain(inline_tags).collect()),
                        )
                    })
                    .collect()
            }
        })
    }

    fn tag_to_split(tags_paths: &TagsPaths) -> Option<Intern<Tag>> {
        tags_paths
            .iter()
            .map(|(tag, tag_paths)| (tag, tag_paths.len()))
            .max_by(|(tag, count), (other_tag, other_count)| {
                count
                    .cmp(other_count)
                    .then_with(|| tag.len().cmp(&other_tag.len()))
                    .then_with(|| tag.cmp(other_tag).reverse())
            })
            .map(|(tag, _)| *tag)
    }

    sort_inner(Partition::new(paths), Default::default())
}

fn combine_<T>(
    mut paths: Vec<(usize, (&TaggedPath, Vec<T>))>,
) -> Vec<(usize, (&TaggedPath, PathBuf))>
where
    T: AsRef<TagRef>,
{
    fn combine_inner<'a, T>(
        sorted: &[(usize, (&'a TaggedPath, Vec<T>))],
        prefix: PathBuf,
        tag_index: usize,
    ) -> Vec<(usize, (&'a TaggedPath, PathBuf))>
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
                            (
                                *path,
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
                                        format!("{DIR_SEPARATOR}{TAG_IGNORE}{EXT_SEPARATOR}")
                                    },
                                    path.ext(),
                                )),
                            ),
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
                    (
                        path,
                        prefix.join(format!("{TAG_IGNORE}{EXT_SEPARATOR}{}", path.ext())),
                    ),
                ));
            } else {
                let mut ids: BTreeSet<_> = (1..=paths.len()).collect();
                let paths_ids: Vec<_> = paths
                    .iter()
                    .map(|(_, path)| {
                        path.ignored_tags()
                            // We explicitly want to remove from `ids`.
                            // `ids` should only contain ids not in `paths_ids`.
                            .filter_map(|s| s.parse().map_or(None, |x| ids.remove(&x).then_some(x)))
                            .next()
                    })
                    .collect();
                for ((orig_i, path), id) in paths.into_iter().zip(paths_ids) {
                    let id = id.unwrap_or_else(|| {
                        ids.pop_first().expect("`ids` should contain enough ids")
                    });
                    res.push((
                        orig_i,
                        (
                            path,
                            prefix.join(format!("{TAG_IGNORE}{id}{EXT_SEPARATOR}{}", path.ext())),
                        ),
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
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::testing::{tagged_path, TaggedPaths};

    use super::*;

    #[test]
    fn sort_tags_sorts_by_frequency_within_subsets() {
        assert_eq!(
            sort_tags_by_subfrequency(&[
                tagged_path("foo.x"),
                tagged_path("foo.x"),
                tagged_path("bar-foo.x"),
                tagged_path("baz-foo-bar.x"),
                tagged_path("baz.x"),
                tagged_path("baz.x"),
            ])
            .collect::<Vec<_>>(),
            [
                tagged_path("foo.x"),
                tagged_path("foo.x"),
                tagged_path("foo-bar.x"),
                tagged_path("foo-bar-baz.x"),
                tagged_path("baz.x"),
                tagged_path("baz.x"),
            ]
        )
    }

    #[test]
    fn sort_tags_breaks_ties_in_favor_of_increasing_length() {
        assert_eq!(
            sort_tags_by_subfrequency(&[tagged_path("a-bb.x")]).collect::<Vec<_>>(),
            [tagged_path("bb-a.x")]
        )
    }

    #[proptest]
    fn sort_tags_is_idempotent(paths: TaggedPaths) {
        let expected = sort_tags_by_subfrequency(&paths.0).collect::<Vec<_>>();
        assert_eq!(
            sort_tags_by_subfrequency(&expected).collect::<Vec<_>>(),
            expected
        )
    }

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
    fn combine_adds_ignored_tag() {
        assert_eq!(
            combine(&[tagged_path("_foo.x")]).collect::<Vec<_>>(),
            [PathBuf::from("_.x")]
        );
    }

    #[test]
    fn combine_adds_different_ignored_tags_to_multiple_with_same_extension() {
        assert_eq!(
            combine(&[tagged_path("_foo.x"), tagged_path("_foo.x")]).collect::<Vec<_>>(),
            [PathBuf::from("_1.x"), PathBuf::from("_2.x")]
        );
    }

    #[test]
    fn combine_adds_same_ignored_tag_to_multiple_with_different_extensions() {
        assert_eq!(
            combine(&[tagged_path("_foo.x"), tagged_path("_foo.y")]).collect::<Vec<_>>(),
            [PathBuf::from("_.x"), PathBuf::from("_.y")]
        );
    }

    #[test]
    fn combine_adds_ignored_tag_after_slash() {
        assert_eq!(
            combine(&[tagged_path("a.x"), tagged_path("a-b.x"),]).collect::<Vec<_>>(),
            [PathBuf::from("a/_.x"), PathBuf::from("a/b.x")]
        );
    }

    #[test]
    fn combine_adds_ignored_tag_after_slash_from_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        assert_eq!(
            combine(&[tagged_path(format!("{a}-{b}.{c}"))]).collect::<Vec<_>>(),
            [PathBuf::from(format!("{a}-{b}/_.{c}"))]
        );
    }

    #[test]
    fn combine_reuses_ignored_tags() {
        assert_eq!(
            combine(&[tagged_path("_1.x"), tagged_path("_2.x")]).collect::<Vec<_>>(),
            [PathBuf::from("_1.x"), PathBuf::from("_2.x")]
        );
        assert_eq!(
            combine(&[tagged_path("_2.x"), tagged_path("_1.x")]).collect::<Vec<_>>(),
            [PathBuf::from("_2.x"), PathBuf::from("_1.x")]
        );
    }

    #[proptest]
    fn combine_does_not_change_tags(paths: TaggedPaths) {
        prop_assert_eq!(
            combine(&paths.0)
                .map(|path| TaggedPath::from_path(path)
                    .unwrap()
                    .tags()
                    .map(|tag| tag.to_owned())
                    .collect_vec())
                .collect_vec(),
            paths
                .into_iter()
                .map(|path| path.tags().map(|tag| tag.to_owned()).collect_vec())
                .collect_vec()
        );
    }

    #[proptest]
    fn combine_does_not_change_extensions(paths: TaggedPaths) {
        prop_assert_eq!(
            combine(&paths.0)
                .map(|path| TaggedPath::from_path(path).unwrap().ext().to_owned())
                .collect_vec(),
            paths
                .into_iter()
                .map(|path| path.ext().to_owned())
                .collect_vec()
        );
    }
}

mod partition {
    use std::mem::replace;

    use internment::Intern;
    use itertools::{Either, Itertools};
    use rayon::prelude::*;
    use rustc_hash::{FxHashMap, FxHashSet};
    use smallvec::SmallVec;

    use crate::{Tag, TaggedPath};

    #[derive(Debug)]
    pub struct Partition<'a> {
        paths: &'a [TaggedPath],
        tags_paths: TagsPaths<'a>,
        paths_tags: PathsTags<'a>,
        done_paths: DonePaths<'a>,
    }

    pub type TagsPaths<'a> = FxHashMap<Intern<Tag>, FxHashSet<usize>>;
    type PathsTags<'a> = FxHashMap<usize, PathTags>;

    #[derive(Debug, Default)]
    struct PathTags {
        unused_tags: FxHashSet<Intern<Tag>>,
        inline_tags: SmallVec<[Intern<Tag>; 1]>,
    }

    type DonePaths<'a> = Vec<(usize, SmallVec<[Intern<Tag>; 1]>)>;

    impl<'a> Partition<'a> {
        pub fn new(paths: &'a [TaggedPath]) -> Self {
            let (paths_tags, done_paths): (PathsTags, _) =
                paths.iter().enumerate().partition_map(|(i, path)| {
                    if path.tags_is_empty() {
                        Either::Right((i, SmallVec::new()))
                    } else {
                        Either::Left((
                            i,
                            PathTags {
                                unused_tags: path.tags().map(Intern::from_ref).collect(),
                                inline_tags: Default::default(),
                            },
                        ))
                    }
                });

            let mut tags_paths: TagsPaths = Default::default();
            for (path, tags) in &paths_tags {
                for tag in &tags.unused_tags {
                    tags_paths.entry(*tag).or_default().insert(*path);
                }
            }

            let mut this = Self {
                paths,
                tags_paths,
                paths_tags,
                done_paths,
            };
            this.extract_inline_tags();

            this.validate();

            this
        }

        /// Return (with_tag, without_tag).
        pub fn partition(mut self, tag: Intern<Tag>) -> (Self, Self) {
            let paths = self.tags_paths.remove(&tag).unwrap();

            let (mut with_tag, mut without_tag) = if paths.len() > self.unused_len() / 2 {
                self._partition_reverse(tag, paths)
            } else {
                self._partition(tag, paths)
            };

            with_tag.extract_inline_tags();
            without_tag.extract_inline_tags();

            with_tag.validate();
            without_tag.validate();

            (with_tag, without_tag)
        }

        fn _partition(self, tag: Intern<Tag>, paths: FxHashSet<usize>) -> (Self, Self) {
            let mut with_tag = Self {
                paths: self.paths,
                tags_paths: Default::default(),
                paths_tags: Default::default(),
                done_paths: Default::default(),
            };
            let mut without_tag = self;

            for path in paths {
                let mut path_tags = without_tag.paths_tags.remove(&path).unwrap();
                if path_tags.unused_tags.len() == 1 {
                    debug_assert!(path_tags.unused_tags.contains(&tag));
                    with_tag.done_paths.push((path, path_tags.inline_tags));
                } else {
                    path_tags.unused_tags.remove(&tag);
                    for tag in &path_tags.unused_tags {
                        with_tag.tags_paths.entry(*tag).or_default().insert(path);
                        let tag_paths = without_tag.tags_paths.get_mut(tag).unwrap();
                        if tag_paths.len() == 1 {
                            without_tag.tags_paths.remove(tag);
                        } else {
                            tag_paths.remove(&path);
                        }
                    }
                    with_tag.paths_tags.insert(path, path_tags);
                }
            }

            (with_tag, without_tag)
        }

        fn _partition_reverse(mut self, tag: Intern<Tag>, paths: FxHashSet<usize>) -> (Self, Self) {
            let paths_without = self
                .paths_tags
                .par_iter()
                .filter(|(_, path_tags)| !path_tags.unused_tags.contains(&tag))
                .map(|(path, _)| path)
                .copied()
                .collect::<Vec<_>>();

            let mut done_paths = DonePaths::default();
            for path in paths {
                let path_tags = self.paths_tags.get_mut(&path).unwrap();
                if path_tags.unused_tags.len() == 1 {
                    debug_assert!(path_tags.unused_tags.contains(&tag));
                    done_paths.push((path, self.paths_tags.remove(&path).unwrap().inline_tags));
                } else {
                    path_tags.unused_tags.remove(&tag);
                }
            }

            let mut without_tag = Self {
                paths: self.paths,
                tags_paths: Default::default(),
                paths_tags: Default::default(),
                done_paths: replace(&mut self.done_paths, done_paths),
            };
            let mut with_tag = self;

            for path in paths_without {
                let path_tags = with_tag.paths_tags.remove(&path).unwrap();
                for tag in &path_tags.unused_tags {
                    if let Some(tag_paths) = with_tag.tags_paths.get_mut(tag) {
                        without_tag.tags_paths.entry(*tag).or_default().insert(path);
                        if tag_paths.len() == 1 {
                            with_tag.tags_paths.remove(tag);
                        } else {
                            tag_paths.remove(&path);
                        }
                    }
                }
                without_tag.paths_tags.insert(path, path_tags);
            }

            (with_tag, without_tag)
        }

        fn extract_inline_tags(&mut self) {
            self.tags_paths.retain(|tag, paths| {
                if paths.len() == 1 {
                    let path = paths.iter().next().unwrap();
                    let path_tags = self.paths_tags.get_mut(path).unwrap();
                    path_tags.inline_tags.push(*tag);
                    if path_tags.unused_tags.len() == 1 {
                        debug_assert!(path_tags.unused_tags.contains(tag));
                        self.done_paths
                            .push((*path, self.paths_tags.remove(path).unwrap().inline_tags));
                    } else {
                        path_tags.unused_tags.remove(tag);
                    }
                    false
                } else {
                    true
                }
            });
        }

        fn validate(&self) {
            debug_assert_eq!(
                self.paths_tags.len(),
                self.tags_paths.values().flatten().unique().count()
            );
            debug_assert!(!self
                .done_paths
                .iter()
                .any(|(path, _)| self.paths_tags.contains_key(path)))
        }

        pub fn len(&self) -> usize {
            self.paths_tags.len() + self.done_paths.len()
        }

        fn unused_len(&self) -> usize {
            self.paths_tags.len()
        }

        pub fn tags_paths(&'_ self) -> &'_ TagsPaths<'_> {
            &self.tags_paths
        }

        pub fn finalize(
            self,
        ) -> impl Iterator<Item = (usize, &'a TaggedPath, SmallVec<[Intern<Tag>; 1]>)> {
            self.done_paths
                .into_iter()
                .map(|(i, tags)| (i, self.paths.get(i).unwrap(), tags))
        }
    }
}
