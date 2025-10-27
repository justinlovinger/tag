use std::{collections::BTreeSet, path::PathBuf};

use internment::Intern;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{
    tagged_filesystem::MoveOp, Tag, TagRef, TaggedPath, DIR_SEPARATOR, EXT_SEPARATOR,
    INLINE_SEPARATOR, PATH_PART_MAX_LEN, TAG_IGNORE,
};

use self::partition::{Partition, TagsPaths};

pub(crate) fn organize(paths: &[TaggedPath]) -> Vec<MoveOp> {
    combine(sort(paths))
        .into_iter()
        .filter_map(|(path, to)| {
            (path.as_path() != to).then(|| MoveOp {
                from: path.as_path().to_owned(),
                to,
            })
        })
        .collect()
}

fn sort(paths: &[TaggedPath]) -> Vec<(&TaggedPath, Vec<Intern<Tag>>)> {
    fn sort_inner(
        paths: Partition<'_>,
        prefix: Vec<Intern<Tag>>,
    ) -> Vec<(&'_ TaggedPath, Vec<Intern<Tag>>)> {
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
                    .map(|(path, mut inline_tags)| {
                        // Unstable sort is fine because every tag should be unique.
                        inline_tags.sort_unstable_by(|tag, other| {
                            tag.len()
                                .cmp(&other.len())
                                .reverse()
                                .then_with(|| tag.cmp(other))
                        });
                        (path, prefix.iter().copied().chain(inline_tags).collect())
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

fn combine<T>(mut tags: Vec<(&TaggedPath, Vec<T>)>) -> Vec<(&TaggedPath, PathBuf)>
where
    T: AsRef<TagRef>,
{
    fn combine_inner<'a, T>(
        sorted: &[(&'a TaggedPath, Vec<T>)],
        prefix: PathBuf,
        tag_index: usize,
    ) -> Vec<(&'a TaggedPath, PathBuf)>
    where
        T: AsRef<TagRef>,
    {
        let mut res = Vec::new();
        let mut i = 0;
        let mut without_tags: FxHashMap<_, Vec<_>> = FxHashMap::default();
        while let Some((path, tags)) = sorted.get(i) {
            match tags.get(tag_index) {
                Some(tag) => {
                    let j = sorted
                        .iter()
                        .enumerate()
                        .skip(i + 1)
                        .find(|(_, (_, tags))| {
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
                        ));
                    } else {
                        let (_, tags_of_last) = &sorted[j - 1];
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
                    without_tags.entry(path.ext()).or_default().push(*path);
                    i += 1;
                }
            }
        }

        for paths in without_tags.into_values() {
            if paths.len() == 1 {
                let path = paths.into_iter().next().unwrap();
                res.push((
                    path,
                    prefix.join(format!("{TAG_IGNORE}{EXT_SEPARATOR}{}", path.ext())),
                ));
            } else {
                let mut ids: BTreeSet<_> = (1..=paths.len()).collect();
                let paths_ids: Vec<_> = paths
                    .iter()
                    .map(|path| {
                        path.ignored_tags()
                            // We explicitly want to remove from `ids`.
                            // `ids` should only contain ids not in `paths_ids`.
                            .filter_map(|s| s.parse().map_or(None, |x| ids.remove(&x).then_some(x)))
                            .next()
                    })
                    .collect();
                for (path, id) in paths.into_iter().zip(paths_ids) {
                    let id = id.unwrap_or_else(|| {
                        ids.pop_first().expect("`ids` should contain enough ids")
                    });
                    res.push((
                        path,
                        prefix.join(format!("{TAG_IGNORE}{id}{EXT_SEPARATOR}{}", path.ext())),
                    ));
                }
            }
        }

        res
    }

    tags.sort_by(|(_, tags), (_, other)| {
        tags.iter()
            .map(|tag| tag.as_ref())
            .cmp(other.iter().map(|tag| tag.as_ref()))
            // Reversing the order places items with fewer tags at the end,
            // which simplifies checking how many items have no more tags
            // at a given recursive step.
            .reverse()
    });
    combine_inner(&tags, PathBuf::new(), 0)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rustc_hash::FxHashSet;
    use test_strategy::proptest;

    use crate::testing::{ext, tag, TaggedPaths};

    use super::*;

    #[test]
    fn combine_separates_different_prefixes_by_inline() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (&paths[0], vec![tag("bar"), tag("foo")]),
                (&paths[1], vec![tag("baz"), tag("bin")]),
            ]),
            vec![
                (&paths[1], PathBuf::from("baz-bin.x")),
                (&paths[0], PathBuf::from("bar-foo.x")),
            ]
        );
    }

    #[test]
    fn combine_separates_common_prefixes_by_dir() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (&paths[0], vec![tag("foo"), tag("bar")]),
                (&paths[1], vec![tag("foo"), tag("baz")]),
            ]),
            vec![
                (&paths[1], PathBuf::from("foo/baz.x")),
                (&paths[0], PathBuf::from("foo/bar.x")),
            ]
        );
    }

    #[test]
    fn combine_separates_by_dir_and_inline() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (
                    &paths[0],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("bar").unwrap(),
                        Tag::new("baz").unwrap(),
                    ]
                ),
                (
                    &paths[1],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("baz").unwrap(),
                        Tag::new("bar").unwrap(),
                    ]
                )
            ]),
            vec![
                (&paths[1], PathBuf::from("foo/baz-bar.x")),
                (&paths[0], PathBuf::from("foo/bar-baz.x")),
            ]
        );
    }

    #[test]
    fn combine_uses_inline_in_common_prefixes() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (
                    &paths[0],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("bar").unwrap(),
                        Tag::new("baz").unwrap(),
                    ]
                ),
                (
                    &paths[1],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("bar").unwrap(),
                        Tag::new("bin").unwrap(),
                    ]
                )
            ]),
            vec![
                (&paths[1], PathBuf::from("foo-bar/bin.x")),
                (&paths[0], PathBuf::from("foo-bar/baz.x")),
            ]
        );
    }

    #[test]
    fn combine_does_not_use_inline_in_common_prefix_if_not_all_common() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (
                    &paths[0],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("bar").unwrap(),
                        Tag::new("baz").unwrap(),
                    ]
                ),
                (
                    &paths[1],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new("bar").unwrap(),
                        Tag::new("bin").unwrap(),
                    ]
                ),
                (
                    &paths[2],
                    vec![Tag::new("foo").unwrap(), Tag::new("bin").unwrap()]
                ),
            ]),
            vec![
                (&paths[2], PathBuf::from("foo/bin.x")),
                (&paths[1], PathBuf::from("foo/bar/bin.x")),
                (&paths[0], PathBuf::from("foo/bar/baz.x")),
            ]
        );
    }

    #[test]
    fn combine_uses_dir_in_long_common_prefixes() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (
                    &paths[0],
                    vec![
                        Tag::new(&a).unwrap(),
                        Tag::new(&b).unwrap(),
                        Tag::new(&c).unwrap(),
                        Tag::new("baz").unwrap(),
                    ]
                ),
                (
                    &paths[1],
                    vec![
                        Tag::new(&a).unwrap(),
                        Tag::new(&b).unwrap(),
                        Tag::new(&c).unwrap(),
                        Tag::new("bin").unwrap(),
                    ]
                )
            ]),
            vec![
                (&paths[1], PathBuf::from(format!("{a}-{b}/{c}/bin.x"))),
                (&paths[0], PathBuf::from(format!("{a}-{b}/{c}/baz.x"))),
            ]
        );
    }

    #[test]
    fn combine_uses_dir_in_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (&paths[0], vec![Tag::new("foo").unwrap()]),
                (
                    &paths[1],
                    vec![
                        Tag::new("foo").unwrap(),
                        Tag::new(&a).unwrap(),
                        Tag::new(&b).unwrap(),
                        Tag::new(&c).unwrap(),
                    ]
                ),
            ]),
            vec![
                (&paths[1], PathBuf::from(format!("foo/{a}-{b}/{c}.x"))),
                (&paths[0], PathBuf::from("foo/_.x")),
            ]
        );
    }

    #[test]
    fn combine_adds_ignored_tag() {
        let paths = [TaggedPath::from_tags::<Tag, _>(
            &FxHashSet::default(),
            ext("x"),
        )];
        assert_eq!(
            combine::<Tag>(vec![(&paths[0], vec![])]),
            vec![(&paths[0], PathBuf::from("_.x")),]
        );
    }

    #[test]
    fn combine_adds_different_ignored_tags_to_multiple_with_same_extension() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine::<Tag>(vec![(&paths[0], vec![]), (&paths[1], vec![]),]),
            vec![
                (&paths[0], PathBuf::from("_1.x")),
                (&paths[1], PathBuf::from("_2.x")),
            ]
        );
    }

    #[test]
    fn combine_adds_same_ignored_tag_to_multiple_with_different_extensions() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("y")),
        ];
        assert_eq!(
            combine::<Tag>(vec![(&paths[0], vec![]), (&paths[1], vec![]),]),
            vec![
                (&paths[0], PathBuf::from("_.x")),
                (&paths[1], PathBuf::from("_.y")),
            ]
        );
    }

    #[test]
    fn combine_adds_ignored_tag_after_slash() {
        let paths = [
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")),
        ];
        assert_eq!(
            combine(vec![
                (&paths[0], vec![Tag::new("a").unwrap()]),
                (
                    &paths[1],
                    vec![Tag::new("a").unwrap(), Tag::new("b").unwrap()]
                ),
            ]),
            vec![
                (&paths[1], PathBuf::from("a/b.x")),
                (&paths[0], PathBuf::from("a/_.x")),
            ]
        );
    }

    #[test]
    fn combine_adds_ignored_tag_after_slash_from_long_inline() {
        let a = "a".repeat(100);
        let b = "b".repeat(100);
        let c = "c".repeat(100);
        let paths = [TaggedPath::from_tags::<Tag, _>(
            &FxHashSet::default(),
            ext(&c),
        )];
        assert_eq!(
            combine(vec![(
                &paths[0],
                vec![Tag::new(&a).unwrap(), Tag::new(&b).unwrap()]
            )]),
            vec![(&paths[0], PathBuf::from(format!("{a}-{b}/_.{c}")))]
        );
    }

    #[test]
    fn combine_reuses_ignored_tags() {
        let paths = [
            TaggedPath::new("_1.x").unwrap(),
            TaggedPath::new("_2.x").unwrap(),
        ];
        assert_eq!(
            combine::<Tag>(vec![(&paths[0], vec![]), (&paths[1], vec![])]),
            vec![
                (&paths[0], PathBuf::from("_1.x")),
                (&paths[1], PathBuf::from("_2.x")),
            ]
        );
        assert_eq!(
            combine::<Tag>(vec![(&paths[1], vec![]), (&paths[0], vec![])]),
            vec![
                (&paths[1], PathBuf::from("_2.x")),
                (&paths[0], PathBuf::from("_1.x")),
            ]
        );
    }

    #[proptest]
    fn combine_does_not_change_tags(paths: TaggedPaths) {
        let paths = paths
            .0
            .iter()
            .map(|path| (path.tags().map(|tag| tag.to_owned()).collect_vec(), path))
            .map(|(tags, path)| (path, tags))
            .collect::<Vec<_>>();
        let (paths, new_paths): (Vec<_>, Vec<_>) = combine(paths).into_iter().unzip();
        prop_assert_eq!(
            new_paths
                .into_iter()
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
        let paths = paths
            .0
            .iter()
            .map(|path| (path.tags().map(|tag| tag.to_owned()).collect_vec(), path))
            .map(|(tags, path)| (path, tags))
            .collect::<Vec<_>>();
        let (paths, new_paths): (Vec<_>, Vec<_>) = combine(paths).into_iter().unzip();
        prop_assert_eq!(
            new_paths
                .into_iter()
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

    use by_address::ByThinAddress;
    use internment::Intern;
    use itertools::{Either, Itertools};
    use rayon::prelude::*;
    use rustc_hash::{FxHashMap, FxHashSet};
    use smallvec::SmallVec;

    use crate::{Tag, TaggedPath};

    #[derive(Debug)]
    pub struct Partition<'a> {
        tags_paths: TagsPaths<'a>,
        paths_tags: PathsTags<'a>,
        done_paths: DonePaths<'a>,
    }

    pub type TagsPaths<'a> = FxHashMap<Intern<Tag>, FxHashSet<ByThinAddress<&'a TaggedPath>>>;
    type PathsTags<'a> = FxHashMap<ByThinAddress<&'a TaggedPath>, PathTags>;

    #[derive(Debug, Default)]
    struct PathTags {
        unused_tags: FxHashSet<Intern<Tag>>,
        inline_tags: SmallVec<[Intern<Tag>; 1]>,
    }

    type DonePaths<'a> = Vec<(&'a TaggedPath, SmallVec<[Intern<Tag>; 1]>)>;

    impl<'a> Partition<'a> {
        pub fn new(paths: &'a [TaggedPath]) -> Self {
            let (paths_tags, done_paths): (PathsTags, _) = paths.iter().partition_map(|path| {
                if path.tags_is_empty() {
                    Either::Right((path, SmallVec::new()))
                } else {
                    Either::Left((
                        ByThinAddress(path),
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

        fn _partition(
            self,
            tag: Intern<Tag>,
            paths: FxHashSet<ByThinAddress<&'a TaggedPath>>,
        ) -> (Self, Self) {
            let mut with_tag = Self {
                tags_paths: Default::default(),
                paths_tags: Default::default(),
                done_paths: Default::default(),
            };
            let mut without_tag = self;

            for path in paths {
                let mut path_tags = without_tag.paths_tags.remove(&path).unwrap();
                if path_tags.unused_tags.len() == 1 {
                    debug_assert!(path_tags.unused_tags.contains(&tag));
                    with_tag.done_paths.push((path.0, path_tags.inline_tags));
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

        fn _partition_reverse(
            mut self,
            tag: Intern<Tag>,
            paths: FxHashSet<ByThinAddress<&'a TaggedPath>>,
        ) -> (Self, Self) {
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
                    done_paths.push((path.0, self.paths_tags.remove(&path).unwrap().inline_tags));
                } else {
                    path_tags.unused_tags.remove(&tag);
                }
            }

            let mut without_tag = Self {
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
                            .push((path.0, self.paths_tags.remove(path).unwrap().inline_tags));
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
                .any(|(path, _)| self.paths_tags.contains_key(&ByThinAddress(*path))))
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
        ) -> impl Iterator<Item = (&'a TaggedPath, SmallVec<[Intern<Tag>; 1]>)> {
            self.done_paths.into_iter()
        }
    }
}
