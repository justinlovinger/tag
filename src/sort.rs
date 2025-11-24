use std::marker::Sync;

use internment::Intern;

use crate::{Tag, TaggedPath, TaggedPathRef};

use self::partition::{Partition, TagsPaths};

/// Sort tags by subfrequency
///
/// Paths are returned in the order given.
///
/// Subfrequency is frequency calculated within each subset
/// formed by selecting the next tag.
/// For example,
/// in `foo-baz-bar.x foo.x foo.x foo-bar.x baz.x baz.x`,
/// `foo-baz-bar.x` is sorted as `foo-bar-baz.x`
/// because `bar` is more common than `baz`
/// within the subset of paths containing `foo`,
/// even though `baz` is more common globally.
pub fn sort_tags_by_subfrequency<P>(paths: &[P]) -> impl Iterator<Item = TaggedPath>
where
    P: AsRef<TaggedPathRef> + Sync,
{
    let mut res = sort_tags_by_subfrequency_(paths);
    res.sort_by_key(|(i, _)| *i);
    res.into_iter().map(|(_, path)| path)
}

fn sort_tags_by_subfrequency_<P>(paths: &[P]) -> Vec<(usize, TaggedPath)>
where
    P: AsRef<TaggedPathRef> + Sync,
{
    fn sort_inner<P>(
        paths: &[P],
        subpaths: Partition,
        prefix: Vec<Intern<Tag>>,
    ) -> Vec<(usize, TaggedPath)>
    where
        P: AsRef<TaggedPathRef> + Sync,
    {
        stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
            if let Some(tag) = tag_to_split(subpaths.tags_paths()) {
                let (with_tag, without_tag) = subpaths.partition(tag);
                debug_assert_ne!(with_tag.len(), 0);

                let with_tag_prefix = prefix.iter().cloned().chain([tag]).collect();
                let (mut xs, ys) = rayon::join(
                    || sort_inner(paths, with_tag, with_tag_prefix),
                    || sort_inner(paths, without_tag, prefix),
                );
                xs.extend(ys);
                xs
            } else {
                debug_assert_eq!(subpaths.tags_paths().len(), 0);
                subpaths
                    .finalize()
                    .map(|(i, mut inline_tags)| {
                        // Unstable sort is fine
                        // because the comparison function only returns items equal
                        // when actually equal.
                        inline_tags.sort_unstable_by(|tag, other| {
                            tag.len()
                                .cmp(&other.len())
                                .reverse()
                                .then_with(|| tag.cmp(other))
                        });
                        (
                            i,
                            TaggedPath::from_tags(
                                prefix.iter().copied().chain(inline_tags),
                                paths.get(i).unwrap().as_ref().ext(),
                            ),
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

    sort_inner(paths, Partition::new(paths), Default::default())
}

mod partition {
    use std::mem::replace;

    use internment::Intern;
    use itertools::{Either, Itertools};
    use rayon::prelude::*;
    use rustc_hash::{FxHashMap, FxHashSet};
    use smallvec::SmallVec;

    use crate::{Tag, TaggedPathRef};

    #[derive(Debug)]
    pub struct Partition {
        tags_paths: TagsPaths,
        paths_tags: PathsTags,
        done_paths: DonePaths,
    }

    pub type TagsPaths = FxHashMap<Intern<Tag>, FxHashSet<usize>>;
    type PathsTags = FxHashMap<usize, PathTags>;

    #[derive(Debug, Default)]
    struct PathTags {
        unused_tags: FxHashSet<Intern<Tag>>,
        inline_tags: SmallVec<[Intern<Tag>; 1]>,
    }

    type DonePaths = Vec<(usize, SmallVec<[Intern<Tag>; 1]>)>;

    impl Partition {
        pub fn new<P>(paths: &[P]) -> Self
        where
            P: AsRef<TaggedPathRef>,
        {
            let (paths_tags, done_paths): (PathsTags, _) =
                paths.iter().enumerate().partition_map(|(i, path)| {
                    let path = path.as_ref();
                    if path.tags().next().is_none() {
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

        pub fn tags_paths(&self) -> &TagsPaths {
            &self.tags_paths
        }

        pub fn finalize(self) -> impl Iterator<Item = (usize, SmallVec<[Intern<Tag>; 1]>)> {
            self.done_paths.into_iter()
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
        prop_assert_eq!(
            sort_tags_by_subfrequency(&expected).collect::<Vec<_>>(),
            expected
        )
    }
}
