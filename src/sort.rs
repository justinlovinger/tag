use internment::Intern;
use itertools::Itertools;

use crate::{ExtRef, Tag, TagRef, TaggedPath, EXT_SEPARATOR, INLINE_SEPARATOR, TAG_IGNORE};

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
