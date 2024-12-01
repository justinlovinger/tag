use internment::Intern;
use itertools::Itertools;

use crate::{
    tagged_filesystem::MoveOp, Tag, TaggedPath, DIR_SEPARATOR, INLINE_SEPARATOR, PATH_PART_MAX_LEN,
    TAG_END,
};

use self::partition::{Partition, TagsPaths};

type Prefix = Vec<(Intern<Tag>, usize)>; // (tag, count)

pub(crate) fn organize(paths: &[TaggedPath]) -> Vec<MoveOp> {
    _organize(Partition::new(paths), Default::default())
}

fn _organize(paths: Partition, prefix: Prefix) -> Vec<MoveOp> {
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        if let Some(tag) = tag_to_split(paths.tags_paths()) {
            let (with_tag, without_tag) = paths.partition(tag);
            debug_assert_ne!(with_tag.len(), 0);

            let with_tag_prefix = prefix
                .iter()
                .cloned()
                .chain([(tag, with_tag.len())])
                .collect();
            let (mut xs, ys) = rayon::join(
                || _organize(with_tag, with_tag_prefix),
                || _organize(without_tag, prefix),
            );
            xs.extend(ys);
            xs
        } else {
            debug_assert_eq!(paths.tags_paths().len(), 0);
            to_move_ops(paths, prefix)
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

fn to_move_ops(paths: Partition, prefix: Prefix) -> Vec<MoveOp> {
    let prefix = {
        let separators = prefix
            .iter()
            .map(|(tag, count)| (tag.len(), count))
            .tuple_windows()
            .map({
                let mut len = 0;
                move |((tag_len, count), (next_len, next_count))| {
                    if count == next_count
                        && len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len
                            <= PATH_PART_MAX_LEN
                    {
                        len += tag_len + INLINE_SEPARATOR.len_utf8();
                        INLINE_SEPARATOR
                    } else {
                        len = 0;
                        DIR_SEPARATOR
                    }
                }
            })
            .chain([DIR_SEPARATOR]);
        format!(
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
        )
    };

    paths
        .finalize()
        .map(|(path, mut inline_tags)| {
            let inline_tags = {
                inline_tags.sort_unstable_by(|tag, other| {
                    tag.len()
                        .cmp(&other.len())
                        .reverse()
                        .then_with(|| tag.cmp(other))
                });
                let separators = inline_tags
                    .iter()
                    .map(|tag| tag.len())
                    .chain([TAG_END.len_utf8() + path.name().len()])
                    .tuple_windows()
                    .map({
                        let mut len = 0;
                        move |(tag_len, next_len)| {
                            if len + tag_len + INLINE_SEPARATOR.len_utf8() + next_len
                                <= PATH_PART_MAX_LEN
                            {
                                len += tag_len + INLINE_SEPARATOR.len_utf8();
                                INLINE_SEPARATOR
                            } else {
                                len = 0;
                                DIR_SEPARATOR
                            }
                        }
                    });
                inline_tags
                    .iter()
                    .zip(separators)
                    .format_with("", |(tag, sep), f| {
                        f(&tag)?;
                        f(&sep)?;
                        Ok(())
                    })
            };

            (
                format!("{}{}{TAG_END}{}", &prefix, inline_tags, path.name()).into(),
                path,
            )
        })
        .filter(|(to, path)| path.as_path() != to)
        .map(|(to, path)| MoveOp {
            to,
            from: path.as_path().into(),
        })
        .collect()
}

mod partition {
    use std::mem::replace;

    use by_address::ByThinAddress;
    use internment::Intern;
    use itertools::Itertools;
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
            let paths_tags: PathsTags = paths
                .iter()
                .map(ByThinAddress)
                .filter(|path| !path.tags_is_empty()) // Paths with no tags will never move.
                .map(|path| {
                    (
                        path,
                        PathTags {
                            unused_tags: path.tags().map(Intern::from_ref).collect(),
                            inline_tags: Default::default(),
                        },
                    )
                })
                .collect();

            let mut tags_paths: TagsPaths = Default::default();
            for (path, tags) in &paths_tags {
                for tag in &tags.unused_tags {
                    tags_paths.entry(*tag).or_default().insert(*path);
                }
            }

            let mut this = Self {
                tags_paths,
                paths_tags,
                done_paths: Default::default(),
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

        pub fn tags_paths(&self) -> &TagsPaths {
            &self.tags_paths
        }

        pub fn finalize(
            self,
        ) -> impl Iterator<Item = (&'a TaggedPath, SmallVec<[Intern<Tag>; 1]>)> {
            self.done_paths.into_iter()
        }
    }
}
