use std::rc::Rc;

use internment::Intern;
use itertools::Itertools;

use crate::{types::MoveOp, Tag, TaggedFile, DIR_SEPARATOR, INLINE_SEPARATOR, TAG_END};

use self::files::{Files, TagsFiles};

type Prefix = Vec<(Intern<Tag>, usize)>; // (tag, count)

pub fn organize(files: &[TaggedFile]) -> impl Iterator<Item = MoveOp> + '_ {
    _organize(Files::new(files), Default::default())
}

fn _organize(mut files: Files, prefix: Prefix) -> impl Iterator<Item = MoveOp> + '_ {
    let mut moves = Vec::new();
    loop {
        if let Some((tag, count)) = tag_to_split(files.tags_files()) {
            debug_assert_ne!(count, 0);

            let (with_tag, without_tag) = files.partition(tag);
            files = without_tag;

            moves.extend(_organize(
                with_tag,
                prefix.iter().cloned().chain([(tag, count)]).collect(),
            ));
        } else {
            debug_assert_eq!(files.tags_files().len(), 0);
            return moves.into_iter().chain(to_move_ops(files, prefix));
        }
    }
}

fn tag_to_split(tags_files: &TagsFiles) -> Option<(Intern<Tag>, usize)> {
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

fn to_move_ops(files: Files, prefix: Prefix) -> impl Iterator<Item = MoveOp> + '_ {
    let separators = prefix
        .iter()
        .map(|(_, count)| count)
        .tuple_windows()
        .map(|(count, next_count)| {
            if count == next_count {
                INLINE_SEPARATOR
            } else {
                DIR_SEPARATOR
            }
        })
        .chain([DIR_SEPARATOR]);
    let prefix = Rc::new(format!(
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
    ));
    let (untagged_files, inline_tags) = files.finalize();
    untagged_files
        .map({
            let prefix = Rc::clone(&prefix);
            move |file| (format!("{}{TAG_END}{}", &prefix, file.name()).into(), file)
        })
        .chain(inline_tags.map(move |(file, mut tags)| {
            tags.sort_unstable_by(|tag, other| {
                tag.len()
                    .cmp(&other.len())
                    .reverse()
                    .then_with(|| tag.cmp(other))
            });
            (
                format!(
                    "{}{}{TAG_END}{}",
                    &prefix,
                    tags.into_iter().format_with("", |tag, f| {
                        f(&tag)?;
                        f(&INLINE_SEPARATOR)?;
                        Ok(())
                    }),
                    file.name()
                )
                .into(),
                file,
            )
        }))
        .filter(|(to, file)| file.as_path() != to)
        .map(|(to, file)| MoveOp {
            to,
            from: file.as_path().into(),
        })
}

mod files {
    use std::{
        collections::{HashMap, HashSet},
        mem::replace,
    };

    use by_address::ByThinAddress;
    use internment::Intern;
    use itertools::Itertools;
    use smallvec::SmallVec;

    use crate::{Tag, TaggedFile};

    #[derive(Debug)]
    pub struct Files<'a> {
        tags_files: TagsFiles<'a>,
        files_tags: FilesTags<'a>,
        done_files: DoneFiles<'a>,
    }

    pub type TagsFiles<'a> = HashMap<Intern<Tag>, HashSet<ByThinAddress<&'a TaggedFile>>>;
    type FilesTags<'a> = HashMap<ByThinAddress<&'a TaggedFile>, FileTags>;

    #[derive(Debug, Default)]
    struct FileTags {
        unused_tags: HashSet<Intern<Tag>>,
        inline_tags: SmallVec<[Intern<Tag>; 1]>,
    }

    #[derive(Debug, Default)]
    struct DoneFiles<'a> {
        untagged: DoneUntagged<'a>,
        with_inline: DoneWithInline<'a>,
    }

    type DoneUntagged<'a> = Vec<&'a TaggedFile>;
    type DoneWithInline<'a> = Vec<(&'a TaggedFile, SmallVec<[Intern<Tag>; 1]>)>;

    impl<'a> Files<'a> {
        pub fn new(files: &'a [TaggedFile]) -> Self {
            let mut tags_files: TagsFiles = HashMap::new();
            let files_tags: FilesTags = HashMap::from_iter(
                files
                    .iter()
                    .map(ByThinAddress)
                    .filter(|file| !file.tags_is_empty()) // Files with no tags will never move.
                    .map(|file| {
                        (
                            file,
                            FileTags {
                                unused_tags: file
                                    .tags()
                                    .map(|tag| Intern::new(tag.to_owned()))
                                    .collect(),
                                inline_tags: Default::default(),
                            },
                        )
                    }),
            );
            for (file, tags) in &files_tags {
                for tag in &tags.unused_tags {
                    tags_files.entry(*tag).or_default().insert(*file);
                }
            }
            let mut this = Self {
                tags_files,
                files_tags,
                done_files: Default::default(),
            };
            this.extract_inline_tags();
            this
        }

        /// Return (with_tag, without_tag).
        pub fn partition(mut self, tag: Intern<Tag>) -> (Self, Self) {
            let files = self.tags_files.remove(&tag).unwrap();

            let (mut with_tag, mut without_tag) = if files.len() > self.files_tags.len() / 2 {
                let files_without = self
                    .files_tags
                    .iter()
                    .filter(|(_, file_tags)| !file_tags.unused_tags.contains(&tag))
                    .map(|(file, _)| file)
                    .copied()
                    .collect_vec();

                let mut done_files = DoneFiles::default();
                for file in files {
                    let file_tags = self.files_tags.get_mut(&file).unwrap();
                    debug_assert!(file_tags.unused_tags.contains(&tag));
                    if file_tags.unused_tags.len() == 1 {
                        let file_tags = self.files_tags.remove(&file).unwrap();
                        if file_tags.inline_tags.is_empty() {
                            done_files.untagged.push(file.0);
                        } else {
                            done_files.with_inline.push((file.0, file_tags.inline_tags));
                        }
                    } else {
                        file_tags.unused_tags.remove(&tag);
                    }
                }

                let mut without_tag = Self {
                    tags_files: HashMap::new(),
                    files_tags: HashMap::new(),
                    done_files: replace(&mut self.done_files, done_files),
                };
                let mut with_tag = self;

                for file in files_without {
                    let file_tags = with_tag.files_tags.remove(&file).unwrap();
                    debug_assert!(!file_tags.unused_tags.contains(&tag));
                    for tag in &file_tags.unused_tags {
                        if let Some(tag_files) = with_tag.tags_files.get_mut(tag) {
                            without_tag.tags_files.entry(*tag).or_default().insert(file);
                            if tag_files.len() == 1 {
                                with_tag.tags_files.remove(tag);
                            } else {
                                tag_files.remove(&file);
                            }
                        }
                    }
                    without_tag.files_tags.insert(file, file_tags);
                }

                (with_tag, without_tag)
            } else {
                let mut with_tag = Self {
                    tags_files: HashMap::new(),
                    files_tags: HashMap::new(),
                    done_files: Default::default(),
                };
                let mut without_tag = self;

                for file in files {
                    let mut file_tags = without_tag.files_tags.remove(&file).unwrap();
                    debug_assert!(file_tags.unused_tags.contains(&tag));
                    if file_tags.unused_tags.len() == 1 {
                        if file_tags.inline_tags.is_empty() {
                            with_tag.done_files.untagged.push(file.0);
                        } else {
                            with_tag
                                .done_files
                                .with_inline
                                .push((file.0, file_tags.inline_tags));
                        }
                    } else {
                        file_tags.unused_tags.remove(&tag);
                        for tag in &file_tags.unused_tags {
                            with_tag.tags_files.entry(*tag).or_default().insert(file);
                            let tag_files = without_tag.tags_files.get_mut(tag).unwrap();
                            if tag_files.len() == 1 {
                                without_tag.tags_files.remove(tag);
                            } else {
                                tag_files.remove(&file);
                            }
                        }
                        with_tag.files_tags.insert(file, file_tags);
                    }
                }

                (with_tag, without_tag)
            };

            with_tag.extract_inline_tags();
            without_tag.extract_inline_tags();

            debug_assert_eq!(
                with_tag.files_tags.len(),
                with_tag.tags_files.values().flatten().unique().count()
            );
            debug_assert_eq!(
                without_tag.files_tags.len(),
                without_tag.tags_files.values().flatten().unique().count()
            );

            (with_tag, without_tag)
        }

        fn extract_inline_tags(&mut self) {
            self.tags_files.retain(|tag, files| {
                if files.len() == 1 {
                    let file = files.iter().next().unwrap();
                    let file_tags = self.files_tags.get_mut(file).unwrap();
                    file_tags.inline_tags.push(*tag);
                    if file_tags.unused_tags.len() == 1 {
                        let file_tags = self.files_tags.remove(file).unwrap();
                        self.done_files
                            .with_inline
                            .push((file.0, file_tags.inline_tags));
                    } else {
                        file_tags.unused_tags.remove(tag);
                    }
                    false
                } else {
                    true
                }
            });
        }

        pub fn tags_files(&self) -> &TagsFiles {
            &self.tags_files
        }

        pub fn finalize(
            self,
        ) -> (
            impl Iterator<Item = &'a TaggedFile>,
            impl Iterator<Item = (&'a TaggedFile, SmallVec<[Intern<Tag>; 1]>)>,
        ) {
            (
                self.done_files.untagged.into_iter(),
                self.done_files.with_inline.into_iter(),
            )
        }
    }
}
