use std::{collections::BTreeSet, fmt, path::Path};

use filesystem::{FakeFileSystem, FileSystem};
use itertools::Itertools;
use lazy_static::lazy_static;
use proptest::{
    collection::btree_set,
    prelude::{prop::collection::vec, *},
};

use crate::{Tag, TagRef, TaggedFile, TaggedFilesystem, DIR_SEPARATOR, SEPARATORS, TAG_END};

lazy_static! {
    pub static ref SEPARATORS_STRING: String = SEPARATORS.iter().collect();
    static ref SEPARATORS_AND_ENDS: String = format!("{}{TAG_END}", *SEPARATORS_STRING);
    static ref SEPARATOR_REGEX: String = format!("[{}]", *SEPARATORS_STRING);
    static ref TAG_REGEX: String = format!(
        "[^{}][^{}]{{0,16}}",
        *SEPARATORS_AND_ENDS, *SEPARATORS_STRING
    );
    static ref NAME_REGEX: String = format!("[^{DIR_SEPARATOR}]{{0,16}}");
}

impl Arbitrary for TaggedFilesystem<FakeFileSystem> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        const MAX_TAG_SET_SIZE: usize = 100;
        const MAX_TAGS: usize = 10;
        const MAX_FILES: usize = 100;
        btree_set(Tag::arbitrary(), 1..=MAX_TAG_SET_SIZE)
            .prop_map(|set| set.into_iter().collect_vec())
            .prop_flat_map(|tags| {
                vec(
                    // We use a `vec` instead of a `btree_set`
                    // because we want these unsorted.
                    vec(0..tags.len(), 0..=MAX_TAGS)
                        .prop_map(move |indices| {
                            indices
                                .into_iter()
                                .unique()
                                .map(|i| tags[i].clone())
                                .collect_vec()
                        })
                        .prop_flat_map(|tags| {
                            (
                                NAME_REGEX.as_str(),
                                vec(SEPARATOR_REGEX.as_str(), tags.len()).prop_map(|xs| {
                                    xs.into_iter()
                                        .map(|s| s.chars().next().expect("at least one character"))
                                        .collect()
                                }),
                            )
                                .prop_map(move |(name, seps)| {
                                    TaggedFile::from(RawTaggedFile {
                                        name,
                                        tags: tags.clone(),
                                        seps,
                                    })
                                })
                        }),
                    0..=MAX_FILES,
                )
                .prop_filter("duplicate files", |files| {
                    files.iter().map(TagSetTaggedFile::new).all_unique()
                })
                .prop_map(|files| {
                    let fs = FakeFileSystem::new();
                    for file in files.iter() {
                        make_file_and_parent(&fs, file.as_path());
                    }
                    TaggedFilesystem::new(fs)
                })
            })
            .boxed()
    }
}

pub fn make_file_and_parent<P>(fs: &FakeFileSystem, path: P)
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        fs.create_dir_all(parent).unwrap();
    }
    fs.create_file(path, "").unwrap();
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagSetTaggedFile<'a> {
    tags: BTreeSet<&'a TagRef>,
    name: &'a str,
}

impl<'a> TagSetTaggedFile<'a> {
    pub fn new(file: &'a TaggedFile) -> Self {
        Self {
            tags: file.tags().collect(),
            name: file.name(),
        }
    }
}

impl Arbitrary for TaggedFile {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        RawTaggedFile::arbitrary().prop_map_into().boxed()
    }
}

#[derive(Clone, Debug)]
pub struct RawTaggedFile {
    pub name: String,
    pub tags: Vec<Tag>,
    pub seps: Vec<char>,
}

impl fmt::Display for RawTaggedFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (tag, sep) in self.tags.iter().zip(self.seps.iter()) {
            tag.fmt(f)?;
            sep.fmt(f)?;
        }
        TAG_END.fmt(f)?;
        self.name.fmt(f)
    }
}

impl From<RawTaggedFile> for TaggedFile {
    fn from(value: RawTaggedFile) -> Self {
        Self::new(value.to_string()).unwrap()
    }
}

impl Arbitrary for RawTaggedFile {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            NAME_REGEX.as_str(),
            vec(Tag::arbitrary(), 0_usize..16)
                .prop_map(|tags| tags.into_iter().unique().collect_vec())
                .prop_flat_map(|tags| {
                    (
                        vec(SEPARATOR_REGEX.as_str(), tags.len()).prop_map(|xs| {
                            xs.into_iter()
                                .map(|s| s.chars().next().expect("at least one character"))
                                .collect()
                        }),
                        Just(tags),
                    )
                }),
        )
            .prop_map(|(name, (seps, tags))| Self { name, tags, seps })
            .boxed()
    }
}

impl Arbitrary for Tag {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        TAG_REGEX
            .as_str()
            .prop_filter("all characters were '.'", |s| s.chars().any(|c| c != '.'))
            .prop_map(|x| Self::new(x).unwrap())
            .boxed()
    }
}
