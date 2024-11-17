use std::{
    collections::BTreeSet,
    fmt,
    fs::{create_dir_all, File},
    path::Path,
};

use itertools::Itertools;
use once_cell::sync::Lazy;
use proptest::{
    collection::hash_set,
    prelude::{prop::collection::vec, *},
};
use tempfile::TempDir;

use crate::{Tag, TagRef, TaggedFile, TaggedFilesystem, SEPARATORS, TAG_END};

pub static SEPARATORS_STRING: Lazy<String> = Lazy::new(|| SEPARATORS.iter().collect());
static _SEPARATORS_AND_ENDS: Lazy<String> =
    Lazy::new(|| format!("{}{TAG_END}", *SEPARATORS_STRING));
static SEPARATOR_REGEX: Lazy<String> = Lazy::new(|| format!("[{}]", *SEPARATORS_STRING));
pub static TAG_REGEX: Lazy<String> = Lazy::new(|| "[a-z][a-z_.]{1,16}".to_string());
pub static NAME_REGEX: Lazy<String> = Lazy::new(|| "[a-z-_.]{0,16}".to_string());

pub fn with_tempdir<F, T>(f: F) -> T
where
    F: FnOnce() -> T,
{
    // Working directory is not per-thread.
    static LOCK: Lazy<std::sync::Arc<std::sync::Mutex<()>>> =
        Lazy::new(|| std::sync::Arc::new(std::sync::Mutex::new(())));
    // Lock may get poisoned from panicking tests, but that is ok.
    let _lock = LOCK.lock().unwrap_or_else(|e| e.into_inner());

    let cwd = TempDir::with_prefix(std::thread::current().name().map_or_else(
        || format!("{:?}", std::thread::current().id()),
        |x| x.to_string().replace(':', "_"),
    ))
    .unwrap();
    std::env::set_current_dir(&cwd).unwrap();
    let res = (f)();
    cwd.close().unwrap();
    res
}

#[derive(Debug)]
pub struct TaggedFilesWithMetadata {
    pub files: TaggedFiles,
    pub tags: Vec<Tag>,
}

impl Arbitrary for TaggedFilesWithMetadata {
    type Parameters = TaggedFilesParams;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
        tagged_files_with_metadata_strategy(params)
    }
}

#[derive(Debug, Clone)]
pub struct TaggedFiles(pub Vec<TaggedFile>);

impl IntoIterator for TaggedFiles {
    type Item = TaggedFile;
    type IntoIter = <Vec<TaggedFile> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub struct TaggedFilesParams {
    pub min_tag_set: usize,
    pub max_tag_set: usize,
    pub min_tags: usize,
    pub max_tags: usize,
    pub min_files: usize,
    pub max_files: usize,
}

impl Default for TaggedFilesParams {
    fn default() -> Self {
        Self {
            min_tag_set: 1,
            max_tag_set: 100,
            min_tags: 0,
            max_tags: 10,
            min_files: 0,
            max_files: 100,
        }
    }
}

impl Arbitrary for TaggedFiles {
    type Parameters = TaggedFilesParams;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
        tagged_files_strategy(params)
    }
}

pub fn make_filesystem_with<P>(files: impl IntoIterator<Item = P>) -> TaggedFilesystem
where
    P: AsRef<Path>,
{
    for file in files.into_iter() {
        make_file_and_parent(file.as_ref());
    }
    TaggedFilesystem::new()
}

fn tagged_files_strategy(params: TaggedFilesParams) -> BoxedStrategy<TaggedFiles> {
    tagged_files_with_metadata_strategy(params)
        .prop_map(|x| x.files)
        .boxed()
}

fn tagged_files_with_metadata_strategy(
    params: TaggedFilesParams,
) -> BoxedStrategy<TaggedFilesWithMetadata> {
    hash_set(Tag::arbitrary(), params.min_tag_set..=params.max_tag_set)
        .prop_map(|set| set.into_iter().collect_vec())
        .prop_flat_map(move |tags| {
            let tags_ = tags.clone();
            (
                vec(
                    // We use a `vec` because we want these unsorted.
                    vec(0..tags.len(), params.min_tags..=params.max_tags)
                        .prop_map(move |indices| {
                            indices
                                .into_iter()
                                .unique()
                                .map(|i| tags_[i].clone())
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
                    params.min_files..=params.max_files,
                )
                .prop_filter("duplicate files", |files| {
                    files.iter().map(TagSetTaggedFile::new).all_unique()
                }),
                Just(tags),
            )
        })
        .prop_map(|(files, tags)| TaggedFilesWithMetadata {
            files: TaggedFiles(files),
            tags,
        })
        .boxed()
}

pub fn make_file_and_parent<P>(path: P)
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    if let Some(parent) = path.parent() {
        create_dir_all(parent).unwrap();
    }
    // We would prefer this to be `File::create_new`, once it is stable.
    File::create(path).unwrap();
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
            .prop_map(|x| Self::new(x).unwrap())
            .boxed()
    }
}
