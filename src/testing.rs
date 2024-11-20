use once_cell::sync::Lazy;

use crate::{SEPARATORS, TAG_END};

pub use self::{fs::*, tag::*, tagged_file::*, tagged_filesystem::*};

pub static SEPARATORS_STRING: Lazy<String> = Lazy::new(|| SEPARATORS.iter().collect());
static _SEPARATORS_AND_ENDS: Lazy<String> =
    Lazy::new(|| format!("{}{TAG_END}", *SEPARATORS_STRING));
static SEPARATOR_REGEX: Lazy<String> = Lazy::new(|| format!("[{}]", *SEPARATORS_STRING));

mod fs {
    use std::{
        fs::{create_dir_all, File},
        path::Path,
        sync::{Arc, Mutex},
    };

    use once_cell::sync::Lazy;

    pub fn with_tempdir<F, T>(f: F) -> T
    where
        F: FnOnce() -> T,
    {
        let cwd = tempfile::tempdir().unwrap();

        // Working directory is not per-thread.
        static LOCK: Lazy<Arc<Mutex<()>>> = Lazy::new(|| Arc::new(Mutex::new(())));
        // Lock may get poisoned from panicking tests, but that is ok.
        let lock = LOCK.lock().unwrap_or_else(|e| e.into_inner());

        std::env::set_current_dir(&cwd).unwrap();
        let res = (f)();

        drop(lock);

        cwd.close().unwrap();

        res
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
}

mod tagged_filesystem {
    use std::path::Path;

    use proptest::prelude::{prop::collection::vec, *};

    use crate::{TaggedFile, TaggedFilesystem};

    use super::{make_file_and_parent, TaggedFileParams};

    pub fn tagged_filesystem_with<P>(files: impl IntoIterator<Item = P>) -> TaggedFilesystem
    where
        P: AsRef<Path>,
    {
        for file in files.into_iter() {
            make_file_and_parent(file.as_ref());
        }
        TaggedFilesystem::new()
    }

    pub struct TaggedFilesParams {
        pub tagged_file_params: TaggedFileParams,
        pub min_files: usize,
        pub max_files: usize,
    }

    impl Default for TaggedFilesParams {
        fn default() -> Self {
            Self {
                tagged_file_params: TaggedFileParams::default(),
                min_files: 0,
                max_files: 10,
            }
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

    impl Arbitrary for TaggedFiles {
        type Parameters = TaggedFilesParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            vec(
                TaggedFile::arbitrary_with(params.tagged_file_params),
                params.min_files..=params.max_files,
            )
            .prop_map(TaggedFiles)
            .boxed()
        }
    }
}

mod tagged_file {
    use std::{collections::BTreeSet, fmt};

    use itertools::Itertools;

    use proptest::{
        prelude::{prop::collection::vec, *},
        sample::subsequence,
        strategy::LazyJust,
    };
    use uuid::Uuid;

    use crate::{Tag, TaggedFile, TAG_END};

    use super::{SEPARATOR_REGEX, TAGS};

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TagSetTaggedFile {
        tags: BTreeSet<Tag>,
        name: String,
    }

    impl TagSetTaggedFile {
        pub fn new(file: TaggedFile) -> Self {
            Self {
                tags: file.tags().map(|tag| tag.to_owned()).collect(),
                name: file.name().to_owned(),
            }
        }
    }

    pub struct TaggedFileParams {
        pub min_tags: usize,
        pub max_tags: usize,
    }

    impl Default for TaggedFileParams {
        fn default() -> Self {
            Self {
                min_tags: 0,
                max_tags: 8,
            }
        }
    }

    impl Arbitrary for TaggedFile {
        type Parameters = TaggedFileParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            RawTaggedFile::arbitrary_with(params)
                .prop_map_into()
                .boxed()
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
        type Parameters = TaggedFileParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            (
                // We specifically want unique names.
                // That means not using Proptest to generate them.
                // Proptest could shrink names into being non-unique.
                // Note,
                // `Just` may clone the given value,
                // so we need to use `LazyJust` to ensure a new value is generated every time.
                LazyJust::new(|| Uuid::new_v4().to_string()),
                // Note,
                // `subsequence` puts tags in the order defined in `TAGS`,
                // which could leave gaps in our testing.
                subsequence(TAGS, params.min_tags..=params.max_tags)
                    .prop_map(|tags| {
                        tags.into_iter()
                            .map(|x| Tag::new(x.to_owned()).unwrap())
                            .collect_vec()
                    })
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
}

mod tag {
    use proptest::{prelude::*, sample::select};

    use crate::Tag;

    pub const TAGS: &[&str] = &[
        "foo", "bar", "baz", "qux", "quux", "corge", "grault", "garply", "waldo", "fred", "plugh",
        "xyzzy", "thud",
    ];

    impl Arbitrary for Tag {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            select(TAGS)
                .prop_map(|x| Self::new(x.to_owned()).unwrap())
                .boxed()
        }
    }
}
