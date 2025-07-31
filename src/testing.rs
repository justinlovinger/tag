use once_cell::sync::Lazy;

use crate::{SEPARATORS, TAG_END};

pub use self::{fs::*, name::*, tag::*, tagged_filesystem::*, tagged_path::*};

pub static SEPARATORS_STRING: Lazy<String> = Lazy::new(|| SEPARATORS.iter().collect());
static _SEPARATORS_AND_ENDS: Lazy<String> =
    Lazy::new(|| format!("{}{TAG_END}", *SEPARATORS_STRING));
static SEPARATOR_REGEX: Lazy<String> = Lazy::new(|| format!("[{}]", *SEPARATORS_STRING));

mod fs {
    use std::{
        fs::{create_dir_all, File},
        path::Path,
    };

    pub fn with_temp_dir<F, T>(f: F) -> T
    where
        F: FnOnce(&Path) -> T,
    {
        let dir = tempfile::tempdir().unwrap();
        let res = (f)(dir.path());
        dir.close().unwrap();
        res
    }

    pub fn create_file_and_parent<P>(path: P)
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
    use proptest::prelude::{prop::collection::vec, *};

    use crate::TaggedPath;

    use super::TaggedPathParams;

    pub struct TaggedPathsParams {
        pub tagged_path_params: TaggedPathParams,
        pub min_paths: usize,
        pub max_paths: usize,
    }

    impl Default for TaggedPathsParams {
        fn default() -> Self {
            Self {
                tagged_path_params: TaggedPathParams::default(),
                min_paths: 0,
                max_paths: 10,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TaggedPaths(pub Vec<TaggedPath>);

    impl TaggedPaths {
        pub fn iter(&self) -> std::slice::Iter<'_, TaggedPath> {
            self.0.iter()
        }

        pub fn len(&self) -> usize {
            self.0.len()
        }
    }

    impl IntoIterator for TaggedPaths {
        type Item = TaggedPath;
        type IntoIter = <Vec<TaggedPath> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a TaggedPaths {
        type Item = &'a TaggedPath;
        type IntoIter = <&'a [TaggedPath] as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.iter()
        }
    }

    impl Arbitrary for TaggedPaths {
        type Parameters = TaggedPathsParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            vec(
                TaggedPath::arbitrary_with(params.tagged_path_params),
                params.min_paths..=params.max_paths,
            )
            .prop_map(TaggedPaths)
            .boxed()
        }
    }
}

mod tagged_path {
    use std::fmt;

    use itertools::Itertools;

    use proptest::{
        prelude::{prop::collection::vec, *},
        sample::subsequence,
    };

    use crate::{Name, Tag, TaggedPath, TAG_END};

    use super::{SEPARATOR_REGEX, TAGS};

    pub struct TaggedPathParams {
        pub min_tags: usize,
        pub max_tags: usize,
    }

    impl Default for TaggedPathParams {
        fn default() -> Self {
            Self {
                min_tags: 0,
                max_tags: 8,
            }
        }
    }

    impl Arbitrary for TaggedPath {
        type Parameters = TaggedPathParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            RawTaggedPath::arbitrary_with(params)
                .prop_map_into()
                .boxed()
        }
    }

    #[derive(Clone, Debug)]
    pub struct RawTaggedPath {
        pub name: Name,
        pub tags: Vec<Tag>,
        pub seps: Vec<char>,
    }

    impl fmt::Display for RawTaggedPath {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (tag, sep) in self.tags.iter().zip(self.seps.iter()) {
                tag.fmt(f)?;
                sep.fmt(f)?;
            }
            TAG_END.fmt(f)?;
            self.name.fmt(f)
        }
    }

    impl From<RawTaggedPath> for TaggedPath {
        fn from(value: RawTaggedPath) -> Self {
            Self::new(value.to_string()).unwrap()
        }
    }

    impl Arbitrary for RawTaggedPath {
        type Parameters = TaggedPathParams;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(params: Self::Parameters) -> Self::Strategy {
            (
                Name::arbitrary(),
                // Note,
                // `subsequence` puts tags in the order defined in `TAGS`,
                // which could leave gaps in our testing.
                subsequence(TAGS, params.min_tags..=params.max_tags)
                    .prop_map(|tags| tags.into_iter().map(|x| Tag::new(x).unwrap()).collect_vec())
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

    pub fn tag(s: &str) -> Tag {
        Tag::new(s.to_owned()).unwrap()
    }

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

mod name {
    use proptest::{prelude::*, strategy::LazyJust};
    use uuid::Uuid;

    use crate::Name;

    pub fn name(s: &str) -> Name {
        Name::new(s.to_owned()).unwrap()
    }

    impl Arbitrary for Name {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            // We specifically want unique names.
            // That means not using Proptest to generate them.
            // Proptest could shrink names into being non-unique.
            // Note,
            // `Just` may clone the given value,
            // so we need to use `LazyJust` to ensure a new value is generated every time.
            LazyJust::new(|| Name::new(Uuid::new_v4().to_string()).unwrap()).boxed()
        }
    }
}
