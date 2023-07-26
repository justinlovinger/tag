use std::fmt;

use itertools::Itertools;
use lazy_static::lazy_static;
use proptest::prelude::{prop::collection::vec, *};

use crate::{Tag, TaggedFile, DIR_SEPARATOR, SEPARATORS, TAG_END};

lazy_static! {
    pub static ref SEPARATORS_STRING: String = SEPARATORS.iter().collect();
    static ref SEPARATORS_AND_ENDS: String = format!("{}{TAG_END}", *SEPARATORS_STRING);
    static ref SEPARATOR_REGEX: String = format!("[{}]", *SEPARATORS_STRING);
    static ref TAG_REGEX: String = format!("[^{}][^{}]*", *SEPARATORS_AND_ENDS, *SEPARATORS_STRING);
    static ref NAME_REGEX: String = format!("[^{DIR_SEPARATOR}]*");
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

impl Arbitrary for TaggedFile {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        RawTaggedFile::arbitrary()
            .prop_map(|x| Self::new(x.to_string()).unwrap())
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
