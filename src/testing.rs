use std::fmt;

use lazy_static::lazy_static;
use proptest::prelude::{prop::collection::vec, *};

use crate::{Tag, TaggedFile, SEPARATORS, TAG_END};

lazy_static! {
    pub static ref SEPARATORS_STRING: String = SEPARATORS.iter().collect();
    pub static ref SEPARATORS_AND_ENDS: String = format!("{}{}", *SEPARATORS_STRING, TAG_END);
    pub static ref TAG_REGEX: String =
        format!("[^{}][^{}]*", *SEPARATORS_AND_ENDS, *SEPARATORS_STRING);
    pub static ref SEPARATOR_REGEX: String = format!("[{}]", SEPARATORS_STRING.deref());
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
            r"\PC*",
            (0_usize..16).prop_flat_map(|len| {
                (
                    vec(TAG_REGEX.as_str(), len)
                        .prop_map(|xs| xs.into_iter().map(|s| Tag::new(s).unwrap()).collect()),
                    vec(SEPARATOR_REGEX.as_str(), len).prop_map(|xs| {
                        xs.into_iter()
                            .map(|s| s.chars().next().expect("at least one character"))
                            .collect()
                    }),
                )
            }),
        )
            .prop_map(|(name, (tags, seps))| Self { name, tags, seps })
            .boxed()
    }
}
