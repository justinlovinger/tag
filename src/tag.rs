use std::{borrow::Borrow, cmp::Ordering, ops::Deref};

use derive_more::Display;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::{SEPARATORS, TAG_END};

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag(String);

impl Tag {
    pub fn new(s: String) -> Option<Tag> {
        if s.starts_with(TAG_END) || s.chars().any(|c| SEPARATORS.iter().any(|sep| &c == sep)) {
            None
        } else {
            Some(Tag(s))
        }
    }
}

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, RefCastCustom)]
#[repr(transparent)]
pub struct TagRef(str);

impl TagRef {
    #[ref_cast_custom]
    pub(crate) const fn new(s: &str) -> &Self;
}

impl PartialOrd<&TagRef> for Tag {
    fn partial_cmp(&self, other: &&TagRef) -> Option<Ordering> {
        self.as_ref().partial_cmp(other)
    }
}

impl PartialOrd<Tag> for &TagRef {
    fn partial_cmp(&self, other: &Tag) -> Option<Ordering> {
        self.partial_cmp(&other.as_ref())
    }
}

impl PartialEq<&TagRef> for Tag {
    fn eq(&self, other: &&TagRef) -> bool {
        self.as_ref().eq(other)
    }
}

impl PartialEq<Tag> for &TagRef {
    fn eq(&self, other: &Tag) -> bool {
        self.eq(&other.as_ref())
    }
}

impl Deref for Tag {
    type Target = TagRef;

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl AsRef<TagRef> for Tag {
    fn as_ref(&self) -> &TagRef {
        self.borrow()
    }
}

impl Borrow<TagRef> for Tag {
    fn borrow(&self) -> &TagRef {
        TagRef::new(self.0.as_str())
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::testing::*;

    use super::*;

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_none_for_strings_starting_with_tag_end(
        #[strategy(STARTS_WITH_TAG_END_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_none_for_strings_containing_a_separator(
        #[strategy(TAG_WITH_SEP_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_some_for_valid_tags(#[strategy(TAG_REGEX.as_str())] s: String) {
        prop_assert!(Tag::new(s).is_some());
    }

    lazy_static! {
        static ref STARTS_WITH_TAG_END_REGEX: String = format!(r"{}\PC*", TAG_END);
        static ref TAG_WITH_SEP_REGEX: String = format!(r"\PC*[{}]\PC*", *SEPARATORS_STRING);
    }
}
