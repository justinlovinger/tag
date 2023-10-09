use std::{borrow::Borrow, cmp::Ordering, ops::Deref, path::Path};

use derive_more::Display;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::{SEPARATORS, TAG_END};

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag(String);

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct TagRef(str);

impl Tag {
    pub fn new(s: String) -> Option<Tag> {
        if s.starts_with(TAG_END)
            || s.chars().any(|c| SEPARATORS.iter().any(|sep| &c == sep))
            // Paths like `.` and `..` have special meanings in filesystems.
            || s.chars().all(|c| c == '.')
        {
            None
        } else {
            Some(Tag(s))
        }
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }
}

impl TagRef {
    #[ref_cast_custom]
    pub(crate) const fn new(s: &str) -> &Self;

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
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

impl From<Tag> for String {
    fn from(value: Tag) -> Self {
        value.0
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

impl<'a> From<&'a TagRef> for Tag {
    fn from(value: &'a TagRef) -> Self {
        value.to_owned()
    }
}

impl ToOwned for TagRef {
    type Owned = Tag;

    fn to_owned(&self) -> Self::Owned {
        Tag(self.0.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::testing::*;

    use super::*;

    #[test]
    fn new_returns_none_for_empty_strings() {
        assert!(Tag::new(String::new()).is_none());
    }

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
    fn new_returns_none_for_strings_containing_only_dot(#[strategy(r"\.+")] s: String) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[test]
    fn new_returns_tag_for_valid_tags() {
        test_new("foo");
        test_new("bar");
        test_new("🙂");
        test_new(".x.");
        test_new(".x");
        test_new("x.");
        test_new("has_underscore");
    }

    fn test_new(tag: &str) {
        assert_eq!(Tag::new(tag.to_owned()).unwrap().to_string(), tag);
    }

    lazy_static! {
        static ref STARTS_WITH_TAG_END_REGEX: String = format!(r"{TAG_END}\PC*");
        static ref TAG_WITH_SEP_REGEX: String = format!(r"\PC*[{}]\PC*", *SEPARATORS_STRING);
    }
}
