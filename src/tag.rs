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
        if s.is_empty()
            || s.starts_with(TAG_END)
            || s.starts_with('.')
            || s.chars().any(|c| SEPARATORS.iter().any(|sep| &c == sep))
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
    use once_cell::sync::Lazy;
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::testing::*;

    use super::*;

    #[test]
    fn new_returns_some_for_simple_tags() {
        test_new("foo");
        test_new("bar");
        test_new("🙂");
        test_new("foo.");
        test_new("has_underscore");
    }

    #[proptest]
    fn new_returns_some_for_valid_tags(#[strategy(TAG_REGEX.as_str())] s: String) {
        test_new(&s);
    }

    fn test_new(tag: &str) {
        assert_eq!(Tag::new(tag.to_owned()).unwrap().to_string(), tag);
    }

    #[test]
    fn new_returns_none_for_empty_strings() {
        assert!(Tag::new(String::new()).is_none());
    }

    #[proptest]
    fn new_returns_none_for_strings_starting_with_tag_end(
        #[strategy(STARTS_WITH_TAG_END_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest]
    fn new_returns_none_for_strings_containing_a_separator(
        #[strategy(TAG_WITH_SEP_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest]
    fn new_returns_none_for_strings_starting_with_dot(
        #[strategy(STARTS_WITH_DOT.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    static STARTS_WITH_DOT: Lazy<String> = Lazy::new(|| format!(r"\.{}", *TAG_REGEX));
    static STARTS_WITH_TAG_END_REGEX: Lazy<String> =
        Lazy::new(|| format!(r"{TAG_END}{}", *TAG_REGEX));
    static TAG_WITH_SEP_REGEX: Lazy<String> =
        Lazy::new(|| format!(r"{}[{}]{}", *TAG_REGEX, *SEPARATORS_STRING, *TAG_REGEX));
}
