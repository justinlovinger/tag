use std::{borrow::Borrow, cmp::Ordering, ops::Deref, path::Path, str::FromStr};

use derive_more::Display;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::{DIR_SEPARATOR, INLINE_SEPARATOR, SEPARATORS, TAG_END};

#[derive(Debug, thiserror::Error)]
#[error("Invalid tag: `{0}`. Tags cannot start with `.` or `{TAG_END}` or contain `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}`.")]
pub struct TagError(String);

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag(String);

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct TagRef(str);

impl Tag {
    pub fn new<S>(s: S) -> Result<Tag, TagError>
    where
        S: Into<String>,
    {
        let s = s.into();
        if s.is_empty()
            || s.starts_with(TAG_END)
            || s.starts_with('.')
            || s.chars().any(|c| SEPARATORS.iter().any(|sep| &c == sep))
        {
            Err(TagError(s))
        } else {
            Ok(Tag(s))
        }
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }
}

impl FromStr for Tag {
    type Err = TagError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
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

impl AsRef<TagRef> for TagRef {
    fn as_ref(&self) -> &TagRef {
        self
    }
}

impl ToOwned for TagRef {
    type Owned = Tag;

    fn to_owned(&self) -> Self::Owned {
        Tag(self.0.to_owned())
    }
}

impl AsRef<TagRef> for internment::Intern<Tag> {
    fn as_ref(&self) -> &TagRef {
        let this: &Tag = self.as_ref();
        this.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{DIR_SEPARATOR, INLINE_SEPARATOR};

    use super::*;

    #[test]
    fn new_returns_some_for_valid_tags() {
        test_new("foo");
        test_new("bar");
        test_new("🙂");
        test_new("foo.");
        test_new("has_underscore");
    }

    fn test_new(tag: &str) {
        assert_eq!(Tag::new(tag).unwrap().to_string(), tag);
    }

    #[test]
    fn new_returns_none_for_empty_strings() {
        assert!(Tag::new(String::new()).is_err());
    }

    #[proptest]
    fn new_returns_none_for_strings_starting_with_tag_end(tag: Tag) {
        let s = format!("{TAG_END}{tag}");
        prop_assert!(Tag::new(s).is_err());
    }

    #[proptest]
    fn new_returns_none_for_strings_containing_a_separator(tag_one: Tag, tag_two: Tag) {
        {
            let s = format!("{tag_one}{DIR_SEPARATOR}{tag_two}");
            prop_assert!(Tag::new(s).is_err());
        }
        {
            let s = format!("{tag_one}{INLINE_SEPARATOR}{tag_two}");
            prop_assert!(Tag::new(s).is_err());
        }
    }

    #[proptest]
    fn new_returns_none_for_strings_starting_with_dot(tag: Tag) {
        let s = format!(".{tag}");
        prop_assert!(Tag::new(s).is_err());
    }
}
