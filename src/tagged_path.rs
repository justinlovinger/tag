use std::{
    borrow::Borrow,
    cmp::Ordering,
    hash::Hash,
    ops::Deref,
    path::{Path, PathBuf},
    str::FromStr,
};

use derive_more::Display;
use itertools::Itertools;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::{ExtRef, TagRef, DIR_SEPARATOR, EXT_SEPARATOR, INLINE_SEPARATOR, TAG_IGNORE};

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[error("`{0}` is not a tagged path: tagged paths must end with an extension")]
pub struct TaggedPathError(String);

impl TaggedPathError {
    pub fn into_path(self) -> PathBuf {
        self.0.into()
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TaggedPath(String);

impl FromStr for TaggedPath {
    type Err = TaggedPathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

impl From<TaggedPath> for PathBuf {
    fn from(value: TaggedPath) -> Self {
        value.0.into()
    }
}

impl From<TaggedPath> for String {
    fn from(value: TaggedPath) -> Self {
        value.0
    }
}

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct TaggedPathRef(str);

impl TaggedPathRef {
    #[ref_cast_custom]
    const unsafe fn new_unchecked(s: &str) -> &Self;
}

impl<'a> From<&'a TaggedPathRef> for &'a Path {
    fn from(value: &'a TaggedPathRef) -> Self {
        value.0.as_ref()
    }
}

impl<'a> From<&'a TaggedPathRef> for &'a str {
    fn from(value: &'a TaggedPathRef) -> Self {
        &value.0
    }
}

impl PartialOrd<&TaggedPathRef> for TaggedPath {
    fn partial_cmp(&self, other: &&TaggedPathRef) -> Option<Ordering> {
        self.as_ref().partial_cmp(other)
    }
}

impl PartialOrd<TaggedPath> for &TaggedPathRef {
    fn partial_cmp(&self, other: &TaggedPath) -> Option<Ordering> {
        self.partial_cmp(&other.as_ref())
    }
}

impl PartialEq<&TaggedPathRef> for TaggedPath {
    fn eq(&self, other: &&TaggedPathRef) -> bool {
        self.as_ref().eq(other)
    }
}

impl PartialEq<TaggedPath> for &TaggedPathRef {
    fn eq(&self, other: &TaggedPath) -> bool {
        self.eq(&other.as_ref())
    }
}

impl Deref for TaggedPath {
    type Target = TaggedPathRef;

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl AsRef<TaggedPathRef> for TaggedPath {
    fn as_ref(&self) -> &TaggedPathRef {
        self.borrow()
    }
}

impl Borrow<TaggedPathRef> for TaggedPath {
    fn borrow(&self) -> &TaggedPathRef {
        // SAFETY: `TaggedPathRef` is valid if `TaggedPath` is.
        unsafe { TaggedPathRef::new_unchecked(self.0.as_str()) }
    }
}

impl<'a> From<&'a TaggedPathRef> for TaggedPath {
    fn from(value: &'a TaggedPathRef) -> Self {
        value.to_owned()
    }
}

impl AsRef<TaggedPathRef> for TaggedPathRef {
    fn as_ref(&self) -> &TaggedPathRef {
        self
    }
}

impl ToOwned for TaggedPathRef {
    type Owned = TaggedPath;

    fn to_owned(&self) -> Self::Owned {
        TaggedPath(self.0.to_owned())
    }
}

impl TaggedPath {
    pub fn new<S>(path: S) -> Result<Self, TaggedPathError>
    where
        S: Into<String>,
    {
        let path: String = path.into();
        if Self::is_valid(&path) {
            Ok(Self(path))
        } else {
            Err(TaggedPathError(path))
        }
    }

    fn is_valid(path: &str) -> bool {
        path.split_once(EXT_SEPARATOR)
            .is_some_and(|(_, ext)| !ext.contains(DIR_SEPARATOR))
    }

    pub fn from_path<P>(path: P) -> Result<Self, TaggedPathError>
    where
        P: Into<PathBuf>,
    {
        let path = path.into();
        Self::new(
            path.into_os_string()
                .into_string()
                .expect("path should contain valid unicode"),
        )
    }

    pub fn from_tags<T, E>(tags: impl IntoIterator<Item = T>, ext: E) -> TaggedPath
    where
        T: AsRef<TagRef>,
        E: AsRef<ExtRef>,
    {
        let inline_separator = INLINE_SEPARATOR.to_string();
        Self(format!(
            "{}{EXT_SEPARATOR}{}",
            tags.into_iter()
                .format_with(&inline_separator, |tag, f| f(&tag.as_ref())),
            ext.as_ref()
        ))
    }

    pub fn into_path(self) -> PathBuf {
        self.0.into()
    }
}

impl TaggedPathRef {
    pub fn new(path: &str) -> Result<&Self, TaggedPathError> {
        if TaggedPath::is_valid(path) {
            // SAFETY: validity was just checked above.
            Ok(unsafe { Self::new_unchecked(path) })
        } else {
            Err(TaggedPathError(path.to_owned()))
        }
    }

    pub fn tags(&self) -> impl Iterator<Item = &TagRef> {
        self.tags_str()
            .split([INLINE_SEPARATOR, DIR_SEPARATOR])
            .filter(|s| !s.is_empty() && !s.starts_with(TAG_IGNORE))
            // SAFETY: invalid tags are filtered above.
            .map(|s| unsafe { TagRef::new_unchecked(s) })
    }

    // If this is made public,
    // it should have unit tests.
    pub(crate) fn ignored_tags(&self) -> impl Iterator<Item = &str> {
        self.tags_str()
            .split([INLINE_SEPARATOR, DIR_SEPARATOR])
            .filter(|s| s.starts_with(TAG_IGNORE))
            .map(|s| {
                s.strip_prefix(TAG_IGNORE)
                    .expect("ignored tags should start with TAG_IGNORE")
            })
    }

    fn tags_str(&self) -> &str {
        self.0
            .split_once(EXT_SEPARATOR)
            .expect("should have EXT_SEPARATOR")
            .0
    }

    pub fn ext(&self) -> &ExtRef {
        let s = self
            .0
            .split_once(EXT_SEPARATOR)
            .expect("should have EXT_SEPARATOR")
            .1;
        // SAFETY: extension is validated when `Self` is validated.
        unsafe { ExtRef::new_unchecked(s) }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use once_cell::sync::Lazy;
    use proptest::prelude::*;
    use test_strategy::proptest;

    use crate::{testing::*, Ext, Tag};

    use super::*;

    #[test]
    fn new_returns_ok_for_tagged_paths() {
        assert!(TaggedPath::new("foo-bar.baz").is_ok());
        assert!(TaggedPath::new("foo/bar.baz").is_ok());
        assert!(TaggedPath::new("🙂/🙁.baz").is_ok());
    }

    #[test]
    fn new_returns_ok_for_paths_with_empty_tags() {
        assert!(TaggedPath::new("-bar.baz").is_ok());
        assert!(TaggedPath::new("foo-.baz").is_ok());
        assert!(TaggedPath::new("/bar.baz").is_ok());
        assert!(TaggedPath::new("foo/.baz").is_ok());
        assert!(TaggedPath::new("foo-.baz").is_ok());
    }

    #[test]
    fn new_returns_ok_for_paths_with_no_tags() {
        assert!(TaggedPath::new(".baz").is_ok());
    }

    #[proptest]
    fn new_returns_ok_iff_all_tags_are_valid(#[strategy(MAYBE_TAGGED_PATH.as_str())] s: String) {
        // This test intentionally allows some invalid paths
        // to ensure a wide variety of paths are tested.
        if let Ok(path) = TaggedPath::new(s) {
            for tag in path.tags() {
                prop_assert!(Tag::new(tag.to_string()).is_ok())
            }
        }
    }

    #[test]
    fn new_returns_err_for_paths_without_ext() {
        assert!(TaggedPath::new("foo-bar").is_err());
        assert!(TaggedPath::new("foo/bar").is_err());
    }

    #[test]
    fn new_allows_duplicate_tags() {
        assert!(TaggedPath::new("foo-foo.baz").is_ok());
        assert!(TaggedPath::new("foo/foo.baz").is_ok());
        assert!(TaggedPath::new("foo-bar/foo.baz").is_ok());
        assert!(TaggedPath::new("bar/foo-foo.baz").is_ok());
    }

    #[test]
    fn new_returns_err_if_ext_contains_dir_separator() {
        assert!(TaggedPath::new("foo.baz/biz").is_err());
        assert!(TaggedPath::new("foo./").is_err());
        assert!(TaggedPath::new("foo./baz").is_err());
    }

    #[proptest]
    fn from_tags_matches_new(tags: Vec<Tag>, ext: Ext) {
        let path = TaggedPath::from_tags(tags, ext);
        prop_assert_eq!(TaggedPath::new(path.as_str()).unwrap(), path);
    }

    #[proptest]
    fn tagged_path_ref_new_matches_tagged_path_new(
        #[strategy(MAYBE_TAGGED_PATH.as_str())] path: String,
    ) {
        match (TaggedPathRef::new(&path), TaggedPath::new(&path)) {
            (Ok(x), Ok(y)) => prop_assert_eq!(x, y),
            (Ok(_), Err(_)) => prop_assert!(false),
            (Err(_), Ok(_)) => prop_assert!(false),
            (Err(x), Err(y)) => prop_assert_eq!(x, y),
        }
    }

    #[proptest]
    fn tags_returns_all_tags(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.tags().collect::<Vec<_>>(), raw_path.tags);
    }

    #[test]
    fn tags_ignores_empty_tags() {
        assert_eq!(
            tagged_path("-bar.baz").tags().collect::<Vec<_>>(),
            [tag("bar")]
        );
        assert_eq!(
            tagged_path("foo-.baz").tags().collect::<Vec<_>>(),
            [tag("foo")]
        );
        assert_eq!(
            tagged_path("/bar.baz").tags().collect::<Vec<_>>(),
            [tag("bar")]
        );
        assert_eq!(
            tagged_path("foo/.baz").tags().collect::<Vec<_>>(),
            [tag("foo")]
        );
        assert_eq!(
            tagged_path("foo-.baz").tags().collect::<Vec<_>>(),
            [tag("foo")]
        );
    }

    #[test]
    fn tags_ignores_ignored_tags() {
        assert_eq!(
            TaggedPath::new("foo-_.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo")]
        );
        assert_eq!(
            TaggedPath::new("foo/_.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo")]
        );
        assert_eq!(
            TaggedPath::new("foo-_-bar.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo"), tag("bar")]
        );
        assert_eq!(
            TaggedPath::new("foo-_bar-biz.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo"), tag("biz")]
        );
        assert_eq!(
            TaggedPath::new("foo-_-bar-_.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo"), tag("bar")]
        );
        assert_eq!(
            TaggedPath::new("foo-_bar-biz-_baz.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo"), tag("biz")]
        );
        assert_eq!(
            TaggedPath::new("foo/_bar-biz/_baz.x")
                .unwrap()
                .tags()
                .map(|tag| tag.to_owned())
                .collect_vec(),
            [tag("foo"), tag("biz")]
        );
    }

    #[proptest]
    fn ext_returns_ext(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.ext(), raw_path.ext.as_ref());
    }

    static MAYBE_TAGGED_PATH: Lazy<String> =
        Lazy::new(|| format!(r"\PC{{0,16}}{EXT_SEPARATOR}[a-z-_.]{{0,16}}",));
}
