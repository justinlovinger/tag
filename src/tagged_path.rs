use std::{
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

use derive_more::Display;
use itertools::Itertools;

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

impl TaggedPath {
    pub fn new<S>(path: S) -> Result<TaggedPath, TaggedPathError>
    where
        S: Into<String>,
    {
        let path: String = path.into();
        match path.split_once(EXT_SEPARATOR) {
            Some((_, ext)) => {
                if ext.contains(DIR_SEPARATOR) {
                    Err(TaggedPathError(path))
                } else {
                    Ok(Self(path))
                }
            }
            None => Err(TaggedPathError(path)),
        }
    }

    pub fn from_path<P>(path: P) -> Result<TaggedPath, TaggedPathError>
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

    pub fn from_tags<T, E>(tags: &[T], ext: E) -> TaggedPath
    where
        T: AsRef<TagRef>,
        E: AsRef<ExtRef>,
    {
        if tags.is_empty() {
            Self(format!("{TAG_IGNORE}{EXT_SEPARATOR}{}", ext.as_ref()))
        } else {
            let inline_separator = INLINE_SEPARATOR.to_string();
            Self(format!(
                "{}{EXT_SEPARATOR}{}",
                tags.iter()
                    .format_with(&inline_separator, |tag, f| f(&tag.as_ref())),
                ext.as_ref()
            ))
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

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn into_path(self) -> PathBuf {
        self.0.into()
    }
}

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

impl AsRef<Path> for TaggedPath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl AsRef<str> for TaggedPath {
    fn as_ref(&self) -> &str {
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

    #[test]
    fn from_tags_adds_ignored_tag_if_no_tags() {
        assert_eq!(
            TaggedPath::from_tags::<Tag, _>(&[], ext("x")).to_string(),
            "_.x"
        );
    }

    #[proptest]
    fn from_tags_matches_new(tags: Vec<Tag>, ext: Ext) {
        let path = TaggedPath::from_tags(&tags, ext);
        prop_assert_eq!(TaggedPath::from_path(path.as_path()).unwrap(), path);
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
