use std::{
    fmt,
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

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

#[derive(Clone)]
pub struct TaggedPath {
    path: String,
    /// Index of first `EXT_SEPARATOR`.
    ext: usize,
}

impl fmt::Debug for TaggedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

impl fmt::Display for TaggedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

impl Eq for TaggedPath {}
impl PartialEq for TaggedPath {
    fn eq(&self, other: &Self) -> bool {
        self.path.eq(&other.path)
    }
}
impl Ord for TaggedPath {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.path.cmp(&other.path)
    }
}
impl PartialOrd for TaggedPath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Hash for TaggedPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state)
    }
}

impl TaggedPath {
    pub fn new<S>(path: S) -> Result<TaggedPath, TaggedPathError>
    where
        S: Into<String>,
    {
        let path: String = path.into();
        match path.find(EXT_SEPARATOR) {
            Some(ext) => {
                let this = Self { path, ext };
                if this.ext().as_str().contains(DIR_SEPARATOR) {
                    Err(TaggedPathError(this.path))
                } else {
                    Ok(this)
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
            Self {
                path: format!("{TAG_IGNORE}{EXT_SEPARATOR}{}", ext.as_ref()),
                ext: TAG_IGNORE.len_utf8(),
            }
        } else {
            let inline_separator = INLINE_SEPARATOR.to_string();
            let tags_str = tags
                .iter()
                .format_with(&inline_separator, |tag, f| f(&tag.as_ref()))
                .to_string();
            let ext_index = tags_str.len();
            Self {
                path: format!("{tags_str}{EXT_SEPARATOR}{}", ext.as_ref()),
                ext: ext_index,
            }
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
        // SAFETY: `self.ext` is from a checked method.
        unsafe { self.path.get_unchecked(0..self.ext) }
    }

    pub fn ext(&self) -> &ExtRef {
        // SAFETY: `self.ext` is a valid index from a string method
        // and is known to be `EXT_SEPARATOR`.
        let s = unsafe {
            self.path
                .get_unchecked((self.ext + EXT_SEPARATOR.len_utf8())..self.path.len())
        };
        // SAFETY: extension is validated when `Self` is validated.
        unsafe { ExtRef::new_unchecked(s) }
    }

    pub fn as_path(&self) -> &Path {
        self.path.as_ref()
    }

    pub fn into_path(self) -> PathBuf {
        self.path.into()
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
        value.path.into()
    }
}

impl AsRef<Path> for TaggedPath {
    fn as_ref(&self) -> &Path {
        self.path.as_ref()
    }
}

impl AsRef<str> for TaggedPath {
    fn as_ref(&self) -> &str {
        self.path.as_ref()
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
