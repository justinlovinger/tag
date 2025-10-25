use std::{
    fmt,
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::{
    ExtRef, TagRef, DIR_SEPARATOR, EXT_SEPARATOR, INLINE_SEPARATOR, SEPARATORS, TAG_IGNORE,
};

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[error("`{0}` is not a tagged path: tagged paths must contain zero or more unique tags ended by `{INLINE_SEPARATOR}`, `{DIR_SEPARATOR}`, or `{EXT_SEPARATOR}` with the tagging portion ended by `{EXT_SEPARATOR}`")]
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
    /// Slice indices to get tags from `path`,
    /// start inclusive
    /// and end exclusive.
    tags: Vec<TagIndices>,
    /// Slice indices to get extension from `path`,
    /// start inclusive
    /// and end exclusive.
    ext: SliceIndices,
}

// Including indices makes debugging more difficult.
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

// `tags` and `ext` should always be the same
// for a given `path`.
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

#[derive(Clone, Copy, Debug)]
struct SliceIndices(usize, usize);

#[derive(Clone, Copy, Debug)]
struct TagIndices(usize, usize);

impl From<TagIndices> for SliceIndices {
    fn from(value: TagIndices) -> Self {
        SliceIndices(value.0, value.1)
    }
}

impl TaggedPath {
    pub fn new<S>(path: S) -> Result<TaggedPath, TaggedPathError>
    where
        S: Into<String>,
    {
        let path = path.into();

        let mut tags = Vec::new();
        let mut tag_start = 0;
        let mut is_tag = true;
        for (i, c) in path.char_indices() {
            if i == tag_start {
                if SEPARATORS.contains(&c) {
                    // Tag is empty if `c` is separator.
                    return Err(TaggedPathError(path));
                }
                is_tag = c != TAG_IGNORE;
            } else if SEPARATORS.contains(&c) {
                if is_tag {
                    tags.push(TagIndices(tag_start, i));
                }
                if c == EXT_SEPARATOR {
                    let this = TaggedPath {
                        tags,
                        // Exclude separator from extension.
                        ext: SliceIndices(i + c.len_utf8(), path.len()),
                        path,
                    };
                    return if this.tags_unique() && this.ext_valid() {
                        Ok(this)
                    } else {
                        Err(TaggedPathError(this.path))
                    };
                } else {
                    // Skip this separator
                    // for the start of the next tag.
                    tag_start = i + c.len_utf8();
                }
            }
        }
        Err(TaggedPathError(path))
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

    pub fn from_tags<T, E>(tags: &FxHashSet<T>, ext: E) -> TaggedPath
    where
        T: AsRef<TagRef>,
        E: AsRef<ExtRef>,
    {
        let mut tag_indices = Vec::new();
        let mut start = 0;

        let inline_separator = INLINE_SEPARATOR.to_string(); // As of writing, Rust cannot directly convert const `char` to `&str`.
        let path = if tags.is_empty() {
            start = start + TAG_IGNORE.len_utf8() + EXT_SEPARATOR.len_utf8();
            format!("{TAG_IGNORE}{EXT_SEPARATOR}{}", ext.as_ref())
        } else {
            format!(
                "{}{EXT_SEPARATOR}{}",
                tags.iter().format_with(&inline_separator, |tag, f| {
                    let end = start + tag.as_ref().len();
                    tag_indices.push(TagIndices(start, end));
                    start = end + INLINE_SEPARATOR.len_utf8();
                    f(&tag.as_ref())
                }),
                ext.as_ref(),
            )
        };

        let ext = SliceIndices(start, start + ext.as_ref().len());

        Self {
            path,
            tags: tag_indices,
            ext,
        }
    }

    fn tags_unique(&self) -> bool {
        // A nested loop is faster than a hash-set
        // on small lists.
        for (i, tag) in self
            .tags()
            .take(self.tags_len().saturating_sub(1))
            .enumerate()
        {
            for other_tag in self.tags().skip(i + 1) {
                if tag == other_tag {
                    return false;
                }
            }
        }
        true
    }

    fn ext_valid(&self) -> bool {
        !self.ext().as_str().contains(DIR_SEPARATOR)
    }

    pub fn tags(&self) -> impl Iterator<Item = &TagRef> {
        self.tags
            .iter()
            .copied()
            .map(|x| TagRef::new(self.slice(x)))
    }

    pub fn tags_len(&self) -> usize {
        self.tags.len()
    }

    pub fn tags_is_empty(&self) -> bool {
        self.tags.is_empty()
    }

    // If this is made public,
    // it should have unit tests.
    pub(crate) fn ignored_tags(&self) -> impl Iterator<Item = &str> {
        self.as_path()
            .to_str()
            .expect("paths should be valid strings")
            .split_once(EXT_SEPARATOR)
            .expect("paths should contain EXT_SEPARATOR")
            .0
            .split([INLINE_SEPARATOR, DIR_SEPARATOR])
            .filter(|s| s.starts_with(TAG_IGNORE))
            .map(|s| {
                s.strip_prefix(TAG_IGNORE)
                    .expect("ignored tags should start with TAG_IGNORE")
            })
    }

    pub fn ext(&self) -> &ExtRef {
        ExtRef::new(self.slice(self.ext))
    }

    pub fn as_path(&self) -> &Path {
        self.path.as_ref()
    }

    pub fn into_path(self) -> PathBuf {
        self.path.into()
    }

    fn slice<T>(&self, x: T) -> &str
    where
        T: Into<SliceIndices>,
    {
        let x = x.into();
        // This is safe when used with already verified slices
        // from the same instance.
        unsafe { self.path.get_unchecked(x.0..x.1) }
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
    use std::collections::HashSet;

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
    fn new_returns_err_for_paths_with_empty_tags() {
        assert!(TaggedPath::new("-bar.baz").is_err());
        assert!(TaggedPath::new("foo-.baz").is_err());
        assert!(TaggedPath::new("/bar.baz").is_err());
        assert!(TaggedPath::new("foo/.baz").is_err());
        assert!(TaggedPath::new("foo-.baz").is_err());
    }

    #[test]
    fn new_returns_err_if_there_are_duplicate_tags() {
        assert!(TaggedPath::new("foo-foo.baz").is_err());
        assert!(TaggedPath::new("foo/foo.baz").is_err());
        assert!(TaggedPath::new("foo-bar/foo.baz").is_err());
        assert!(TaggedPath::new("bar/foo-foo.baz").is_err());
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
            TaggedPath::from_tags::<Tag, _>(&FxHashSet::default(), ext("x")).to_string(),
            "_.x"
        );
    }

    #[proptest]
    fn from_tags_matches_new(tags: HashSet<Tag>, ext: Ext) {
        let path = TaggedPath::from_tags(&tags.into_iter().collect(), ext);
        prop_assert_eq!(TaggedPath::from_path(path.as_path()).unwrap(), path);
    }

    #[proptest]
    fn tags_returns_all_tags(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.tags().collect::<Vec<_>>(), raw_path.tags);
    }

    #[proptest]
    fn ext_returns_ext(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.ext(), raw_path.ext.as_ref());
    }

    #[test]
    fn tags_starting_with_tag_ignore_are_ignored() {
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

    static MAYBE_TAGGED_PATH: Lazy<String> =
        Lazy::new(|| format!(r"\PC{{0,16}}{EXT_SEPARATOR}[a-z-_.]{{0,16}}",));
}
