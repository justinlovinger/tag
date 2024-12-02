use std::{
    fmt,
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::{NameRef, TagRef, DIR_SEPARATOR, INLINE_SEPARATOR, SEPARATORS, TAG_END};

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[error("`{0}` is not a tagged path: tagged paths must contain zero or more unique tags ended by `{INLINE_SEPARATOR}` or `{DIR_SEPARATOR}` with the tagging portion ended by `{TAG_END}`")]
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
    /// Slice indices to get name from `path`,
    /// start inclusive
    /// and end exclusive.
    name: SliceIndices,
    /// Slice indices to get tags from `path`,
    /// start inclusive
    /// and end exclusive.
    tags: Vec<TagIndices>,
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

// `name` and `tags` should always be the same
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
    pub fn new(path: String) -> Result<TaggedPath, TaggedPathError> {
        let mut tags = Vec::new();
        let mut tag_start = 0;
        for (i, c) in path.char_indices() {
            if i == tag_start {
                if c == TAG_END {
                    let this = TaggedPath {
                        // Exclude tag-end from name.
                        name: SliceIndices(i + c.len_utf8(), path.len()),
                        path,
                        tags,
                    };
                    return if this.tags_unique() && this.name_valid() {
                        Ok(this)
                    } else {
                        Err(TaggedPathError(this.path))
                    };
                } else if SEPARATORS.contains(&c) || c == '.' {
                    // Tag is empty if `c` is separator.
                    return Err(TaggedPathError(path));
                }
            } else if SEPARATORS.contains(&c) {
                tags.push(TagIndices(tag_start, i));
                // Skip this separator
                // for the start of the next tag.
                tag_start = i + c.len_utf8();
            }
        }
        Err(TaggedPathError(path))
    }

    pub fn from_path(path: PathBuf) -> Result<TaggedPath, TaggedPathError> {
        Self::new(
            path.into_os_string()
                .into_string()
                .expect("path should contain valid unicode"),
        )
    }

    pub fn from_tags<T, N>(tags: &FxHashSet<T>, name: N) -> TaggedPath
    where
        T: AsRef<TagRef>,
        N: AsRef<NameRef>,
    {
        let mut tag_indices = Vec::new();
        let mut start = 0;

        let path = format!(
            "{}{TAG_END}{}",
            tags.iter().format_with("", |tag, f| {
                let end = start + tag.as_ref().len();
                tag_indices.push(TagIndices(start, end));
                start = end + 1;

                f(&tag.as_ref())?;
                f(&INLINE_SEPARATOR)?;
                Ok(())
            }),
            name.as_ref(),
        );

        let start = start + 1; // Offset for tag-end character.
        let name = SliceIndices(start, start + name.as_ref().len());

        Self {
            path,
            name,
            tags: tag_indices,
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

    fn name_valid(&self) -> bool {
        !self.name().as_str().contains(DIR_SEPARATOR)
    }

    pub fn name(&self) -> &NameRef {
        NameRef::new(self.slice(self.name))
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

    pub fn tags_str(&self) -> &str {
        match (self.tags.first(), self.tags.last()) {
            // This is safe
            // because slice indices are at character bounds.
            (Some(x), Some(y)) => unsafe {
                self.path
                    .get_unchecked(x.0..(y.1 + self.separator_of(*y).len_utf8()))
            },
            _ => "",
        }
    }

    pub fn as_path(&self) -> &Path {
        self.path.as_ref()
    }

    pub fn into_path(self) -> PathBuf {
        self.path.into()
    }

    /// Return the separator
    /// following the tag
    /// corresponding to the given indices.
    fn separator_of(&self, x: TagIndices) -> char {
        // This is safe
        // because all tag-slice indices are in `path`.
        char::from(*unsafe { self.path.as_bytes().get_unchecked(x.1) })
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
        Self::new(s.to_owned())
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

    use crate::{testing::*, Name, Tag};

    use super::*;

    #[test]
    fn new_returns_ok_for_tagged_paths() {
        assert!(TaggedPath::new("foo-bar-_baz".to_owned()).is_ok());
        assert!(TaggedPath::new("foo/bar/_baz".to_owned()).is_ok());
        assert!(TaggedPath::new("🙂/🙁-_baz".to_owned()).is_ok());
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

    #[proptest]
    fn new_returns_err_for_non_tagged_paths(s: String) {
        prop_assume!(
            !(s.starts_with(TAG_END)
                || SEPARATORS
                    .map(|c| format!("{}{}", c, TAG_END))
                    .iter()
                    .any(|ended_sep| s.contains(ended_sep)))
        );
        prop_assert!(TaggedPath::new(s).is_err());
    }

    #[test]
    fn new_returns_err_for_paths_with_empty_tags() {
        assert!(TaggedPath::new("-bar-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo--_baz".to_owned()).is_err());
        assert!(TaggedPath::new("/bar-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo/-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo-/_baz".to_owned()).is_err());
    }

    #[test]
    fn new_returns_err_for_tags_starting_with_dot() {
        assert!(TaggedPath::new(".-_baz".to_owned()).is_err());
        assert!(TaggedPath::new(".bar-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo-.-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo-.bar-_baz".to_owned()).is_err());
    }

    #[test]
    fn new_returns_err_if_there_are_duplicate_tags() {
        assert!(TaggedPath::new("foo-foo-_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo/foo/_baz".to_owned()).is_err());
        assert!(TaggedPath::new("foo-bar/foo/_baz".to_owned()).is_err());
        assert!(TaggedPath::new("bar/foo-foo/_baz".to_owned()).is_err());
    }

    #[test]
    fn new_returns_err_if_name_contains_dir_separator() {
        assert!(TaggedPath::new("foo-_baz/biz".to_owned()).is_err());
        assert!(TaggedPath::new("foo/_/".to_owned()).is_err());
        assert!(TaggedPath::new("foo/_/baz".to_owned()).is_err());
    }

    #[proptest]
    fn from_tags_matches_new(tags: HashSet<Tag>, name: Name) {
        let path = TaggedPath::from_tags(&tags.into_iter().collect(), name);
        prop_assert_eq!(
            TaggedPath::from_path(path.as_path().to_owned()).unwrap(),
            path
        );
    }

    #[proptest]
    fn name_returns_name(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.name(), raw_path.name.as_ref());
    }

    #[proptest]
    fn tags_returns_all_tags(raw_path: RawTaggedPath) {
        let path = TaggedPath::new(raw_path.to_string()).unwrap();
        prop_assert_eq!(path.tags().collect::<Vec<_>>(), raw_path.tags);
    }

    #[proptest]
    fn tags_str_returns_string_of_all_tags_with_separators(raw_path: RawTaggedPath) {
        let tagged_path = TaggedPath::new(raw_path.to_string()).unwrap();
        let path = raw_path.to_string();
        prop_assert_eq!(
            tagged_path.tags_str(),
            path.strip_suffix(raw_path.name.as_str())
                .unwrap()
                .strip_suffix(TAG_END)
                .unwrap()
        );
    }

    static MAYBE_TAGGED_PATH: Lazy<String> = Lazy::new(|| {
        format!(
            r"\PC{{0,16}}[{}]{TAG_END}[a-z-_.]{{0,16}}",
            *SEPARATORS_STRING
        )
    });
}
