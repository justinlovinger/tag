use std::{
    fmt,
    path::{Path, PathBuf},
};

use crate::{TagRef, INLINE_SEPARATOR, SEPARATORS, TAG_END};

#[derive(Clone, Debug)]
pub struct TaggedFile {
    path: String,
    /// Slice indices to get name from `path`,
    /// start inclusive
    /// and end exclusive.
    name: SliceIndices,
    /// Slice indices to get tags from `path`,
    /// start inclusive
    /// and end exclusive.
    tags: Vec<SliceIndices>,
}

impl PartialEq for TaggedFile {
    fn eq(&self, other: &Self) -> bool {
        // If `path` is equal,
        // `name` and `tags` should be too.
        self.path.eq(&other.path)
    }
}

#[derive(Clone, Copy, Debug)]
struct SliceIndices(usize, usize);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}`")]
pub struct HasTagError<T>(TaggedFile, T);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` lacks `{1}`")]
pub struct LacksTagError<T>(TaggedFile, T);

#[derive(Clone, Debug, PartialEq)]
pub struct MoveInstruction {
    pub from: PathBuf,
    pub to: PathBuf,
}

impl TaggedFile {
    pub fn new(path: String) -> Option<TaggedFile> {
        let mut tags = Vec::new();
        let mut tag_start = 0;
        for (i, c) in path.char_indices() {
            if SEPARATORS.contains(&c) {
                if i == tag_start {
                    return None;
                } else {
                    tags.push(SliceIndices(tag_start, i));
                }
                // This slice index is safe
                // because all separaters are one byte.
                tag_start = i + 1;
            } else if c == TAG_END && i == tag_start {
                return Some(TaggedFile {
                    // This slice index is safe
                    // because the tag-end character is one byte.
                    name: SliceIndices(i + 1, path.len()),
                    path,
                    tags,
                });
            }
        }
        None
    }

    pub fn name(&self) -> &str {
        self.slice(self.name)
    }

    pub fn tags(&self) -> impl Iterator<Item = &TagRef> {
        self.tags
            .iter()
            .copied()
            .map(|x| TagRef::new(self.slice(x)))
    }

    pub fn tags_str(&self) -> Option<&str> {
        // This is safe
        // because slice indices are at character bounds
        // and +1 is safe
        // because all separators are one byte.
        Some(unsafe {
            self.path
                .get_unchecked(self.tags.first()?.0..(self.tags.last()?.1 + 1))
        })
    }

    pub fn add_tag<T>(self, tag: T) -> Result<MoveInstruction, HasTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        if self.tags().any(|x| x == tag.as_ref()) {
            Err(HasTagError(self, tag))
        } else {
            Ok(MoveInstruction {
                to: format!(
                    "{}{}{}{}{}",
                    self.tags_str().unwrap_or(""),
                    tag.as_ref(),
                    INLINE_SEPARATOR,
                    TAG_END,
                    self.name()
                )
                .into(),
                from: self.into(),
            })
        }
    }

    pub fn del_tag<T>(self, tag: T) -> Result<MoveInstruction, LacksTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        let i = self.tags().zip(self.tags.iter()).find_map(|(tag_, i)| {
            if tag_ == tag.as_ref() {
                Some(i)
            } else {
                None
            }
        });
        match i {
            Some(i) => {
                Ok(MoveInstruction {
                    to: format!(
                        "{}{}",
                        // This is safe
                        // because we know `i.0` is the start of a character
                        unsafe { self.path.get_unchecked(..i.0) },
                        // This is safe
                        // because `i.1` is always a separator
                        // and every separator is one byte.
                        unsafe { self.path.get_unchecked(i.1 + 1..) },
                    )
                    .into(),
                    from: self.into(),
                })
            }
            None => Err(LacksTagError(self, tag)),
        }
    }

    fn slice(&self, x: SliceIndices) -> &str {
        // This is safe when used with already verified slices
        // from the same instance.
        unsafe { self.path.get_unchecked(x.0..x.1) }
    }
}

impl fmt::Display for TaggedFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

impl From<TaggedFile> for PathBuf {
    fn from(value: TaggedFile) -> Self {
        value.path.into()
    }
}

impl AsRef<Path> for TaggedFile {
    fn as_ref(&self) -> &Path {
        self.path.as_ref()
    }
}

impl AsRef<str> for TaggedFile {
    fn as_ref(&self) -> &str {
        self.path.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use lazy_static::lazy_static;
    use proptest::{
        prelude::{prop::collection::vec, *},
        test_runner::FileFailurePersistence,
    };
    use test_strategy::proptest;

    use crate::{testing::*, Tag};

    use super::*;

    #[test]
    fn new_returns_some_for_simple_tagged_files() {
        assert!(TaggedFile::new("foo-bar-_baz".to_owned()).is_some());
        assert!(TaggedFile::new("foo/bar/_baz".to_owned()).is_some());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_some_for_tagged_files(raw_file: RawTaggedFile) {
        prop_assert!(TaggedFile::new(raw_file.to_string()).is_some());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_none_for_non_tagged_files(s: String) {
        prop_assume!(
            !(s.starts_with(TAG_END)
                || SEPARATORS
                    .map(|c| format!("{}{}", c, TAG_END))
                    .iter()
                    .any(|ended_sep| s.contains(ended_sep)))
        );
        prop_assert!(TaggedFile::new(s).is_none());
    }

    #[test]
    fn new_returns_none_for_files_with_empty_tags() {
        assert!(TaggedFile::new("-bar-_baz".to_owned()).is_none());
        assert!(TaggedFile::new("foo--_baz".to_owned()).is_none());
        assert!(TaggedFile::new("/bar-_baz".to_owned()).is_none());
        assert!(TaggedFile::new("foo/-_baz".to_owned()).is_none());
        assert!(TaggedFile::new("foo-/_baz".to_owned()).is_none());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn name_returns_name(raw_file: RawTaggedFile) {
        let file = TaggedFile::new(raw_file.to_string()).unwrap();
        prop_assert_eq!(file.name(), raw_file.name);
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn tags_returns_all_tags(raw_file: RawTaggedFile) {
        let file = TaggedFile::new(raw_file.to_string()).unwrap();
        prop_assert_eq!(file.tags().collect::<Vec<_>>(), raw_file.tags);
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn tags_str_returns_string_of_all_tags_with_separators(raw_file: RawTaggedFile) {
        prop_assume!(!raw_file.tags.is_empty());
        let file = TaggedFile::new(raw_file.to_string()).unwrap();
        let path = raw_file.to_string();
        prop_assert_eq!(
            file.tags_str().unwrap(),
            path.strip_suffix(&raw_file.name)
                .unwrap()
                .strip_suffix(TAG_END)
                .unwrap()
        );
    }

    #[test]
    fn add_tag_returns_path_with_tag_added() {
        test_add_tag("foo-_bar", "baz", "foo-baz-_bar");
        test_add_tag("foo/_bar", "baz", "foo/baz-_bar");
    }

    fn test_add_tag(file: &str, tag: &str, expected_to: &str) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .add_tag(Tag::new(tag.to_owned()).unwrap()),
            Ok(MoveInstruction {
                from: file.into(),
                to: expected_to.into()
            })
        );
    }

    #[test]
    fn add_tag_returns_err_if_file_already_has_tag() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .add_tag(Tag::new("foo".to_owned()).unwrap())
            .is_err());
    }

    #[test]
    fn del_tag_returns_path_with_tag_removed() {
        test_del_tag("foo-_bar", "foo", "_bar");
        test_del_tag("foo/_bar", "foo", "_bar");
        test_del_tag("foo/baz-_bar", "foo", "baz-_bar");
        test_del_tag("foo-baz/_bar", "baz", "foo-_bar");
        test_del_tag("foo/baz/_bar", "baz", "foo/_bar");
    }

    fn test_del_tag(file: &str, tag: &str, expected_to: &str) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .del_tag(Tag::new(tag.to_owned()).unwrap()),
            Ok(MoveInstruction {
                from: file.into(),
                to: expected_to.into()
            })
        );
    }

    #[test]
    fn del_tag_returns_err_if_file_lacks_tag() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .del_tag(Tag::new("baz".to_owned()).unwrap())
            .is_err());
    }

    #[derive(Clone, Debug)]
    struct RawTaggedFile {
        name: String,
        tags: Vec<Tag>,
        seps: Vec<char>,
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

    lazy_static! {
        static ref SEPARATOR_REGEX: String = format!("[{}]", SEPARATORS_STRING.deref());
    }
}
