use std::{
    fmt,
    hash::Hash,
    iter::once,
    path::{Path, PathBuf},
};

use auto_enums::auto_enum;

use crate::{TagRef, DIR_SEPARATOR, INLINE_SEPARATOR, SEPARATORS, TAG_END};

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
    tags: Vec<TagIndices>,
}

// `name` and `tags` should always be the same
// for a given `path`.
impl Eq for TaggedFile {}
impl PartialEq for TaggedFile {
    fn eq(&self, other: &Self) -> bool {
        self.path.eq(&other.path)
    }
}
impl Ord for TaggedFile {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.path.cmp(&other.path)
    }
}
impl PartialOrd for TaggedFile {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Hash for TaggedFile {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    EnsureDirectory(PathBuf),
    Move(MoveOp),
    DeleteDirectoryIfEmpty(PathBuf),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MoveOp {
    pub from: PathBuf,
    pub to: PathBuf,
}

impl From<MoveOp> for Op {
    fn from(value: MoveOp) -> Self {
        Op::Move(value)
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` is not a tagged file")]
pub struct NewError(String);

impl NewError {
    pub fn into_path(self) -> PathBuf {
        self.0.into()
    }
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum InlineTagError<T> {
    LacksTag(#[from] LacksTagError<T>),
    AlreadyInline(#[from] AlreadyInlineError<T>),
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum UninlineTagError<T> {
    LacksTag(#[from] LacksTagError<T>),
    AlreadyUninline(#[from] AlreadyUninlineError<T>),
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}`")]
pub struct HasTagError<T>(TaggedFile, T);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` lacks `{1}`")]
pub struct LacksTagError<T>(TaggedFile, T);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}` inline")]
pub struct AlreadyInlineError<T>(TaggedFile, T);

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("`{0}` already has `{1}` as a directory-tag")]
pub struct AlreadyUninlineError<T>(TaggedFile, T);

impl TaggedFile {
    pub fn new(path: String) -> Result<TaggedFile, NewError> {
        let mut tags = Vec::new();
        let mut tag_start = 0;
        for (i, c) in path.char_indices() {
            if SEPARATORS.contains(&c) {
                if i == tag_start {
                    return Err(NewError(path));
                } else {
                    tags.push(TagIndices(tag_start, i));
                }
                // Skip this separator
                // for the start of the next tag.
                tag_start = i + c.len_utf8();
            } else if c == TAG_END && i == tag_start {
                return Ok(TaggedFile {
                    // Do not include tag-end in name.
                    name: SliceIndices(i + c.len_utf8(), path.len()),
                    path,
                    tags,
                });
            }
        }
        Err(NewError(path))
    }

    pub fn from_path(path: PathBuf) -> Result<TaggedFile, NewError> {
        Self::new(
            path.into_os_string()
                .into_string()
                .expect("path should contain valid unicode"),
        )
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

    pub fn add_inline_tag<T>(self, tag: T) -> Result<impl Iterator<Item = Op>, HasTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        if self.tags().any(|x| x == tag.as_ref()) {
            Err(HasTagError(self, tag))
        } else {
            Ok(once(Op::Move(MoveOp {
                to: format!(
                    "{}{}{}{}{}",
                    self.tags_str(),
                    tag.as_ref(),
                    INLINE_SEPARATOR,
                    TAG_END,
                    self.name()
                )
                .into(),
                from: self.into(),
            })))
        }
    }

    pub fn del_tag<T>(self, tag: T) -> Result<impl Iterator<Item = Op>, LacksTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        let x = self.indices_of(&tag);
        match x {
            Some(x) => Ok(once(Op::Move(MoveOp {
                to: format!("{}{}", self.path_up_to(x), self.path_after(x)).into(),
                from: self.into(),
            }))),
            None => Err(LacksTagError(self, tag)),
        }
    }

    #[auto_enum]
    pub fn inline_tag<T>(self, tag: T) -> Result<impl Iterator<Item = Op>, InlineTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        let tag_indices = self.indices_of(&tag);
        match tag_indices {
            Some(tag_indices) => {
                if self.separator_of(tag_indices) == INLINE_SEPARATOR {
                    return Err(AlreadyInlineError(self, tag).into());
                }

                let dir = self.path_up_to_including(tag_indices).to_owned();
                let remaining_path = self.path_after(tag_indices);
                let to_path = format!("{}{}{}", dir, INLINE_SEPARATOR, remaining_path);

                Ok(
                    #[auto_enum(Iterator)]
                    match remaining_path.find(DIR_SEPARATOR).map(|mut next_dir| {
                        // We add the length of everything before `remaining_path`
                        // so index refers to original string.
                        // We will add the length of the separator later
                        // because it depends on which string we slice.
                        next_dir += dir.as_bytes().len();
                        (
                            // These are safe
                            // assuming we correctly constructed `next_dir` above.
                            Op::EnsureDirectory(
                                unsafe {
                                    to_path.get_unchecked(..next_dir + INLINE_SEPARATOR.len_utf8())
                                }
                                .into(),
                            ),
                            Op::DeleteDirectoryIfEmpty(
                                unsafe {
                                    self.path
                                        .get_unchecked(..next_dir + DIR_SEPARATOR.len_utf8())
                                }
                                .into(),
                            ),
                        )
                    }) {
                        Some((pre_move, pre_del)) => [
                            pre_move,
                            Op::Move(MoveOp {
                                from: self.into(),
                                to: to_path.into(),
                            }),
                            pre_del,
                            Op::DeleteDirectoryIfEmpty(dir.into()),
                        ]
                        .into_iter(),
                        None => [
                            Op::Move(MoveOp {
                                from: self.into(),
                                to: to_path.into(),
                            }),
                            Op::DeleteDirectoryIfEmpty(dir.into()),
                        ]
                        .into_iter(),
                    },
                )
            }
            None => Err(LacksTagError(self, tag).into()),
        }
    }

    pub fn uninline_tag<T>(self, tag: T) -> Result<impl Iterator<Item = Op>, UninlineTagError<T>>
    where
        T: AsRef<TagRef>,
    {
        let tag_indices = self.indices_of(&tag);
        match tag_indices {
            Some(tag_indices) => {
                if self.separator_of(tag_indices) == DIR_SEPARATOR {
                    return Err(AlreadyUninlineError(self, tag).into());
                }

                let dir = self.path_up_to_including(tag_indices);
                let remaining_path = self.path_after(tag_indices);
                let to_path = format!("{}{}{}", dir, DIR_SEPARATOR, remaining_path);

                Ok([
                    Op::EnsureDirectory(dir.into()),
                    Op::Move(MoveOp {
                        from: self.into(),
                        to: to_path.into(),
                    }),
                ]
                .into_iter())
            }
            None => Err(LacksTagError(self, tag).into()),
        }
    }

    /// Return indices corresponding to the given tag
    /// if this file has the tag.
    fn indices_of<T>(&self, tag: T) -> Option<TagIndices>
    where
        T: AsRef<TagRef>,
    {
        self.tags().zip(self.tags.iter()).find_map(|(tag_, i)| {
            if tag_ == tag.as_ref() {
                Some(*i)
            } else {
                None
            }
        })
    }

    /// Return the path
    /// from the beginning of the file
    /// to the start of the tag or name
    /// corresponding to the given indices.
    fn path_up_to<T>(&self, x: T) -> &str
    where
        T: Into<SliceIndices>,
    {
        let x = x.into();
        // This is safe
        // because we know `x.0` is the start of a character
        unsafe { self.path.get_unchecked(..x.0) }
    }

    /// Return the path
    /// from the beginning of the file
    /// to the end of tag or name
    /// corresponding to the given indices.
    /// when used on a tag,
    /// this does not include the separator
    /// following the tag.
    fn path_up_to_including<T>(&self, x: T) -> &str
    where
        T: Into<SliceIndices>,
    {
        let x = x.into();
        // This is safe
        // because we know `x.1` is the start of a character.
        unsafe { self.path.get_unchecked(..x.1) }
    }

    /// Return the path
    /// from the end of the tag
    /// corresponding to the given indices
    /// to the end of the file.
    /// This does not include the tag
    /// or the separator following the tag.
    fn path_after(&self, x: TagIndices) -> &str {
        // This is safe
        // because `x.1` is always a separator
        unsafe {
            self.path
                .get_unchecked(x.1 + self.separator_of(x).len_utf8()..)
        }
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
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::{testing::*, Tag};

    use super::*;

    #[test]
    fn new_returns_some_for_simple_tagged_files() {
        assert!(TaggedFile::new("foo-bar-_baz".to_owned()).is_ok());
        assert!(TaggedFile::new("foo/bar/_baz".to_owned()).is_ok());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_some_for_tagged_files(raw_file: RawTaggedFile) {
        prop_assert!(TaggedFile::new(raw_file.to_string()).is_ok());
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
        prop_assert!(TaggedFile::new(s).is_err());
    }

    #[test]
    fn new_returns_none_for_files_with_empty_tags() {
        assert!(TaggedFile::new("-bar-_baz".to_owned()).is_err());
        assert!(TaggedFile::new("foo--_baz".to_owned()).is_err());
        assert!(TaggedFile::new("/bar-_baz".to_owned()).is_err());
        assert!(TaggedFile::new("foo/-_baz".to_owned()).is_err());
        assert!(TaggedFile::new("foo-/_baz".to_owned()).is_err());
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
        let file = TaggedFile::new(raw_file.to_string()).unwrap();
        let path = raw_file.to_string();
        prop_assert_eq!(
            file.tags_str(),
            path.strip_suffix(&raw_file.name)
                .unwrap()
                .strip_suffix(TAG_END)
                .unwrap()
        );
    }

    #[test]
    fn add_inline_tag_returns_path_with_tag_added() {
        test_add_inline_tag("foo-_bar", "baz", "foo-baz-_bar");
        test_add_inline_tag("foo/_bar", "baz", "foo/baz-_bar");
    }

    fn test_add_inline_tag(file: &str, tag: &str, expected_to: &str) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .add_inline_tag(Tag::new(tag.to_owned()).unwrap())
                .map(Vec::from_iter),
            Ok(vec![Op::Move(MoveOp {
                from: file.into(),
                to: expected_to.into()
            })])
        );
    }

    #[test]
    fn add_inline_tag_returns_err_if_file_already_has_tag() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .add_inline_tag(Tag::new("foo".to_owned()).unwrap())
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
                .del_tag(Tag::new(tag.to_owned()).unwrap())
                .map(Vec::from_iter),
            Ok(vec![Op::Move(MoveOp {
                from: file.into(),
                to: expected_to.into()
            })])
        );
    }

    #[test]
    fn del_tag_returns_err_if_file_lacks_tag() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .del_tag(Tag::new("baz".to_owned()).unwrap())
            .is_err());
    }

    #[test]
    fn inline_tag_returns_path_with_tag_inline() {
        test_inline_tag("foo/_bar", "foo", "foo-_bar", "foo");
        test_inline_tag("foo/baz-_bar", "foo", "foo-baz-_bar", "foo");
        test_inline_tag("foo/baz/_bar", "baz", "foo/baz-_bar", "foo/baz");
    }

    fn test_inline_tag(file: &str, tag: &str, expected_to: &str, expected_del_dir: &str) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .inline_tag(Tag::new(tag.to_owned()).unwrap())
                .map(Vec::from_iter),
            Ok(vec![
                Op::Move(MoveOp {
                    from: file.into(),
                    to: expected_to.into()
                }),
                Op::DeleteDirectoryIfEmpty(expected_del_dir.into()),
            ])
        );
    }

    #[test]
    fn inline_tag_ensures_directory_exists_if_directory_tags_are_merged() {
        test_inline_tag_with_ensure_dir(
            "foo/baz/_bar",
            "foo",
            "foo-baz",
            "foo-baz/_bar",
            ["foo/baz", "foo"],
        );
        test_inline_tag_with_ensure_dir(
            "a/foo/baz/_bar",
            "foo",
            "a/foo-baz",
            "a/foo-baz/_bar",
            ["a/foo/baz", "a/foo"],
        );
        // Note,
        // this should also delete tag-directories in next directory:
        // ```
        // test_inline_tag_with_ensure_dir(
        //     "a/foo/baz/b/_bar",
        //     "foo",
        //     "a/foo-baz/b",
        //     "a/foo-baz/b/_bar",
        //     ["a/foo/baz/b", "a/foo/baz", "a/foo"],
        // );
        // ```
    }

    fn test_inline_tag_with_ensure_dir(
        file: &str,
        tag: &str,
        expected_ensure_dir: &str,
        expected_to: &str,
        expected_del_dir: [&str; 2],
    ) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .inline_tag(Tag::new(tag.to_owned()).unwrap())
                .map(Vec::from_iter),
            Ok(vec![
                Op::EnsureDirectory(expected_ensure_dir.into()),
                Op::Move(MoveOp {
                    from: file.into(),
                    to: expected_to.into()
                }),
                Op::DeleteDirectoryIfEmpty(expected_del_dir[0].into()),
                Op::DeleteDirectoryIfEmpty(expected_del_dir[1].into()),
            ])
        );
    }

    #[test]
    fn inline_tag_returns_err_if_file_lacks_tag() {
        assert!(TaggedFile::new("foo/_bar".to_owned())
            .unwrap()
            .inline_tag(Tag::new("baz".to_owned()).unwrap())
            .is_err());
    }

    #[test]
    fn inline_tag_returns_err_if_tag_is_already_inline() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .inline_tag(Tag::new("foo".to_owned()).unwrap())
            .is_err());
    }

    #[test]
    fn uninline_tag_returns_path_with_tag_as_dir() {
        test_uninline_tag("foo-_bar", "foo", "foo/_bar", "foo");
        test_uninline_tag("foo-baz-_bar", "foo", "foo/baz-_bar", "foo");
        test_uninline_tag("foo/baz-_bar", "baz", "foo/baz/_bar", "foo/baz");
    }

    fn test_uninline_tag(file: &str, tag: &str, expected_to: &str, expected_mk_dir: &str) {
        assert_eq!(
            TaggedFile::new(file.to_owned())
                .unwrap()
                .uninline_tag(Tag::new(tag.to_owned()).unwrap())
                .map(Vec::from_iter),
            Ok(vec![
                Op::EnsureDirectory(expected_mk_dir.into()),
                Op::Move(MoveOp {
                    from: file.into(),
                    to: expected_to.into()
                }),
            ])
        );
    }

    #[test]
    fn uninline_tag_returns_err_if_file_lacks_tag() {
        assert!(TaggedFile::new("foo-_bar".to_owned())
            .unwrap()
            .uninline_tag(Tag::new("baz".to_owned()).unwrap())
            .is_err());
    }

    #[test]
    fn uninline_tag_returns_err_if_tag_is_already_dir() {
        assert!(TaggedFile::new("foo/_bar".to_owned())
            .unwrap()
            .uninline_tag(Tag::new("foo".to_owned()).unwrap())
            .is_err());
    }
}
