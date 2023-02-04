#[derive(Clone, Debug)]
pub struct TaggedFile {
    file: String,
    name: SliceIndices,
    tags: Vec<SliceIndices>,
}

#[derive(Clone, Copy, Debug)]
struct SliceIndices(usize, usize);

impl TaggedFile {
    pub fn new(file: String) -> Option<TaggedFile> {
        let mut tags = Vec::new();
        let mut tag_start = 0;
        for (i, c) in file.char_indices() {
            if c == '-' || c == '/' {
                if i == tag_start {
                    return None;
                } else {
                    tags.push(SliceIndices(tag_start, i));
                }
                // This slice index is safe
                // because all separaters are one byte.
                tag_start = i + 1;
            } else if c == '_' && i == tag_start {
                return Some(TaggedFile {
                    // This slice index is safe
                    // because the tag-end character is one byte.
                    name: SliceIndices(i + 1, file.len()),
                    file,
                    tags,
                });
            }
        }
        None
    }

    pub fn name(&self) -> &str {
        self.slice(self.name)
    }

    pub fn tags(&self) -> impl Iterator<Item = &str> {
        self.tags.iter().copied().map(|x| self.slice(x))
    }

    fn slice(&self, x: SliceIndices) -> &str {
        // This is safe when used with already verified slfile: s
        // from the same instance.
        unsafe { self.file.get_unchecked(x.0..x.1) }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use proptest::{
        prelude::{prop::collection::vec, *},
        test_runner::FileFailurePersistence,
    };
    use test_strategy::proptest;

    use super::*;

    #[test]
    fn new_tagged_file_returns_some_for_simple_tagged_files() {
        assert!(TaggedFile::new("foo-bar-_baz".to_owned()).is_some());
        assert!(TaggedFile::new("foo/bar/_baz".to_owned()).is_some());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_tagged_file_returns_some_for_tagged_files(raw_file: RawTaggedFile) {
        assert!(TaggedFile::new(raw_file.to_string()).is_some());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_tagged_file_returns_none_for_non_tagged_files(s: String) {
        prop_assume!(!(s.starts_with('_') || s.contains("-_") || s.contains("/_")));
        prop_assert!(TaggedFile::new(s).is_none());
    }

    #[test]
    fn new_tagged_file_returns_none_for_files_with_empty_tags() {
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

    #[derive(Clone, Debug)]
    struct RawTaggedFile {
        name: String,
        tags: Vec<String>,
        seps: Vec<char>,
    }

    impl fmt::Display for RawTaggedFile {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (tag, sep) in self.tags.iter().zip(self.seps.iter()) {
                tag.fmt(f)?;
                sep.fmt(f)?;
            }
            write!(f, "_{}", self.name)
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
                        vec(r"[^-/_]([^-/]*[^-/_])?", len),
                        vec(r"[-/]", len).prop_map(|xs| {
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
}
