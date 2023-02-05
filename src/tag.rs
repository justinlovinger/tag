use crate::{SEPARATORS, TAG_END};

#[derive(Clone, Debug)]
pub struct Tag(String);

impl Tag {
    pub fn new(s: String) -> Option<Tag> {
        if s.starts_with(TAG_END) || s.chars().any(|c| SEPARATORS.iter().any(|sep| &c == sep)) {
            None
        } else {
            Some(Tag(s))
        }
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use proptest::{prelude::*, test_runner::FileFailurePersistence};
    use test_strategy::proptest;

    use crate::testing::*;

    use super::*;

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_none_for_strings_starting_with_tag_end(
        #[strategy(STARTS_WITH_TAG_END_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_none_for_strings_containing_a_separator(
        #[strategy(TAG_WITH_SEP_REGEX.as_str())] s: String,
    ) {
        prop_assert!(Tag::new(s).is_none());
    }

    #[proptest(failure_persistence = Some(Box::new(FileFailurePersistence::Off)))]
    fn new_returns_some_for_valid_tags(#[strategy(TAG_REGEX.as_str())] s: String) {
        prop_assert!(Tag::new(s).is_some());
    }

    lazy_static! {
        static ref STARTS_WITH_TAG_END_REGEX: String = format!(r"{}\PC*", TAG_END);
        static ref TAG_WITH_SEP_REGEX: String = format!(r"\PC*[{}]\PC*", *SEPARATORS_STRING);
    }
}
