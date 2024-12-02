use std::{borrow::Borrow, ops::Deref, path::Path, str::FromStr};

use derive_more::Display;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::DIR_SEPARATOR;

#[derive(Debug, thiserror::Error)]
#[error("Invalid name: `{0}`. Names cannot contain `{DIR_SEPARATOR}`.")]
pub struct NameError(String);

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(String);

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct NameRef(str);

impl Name {
    pub fn new(s: String) -> Result<Name, NameError> {
        if s.is_empty() || s.contains(DIR_SEPARATOR) {
            Err(NameError(s))
        } else {
            Ok(Name(s))
        }
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl FromStr for Name {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s.to_owned())
    }
}

impl NameRef {
    #[ref_cast_custom]
    pub(crate) const fn new(s: &str) -> &Self;

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Deref for Name {
    type Target = NameRef;

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl AsRef<NameRef> for Name {
    fn as_ref(&self) -> &NameRef {
        self.borrow()
    }
}

impl Borrow<NameRef> for Name {
    fn borrow(&self) -> &NameRef {
        NameRef::new(self.0.as_str())
    }
}

impl<'a> From<&'a NameRef> for Name {
    fn from(value: &'a NameRef) -> Self {
        value.to_owned()
    }
}

impl AsRef<NameRef> for NameRef {
    fn as_ref(&self) -> &NameRef {
        self
    }
}

impl ToOwned for NameRef {
    type Owned = Name;

    fn to_owned(&self) -> Self::Owned {
        Name(self.0.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_returns_some_for_valid_names() {
        test_new("foo");
        test_new("bar");
        test_new("🙂");
        test_new("foo.");
        test_new("has_underscore");
        test_new("has-dash");
    }

    fn test_new(name: &str) {
        assert_eq!(Name::new(name.to_owned()).unwrap().to_string(), name);
    }

    #[test]
    fn new_returns_none_for_empty_strings() {
        assert!(Name::new(String::new()).is_err());
    }

    #[test]
    fn new_returns_none_for_strings_containing_a_directory_separator() {
        assert!(Name::new(format!("foo{DIR_SEPARATOR}bar")).is_err());
    }
}
