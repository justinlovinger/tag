use std::{borrow::Borrow, ops::Deref, path::Path, str::FromStr};

use derive_more::Display;
use ref_cast::{ref_cast_custom, RefCastCustom};

use crate::DIR_SEPARATOR;

#[derive(Debug, thiserror::Error)]
#[error("Invalid extension: `{0}`. Extensions cannot contain `{DIR_SEPARATOR}`.")]
pub struct ExtError(String);

#[derive(Clone, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ext(String);

#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash, RefCastCustom)]
#[repr(transparent)]
pub struct ExtRef(str);

impl Ext {
    pub fn new<S>(s: S) -> Result<Ext, ExtError>
    where
        S: Into<String>,
    {
        let s = s.into();
        if s.contains(DIR_SEPARATOR) {
            Err(ExtError(s))
        } else {
            Ok(Ext(s))
        }
    }

    pub fn as_path(&self) -> &Path {
        self.0.as_ref()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl FromStr for Ext {
    type Err = ExtError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

impl ExtRef {
    #[ref_cast_custom]
    pub(crate) const unsafe fn new_unchecked(s: &str) -> &Self;

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

impl Deref for Ext {
    type Target = ExtRef;

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl AsRef<ExtRef> for Ext {
    fn as_ref(&self) -> &ExtRef {
        self.borrow()
    }
}

impl Borrow<ExtRef> for Ext {
    fn borrow(&self) -> &ExtRef {
        // SAFETY: if `Ext` is valid, so is `ExtRef`.
        unsafe { ExtRef::new_unchecked(self.0.as_str()) }
    }
}

impl Borrow<ExtRef> for &Ext {
    fn borrow(&self) -> &ExtRef {
        (*self).borrow()
    }
}

impl<'a> From<&'a ExtRef> for Ext {
    fn from(value: &'a ExtRef) -> Self {
        value.to_owned()
    }
}

impl AsRef<ExtRef> for ExtRef {
    fn as_ref(&self) -> &ExtRef {
        self
    }
}

impl ToOwned for ExtRef {
    type Owned = Ext;

    fn to_owned(&self) -> Self::Owned {
        Ext(self.0.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_returns_ok_for_valid_extensions() {
        test_new("foo");
        test_new("bar");
        test_new("🙂");
        test_new("foo.");
        test_new("has_underscore");
        test_new("has-dash");
        test_new("");
    }

    fn test_new(ext: &str) {
        assert_eq!(Ext::new(ext).unwrap().to_string(), ext);
    }

    #[test]
    fn new_returns_err_for_strings_containing_a_directory_separator() {
        assert!(Ext::new(format!("foo{DIR_SEPARATOR}bar")).is_err());
    }
}
