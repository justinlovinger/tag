use lazy_static::lazy_static;

use crate::{SEPARATORS, TAG_END};

lazy_static! {
    pub(crate) static ref SEPARATORS_STRING: String = SEPARATORS.iter().collect();
    pub(crate) static ref SEPARATORS_AND_ENDS: String =
        format!("{}{}", *SEPARATORS_STRING, TAG_END);
    pub(crate) static ref TAG_REGEX: String =
        format!("[^{}][^{}]*", *SEPARATORS_AND_ENDS, *SEPARATORS_STRING);
}
