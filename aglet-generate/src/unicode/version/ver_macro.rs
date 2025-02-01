use crate::unicode::{SelectVersion, UnicodeVersion, UnicodeVersionWithUpdate};

#[macro_export]
macro_rules! ver {
    ($maj:literal , $min:literal , $update:literal) => {
        $crate::unicode::version::ver_macro::construct_version($maj, $min, $update)
    };
    ($maj:literal , $min:literal) => {
        ver!($maj, $min, 0)
    };
    ($maj:literal) => {
        ver!($maj, 0, 0)
    };
    (LATEST) => {
        $crate::unicode::vs::SelectVersion::Latest
    };
    (latest) => {
        ver!(LATEST)
    };
    (Latest) => {
        ver!(LATEST)
    };
    (DRAFT) => {
        $crate::unicode::vs::SelectVersion::Draft
    };
    (draft) => {
        ver!(DRAFT)
    };
    (Draft) => {
        ver!(DRAFT)
    };
    () => {
        ver!(0, 0, 0)
    };
}

pub trait VersionConstructor {
    fn version(maj: u8, min: u8, patch: u8) -> Self;
}

impl VersionConstructor for UnicodeVersion {
    fn version(maj: u8, min: u8, patch: u8) -> Self {
        UnicodeVersion(maj, min, patch)
    }
}

impl VersionConstructor for SelectVersion {
    fn version(maj: u8, min: u8, patch: u8) -> Self {
        SelectVersion::Version(UnicodeVersion(maj, min, patch))
    }
}

impl VersionConstructor for UnicodeVersionWithUpdate {
    fn version(maj: u8, min: u8, update: u8) -> Self {
        UnicodeVersionWithUpdate(maj, min, update)
    }
}

pub fn construct_version<T: VersionConstructor>(maj: u8, min: u8, update: u8) -> T {
    T::version(maj, min, update)
}
