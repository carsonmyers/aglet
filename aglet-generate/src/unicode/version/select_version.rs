use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use eyre::{eyre, WrapErr};
use nom::Parser;
use serde::{Deserialize, Serialize};

use crate::parse;
use crate::unicode::UnicodeVersion;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SelectVersion {
    Latest,
    Draft,
    Version(UnicodeVersion),
    Hash(String),
}

impl SelectVersion {
    pub fn try_version_from_str<S: AsRef<str>>(value: S) -> eyre::Result<Self> {
        Ok(Self::Version(UnicodeVersion::from_str(value.as_ref())?))
    }

    pub fn try_hash_from_str(value: &str) -> eyre::Result<Self> {
        use crate::parse::hex_digits;
        use nom::combinator::{all_consuming, map};

        let mut parser = map(all_consuming(hex_digits), Self::hash_from_str);
        parse::finish(parser.parse(value)).wrap_err("invalid version hash")
    }

    fn hash_from_str(hash: &str) -> Self {
        Self::Hash(String::from(hash))
    }

    pub fn is_version(&self) -> bool {
        matches!(self, SelectVersion::Version(_))
    }
}

impl Display for SelectVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Latest => write!(f, "Latest"),
            Self::Draft => write!(f, "Draft"),
            Self::Version(v) => write!(f, "{}", v),
            Self::Hash(h) => write!(f, "{}", &h.to_string()[..7]),
        }
    }
}

impl FromStr for SelectVersion {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "latest" => Ok(SelectVersion::Latest),
            "draft" => Ok(SelectVersion::Draft),
            value => {
                let version = Self::try_version_from_str(value);
                let version_or_hash = version.or_else(|_| Self::try_hash_from_str(value));
                let version_or_hash = version_or_hash.or_else(|_| {
                    Err(eyre!(
                        "invalid version selection {}: must be a valid value or hash",
                        value
                    ))
                })?;

                Ok(version_or_hash)
            },
        }
    }
}

impl From<UnicodeVersion> for SelectVersion {
    fn from(value: UnicodeVersion) -> Self {
        Self::Version(value)
    }
}
