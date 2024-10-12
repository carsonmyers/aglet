use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;
use std::str::FromStr;

use crate::parse;
use eyre::{eyre, Error, WrapErr};
use nom::Parser;
use serde::{Deserialize, Deserializer, Serialize};
use tracing::warn;

const MIN_MODERN_VERSION: UnicodeVersion = UnicodeVersion(4, 1, 0);

#[derive(Hash, Copy, Clone, Default, PartialOrd, Ord, PartialEq, Eq)]
pub struct UnicodeVersion(u8, u8, u8);

impl UnicodeVersion {
    pub fn new(maj: u8, min: u8, update: u8) -> Self {
        Self(maj, min, update)
    }

    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::branch::alt;
        use nom::combinator::all_consuming;

        all_consuming(alt((Self::parse_update_version, Self::parse_version))).parse(input)
    }

    pub fn parse_update_version(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::{map, map_res, opt};
        use nom::sequence::{pair, preceded, separated_pair};

        map(
            pair(
                separated_pair(
                    map_res(digit1, str::parse),
                    tag("."),
                    map_res(digit1, str::parse),
                ),
                preceded(tag("-UPDATE"), opt(map_res(digit1, str::parse))),
            ),
            |((x, y), z)| Self(x, y, z.unwrap_or_default()),
        )
        .parse(input)
    }

    pub fn parse_version(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::{map, map_res, opt};
        use nom::sequence::{preceded, tuple};

        map(
            (
                map_res(digit1, str::parse),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
            ),
            |(x, y, z)| Self(x, y.unwrap_or_default(), z.unwrap_or_default()),
        )
        .parse(input)
    }

    pub fn remote_dir(&self) -> String {
        match self {
            Self(x, y, z) if self < &MIN_MODERN_VERSION && z == &0 => {
                format!("Public/{}.{}-Update", x, y)
            },
            Self(x, y, z) if self < &MIN_MODERN_VERSION => {
                format!("Public/{}.{}-Update{}", x, y, z)
            },
            Self(x, y, z) => {
                format!("Public/{}.{}.{}", x, y, z)
            },
        }
    }

    pub fn filename<S: AsRef<str>>(&self, name: S) -> PathBuf {
        match self {
            Self(maj, min, update) if self < &MIN_MODERN_VERSION => {
                PathBuf::from(format!("{}-{}.{}.{}.txt", name.as_ref(), maj, min, update))
            },
            _ => PathBuf::from("ucd").join(format!("{}.txt", name.as_ref())),
        }
    }

    #[allow(unused)]
    pub fn unicode_data_filename(&self) -> String {
        match self {
            Self(x, y, z) if self < &MIN_MODERN_VERSION => {
                format!("UnicodeData-{}.{}.{}.txt", x, y, z)
            },
            _ => String::from("UnicodeData.txt"),
        }
    }
}

impl FromStr for UnicodeVersion {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::finish(Self::parse(s)).wrap_err("invalid unicode version")
    }
}

impl fmt::Debug for UnicodeVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}

impl Display for UnicodeVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}

impl serde::Serialize for UnicodeVersion {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}.{}.{}", self.0, self.1, self.2))
    }
}

struct UnicodeVersionVisitor;

impl<'de> serde::de::Visitor<'de> for UnicodeVersionVisitor {
    type Value = UnicodeVersion;

    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("a unicode version like 15.1.0")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        UnicodeVersion::from_str(v).map_err(|err| E::custom(err))
    }
}

impl<'de> serde::Deserialize<'de> for UnicodeVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(UnicodeVersionVisitor)
    }
}

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
        use nom::combinator::{all_consuming, map};
        use parse::hex_digits;

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

pub trait VersionConstructor {
    fn version(maj: u8, min: u8, update: u8) -> Self;
}

impl VersionConstructor for UnicodeVersion {
    fn version(maj: u8, min: u8, update: u8) -> Self {
        UnicodeVersion(maj, min, update)
    }
}

impl VersionConstructor for SelectVersion {
    fn version(maj: u8, min: u8, update: u8) -> Self {
        SelectVersion::Version(UnicodeVersion(maj, min, update))
    }
}

pub fn construct_version<T: VersionConstructor>(maj: u8, min: u8, update: u8) -> T {
    T::version(maj, min, update)
}

#[macro_export]
macro_rules! ver {
    ($maj:literal , $min:literal , $update:literal) => {
        &$crate::unicode::version::construct_version($maj, $min, $update)
    };
    ($maj:literal , $min:literal) => {
        ver!($maj, $min, 0)
    };
    ($maj:literal) => {
        ver!($maj, 0, 0)
    };
    (LATEST) => {
        $crate::unicode::version::SelectVersion::Latest
    };
    (latest) => {
        ver!(LATEST)
    };
    (Latest) => {
        ver!(LATEST)
    };
    (DRAFT) => {
        $crate::unicode::version::SelectVersion::Draft
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

pub use ver;
