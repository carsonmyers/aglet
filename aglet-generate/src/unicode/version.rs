use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

use eyre::{eyre, Error};
use serde::Deserializer;

use crate::parse;

const MIN_MODERN_VERSION: UnicodeVersion = UnicodeVersion(4, 1, 0);

#[derive(Hash, Copy, Clone, Default, PartialOrd, Ord, PartialEq, Eq)]
pub struct UnicodeVersion(u8, u8, u8);

impl UnicodeVersion {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::branch::alt;

        alt((Self::parse_update_version, Self::parse_version))(input)
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
        )(input)
    }

    pub fn parse_version(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::{map, map_res, opt};
        use nom::sequence::{preceded, tuple};

        map(
            tuple((
                map_res(digit1, str::parse),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
            )),
            |(x, y, z)| Self(x, y.unwrap_or_default(), z.unwrap_or_default()),
        )(input)
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
        use nom::Finish;

        match Self::parse(s).finish() {
            Ok((_, version)) => Ok(version),
            Err(nom::error::Error { input, code }) => {
                Err(eyre!("invalid version {} (...{}): {:?}", s, input, code))
            },
        }
    }
}

impl std::fmt::Debug for UnicodeVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}

impl Display for UnicodeVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
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

#[derive(Debug, Copy, Clone)]
pub enum SelectVersion {
    Latest,
    Draft,
    Version(UnicodeVersion),
}

impl Display for SelectVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Latest => write!(f, "latest"),
            Self::Draft => write!(f, "draft"),
            Self::Version(v) => write!(f, "{}", v),
        }
    }
}

impl FromStr for SelectVersion {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "latest" => Ok(SelectVersion::Latest),
            "draft" => Ok(SelectVersion::Draft),
            value => Ok(SelectVersion::Version(UnicodeVersion::from_str(value)?)),
        }
    }
}
