use std::fmt;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::str::FromStr;

use super::early_versions::MIN_MODERN_VERSION;
use crate::parse;
use crate::unicode::UnicodeVersionWithUpdate;
use eyre::{eyre, Error, WrapErr};
use nom::Parser;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer};
use tracing::info;

#[derive(Hash, Copy, Clone, Default, PartialOrd, Ord, PartialEq, Eq)]
pub struct UnicodeVersion(pub u8, pub u8, pub u8);

impl UnicodeVersion {
    pub fn new(maj: u8, min: u8, patch: u8) -> Self {
        Self(maj, min, patch)
    }

    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::{all_consuming, map, map_res, opt};
        use nom::sequence::preceded;

        map(
            all_consuming((
                map_res(digit1, str::parse),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
                opt(preceded(tag("."), map_res(digit1, str::parse))),
            )),
            |(x, y, z)| Self(x, y.unwrap_or_default(), z.unwrap_or_default()),
        )
        .parse(input)
    }

    pub fn remote_dir(&self) -> Option<String> {
        if self < &MIN_MODERN_VERSION {
            UnicodeVersionWithUpdate::try_from(self)
                .ok()
                .and_then(|version| version.remote_dir())
        } else {
            Some(format!("Public/{}.{}.{}", self.0, self.1, self.2))
        }
    }

    pub fn filename<S: AsRef<str>>(&self, name: S) -> Option<(UnicodeVersion, PathBuf)> {
        if self < &MIN_MODERN_VERSION {
            UnicodeVersionWithUpdate::try_from(self)
                .ok()
                .and_then(|version| version.filename(name))
        } else {
            let filename = format!("{}.txt", name.as_ref());
            let path = PathBuf::from("ucd").join(filename);
            Some((*self, path))
        }
    }
}

impl FromStr for UnicodeVersion {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::finish(Self::parse(s))
            .or_else(|_| {
                info!(
                    "cannot parse {} as unicode version: trying a version with update",
                    s
                );
                UnicodeVersionWithUpdate::from_str(s).and_then(UnicodeVersionWithUpdate::to_version)
            })
            .wrap_err("invalid unicode version")
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

impl<'de> Visitor<'de> for UnicodeVersionVisitor {
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

impl<'de> Deserialize<'de> for UnicodeVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(UnicodeVersionVisitor)
    }
}
