use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use eyre::{eyre, Error};
use nom::IResult;

use crate::parse;

const MIN_MODERN_VERSION: UnicodeVersion = UnicodeVersion::Version(4, 1, 0);

#[derive(Debug, Default, Clone, Ord, Eq)]
pub enum UnicodeVersion {
    #[default]
    Latest,
    Version(u8, u8, u8),
}

impl UnicodeVersion {
    pub fn parse(input: &str) -> IResult<&str, UnicodeVersion> {
        use nom::branch::alt;

        alt((
            Self::parse_latest,
            Self::parse_update_version,
            Self::parse_version,
        ))(input)
    }

    pub fn parse_latest(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::combinator::value;

        value(Self::Latest, tag("latest"))(input)
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
            |((x, y), z)| Self::Version(x, y, z.unwrap_or_default()),
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
            |(x, y, z)| Self::Version(x, y.unwrap_or_default(), z.unwrap_or_default()),
        )(input)
    }

    pub fn remote_dir(&self) -> String {
        match self {
            Self::Latest => String::from("Public/UCD/latest"),
            Self::Version(x, y, z) if self < &MIN_MODERN_VERSION && z == &0 => {
                format!("Public/{}.{}-Update", x, y)
            },
            Self::Version(x, y, z) if self < &MIN_MODERN_VERSION => {
                format!("Public/{}.{}-Update{}", x, y, z)
            },
            Self::Version(x, y, z) => {
                format!("Public/{}.{}.{}", x, y, z)
            },
        }
    }

    pub fn unicode_data_filename(&self) -> String {
        match self {
            Self::Version(x, y, z) if self < &MIN_MODERN_VERSION => {
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

impl PartialEq<Self> for UnicodeVersion {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Latest, Self::Latest) => true,
            (Self::Version(x1, y1, z1), Self::Version(x2, y2, z2)) => {
                x1 == x2 && y1 == y2 && z1 == z2
            },
            _ => false,
        }
    }
}

impl PartialOrd for UnicodeVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Latest, Self::Latest) => Some(Ordering::Equal),
            (Self::Latest, _) => Some(Ordering::Greater),
            (_, Self::Latest) => Some(Ordering::Less),
            (Self::Version(x1, y1, z1), Self::Version(x2, y2, z2)) => {
                (x1, y1, z1).partial_cmp(&(x2, y2, z2))
            },
        }
    }
}
