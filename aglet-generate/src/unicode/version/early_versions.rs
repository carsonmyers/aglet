use crate::parse;
use crate::unicode::UnicodeVersion;
use eyre::{eyre, WrapErr};
use nom::Parser;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::str::FromStr;
use tracing::{error, info};

pub const MIN_MODERN_VERSION: UnicodeVersion = UnicodeVersion(4, 1, 0);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SequentialDataFile {
    ArabicShaping,
    BidiMirroring,
    Blocks,
    CaseFolding,
    CompositionExclusions,
    EastAsianWidth,
    Index,
    Jamo,
    LineBreak,
    NamesList,
    SpecialCasing,
    Unihan,
}

impl FromStr for SequentialDataFile {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use SequentialDataFile::*;

        let res = match s {
            "ArabicShaping" => ArabicShaping,
            "BidiMirroring" => BidiMirroring,
            "Blocks" => Blocks,
            "CaseFolding" => CaseFolding,
            "CompositionExclusions" => CompositionExclusions,
            "EastAsianWidth" => EastAsianWidth,
            "Index" => Index,
            "Jamo" => Jamo,
            "LineBreak" => LineBreak,
            "NamesList" => NamesList,
            "SpecialCasing" => SpecialCasing,
            "Unihan" => Unihan,
            _ => {
                return Err(eyre!(
                    "unknown sequential data file for early versions: {}",
                    s
                ))
            },
        };

        Ok(res)
    }
}

const SAME_AS_VERSION: u8 = 255;
const SAME_AS_VERSION_SHORT: u8 = 254;

struct SequentialDataFileVersion {
    data_file: SequentialDataFile,
    version: u8,
}

struct EarlyVersionInfo {
    version_with_update: UnicodeVersionWithUpdate,
    version: UnicodeVersion,
    sequential_file_versions: &'static [SequentialDataFileVersion],
}

macro_rules! v_info {
    (
        $maj:literal : $min:literal : $update:literal ,
        [ $( $( $file:ident ),+ = $version:tt ),* ]
    ) => {
        v_info!($maj:$min:$update, $maj:$min:$update , [ $( $( $file ),+ = $version ),* ])
    };
    (
        $maj:literal : $min:literal : $update:literal
    ) => {
        v_info!($maj:$min:$update, $maj:$min:$update , [ ])
    };
    (
        $maj1:literal : $min1:literal : $update:literal ,
        $maj2:literal : $min2:literal : $patch:literal ,
        [ $( $( $file:ident ),+ = $version:tt ),* ]
    ) => {
        EarlyVersionInfo {
            version_with_update: UnicodeVersionWithUpdate($maj1, $min1, $update),
            version: UnicodeVersion($maj2, $min2, $patch),
            sequential_file_versions: &[$($(
                SequentialDataFileVersion {
                    data_file: SequentialDataFile::$file,
                    version: $version,
                }
            ),+),*]
        }
    };
    (
        $maj1:literal : $min1:literal : $update:literal ,
        $maj2:literal : $min2:literal : $patch:literal
    ) => {
        v_info!(
            $maj1 : $min1 : $update ,
            $maj2 : $min2 : $patch ,
            [ ]
        )
    };
}

const EARLY_VERSIONS: [EarlyVersionInfo; 13] = [
    v_info!(1:1:0, 1:1:5),
    v_info!(2:0:0, 2:0:14, [ArabicShaping, Blocks, Index, Jamo, NamesList, Unihan = 1]),
    v_info!(2:1:0, 2:1:2, [Unihan = 2]),
    v_info!(2:1:2, 2:1:5),
    v_info!(2:1:3, 2:1:8, [SpecialCasing = 1]),
    v_info!(2:1:4, 2:1:9, [Blocks, Jamo, SpecialCasing = 2]),
    v_info!(3:0:0, [
        ArabicShaping, Jamo, SpecialCasing = 2,
        Blocks, EastAsianWidth, Unihan = 3,
        LineBreak = 5,
        Index, NamesList = SAME_AS_VERSION
    ]),
    v_info!(3:0:1, [
        BidiMirroring = 1,
        CaseFolding, CompositionExclusions = 2,
        ArabicShaping, Jamo, SpecialCasing = 3
    ]),
    v_info!(3:1:0, [
        CaseFolding, CompositionExclusions = 3,
        Blocks, EastAsianWidth, SpecialCasing = 4,
        LineBreak = 6,
        Unihan = SAME_AS_VERSION_SHORT,
        NamesList = SAME_AS_VERSION
    ]),
    v_info!(3:1:1, [
        ArabicShaping, CaseFolding = 4,
        EastAsianWidth, SpecialCasing = 5,
        Unihan = SAME_AS_VERSION
    ]),
    v_info!(3:2:0, [
        ArabicShaping, BidiMirroring, Blocks, CaseFolding, CompositionExclusions, EastAsianWidth,
        Index, Jamo, LineBreak, NamesList, SpecialCasing, Unihan = SAME_AS_VERSION
    ]),
    v_info!(4:0:0, [
        ArabicShaping, BidiMirroring, Blocks, CaseFolding, CompositionExclusions, EastAsianWidth,
        LineBreak, NamesList, SpecialCasing = SAME_AS_VERSION
    ]),
    v_info!(4:0:1, [
        ArabicShaping, Blocks, CaseFolding, Index, Jamo, LineBreak, SpecialCasing, Unihan = SAME_AS_VERSION
    ]),
];

#[derive(Copy, Clone, Default, PartialOrd, Ord, PartialEq, Eq)]
pub struct UnicodeVersionWithUpdate(pub u8, pub u8, pub u8);

impl UnicodeVersionWithUpdate {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::character::complete::digit1;
        use nom::combinator::{all_consuming, map, map_res, opt};
        use nom::sequence::{pair, preceded, separated_pair};

        map(
            all_consuming(pair(
                separated_pair(
                    map_res(digit1, str::parse),
                    tag("."),
                    map_res(digit1, str::parse),
                ),
                preceded(tag("-Update"), opt(map_res(digit1, str::parse))),
            )),
            |((x, y), z)| Self(x, y, z.unwrap_or_default()),
        )
        .parse(input)
    }

    pub fn to_version(self) -> eyre::Result<UnicodeVersion> {
        let version = EARLY_VERSIONS
            .iter()
            .find(|v| v.version_with_update == self)
            .map(|v| v.version)
            .ok_or_else(|| eyre!("version {} does not correspond to a Unicode version", self))?;

        info!(
            "convert early version {} to Unicode version {}",
            &self, &version
        );

        Ok(version)
    }

    pub fn remote_dir(&self) -> Option<String> {
        match self {
            Self(maj, min, 0) => Some(format!("Public/{}.{}-Update", maj, min)),
            Self(maj, min, update) => Some(format!("Public/{}.{}-Update{}", maj, min, update)),
        }
    }

    pub fn filename<S: AsRef<str>>(&self, name: S) -> Option<(UnicodeVersion, PathBuf)> {
        let name = name.as_ref();

        let version_info_index = EARLY_VERSIONS
            .iter()
            .position(|v| &v.version_with_update == self)
            .expect("valid version with update");

        let Ok(file) = SequentialDataFile::from_str(name) else {
            let version = EARLY_VERSIONS[version_info_index].version;
            return Some((
                version,
                PathBuf::from(format!(
                    "{}-{}.{}.{}.txt",
                    name, version.0, version.1, version.2
                )),
            ));
        };

        EARLY_VERSIONS
            .iter()
            .take(version_info_index)
            .rev()
            .find_map(|v| {
                v.sequential_file_versions
                    .iter()
                    .find(|f| f.data_file == file)
                    .map(|f| (v.version, f.version))
            })
            .map(|(UnicodeVersion(maj, min, patch), file_version)| {
                let filename = match file_version {
                    SAME_AS_VERSION => format!("{}-{}.{}.{}.txt", name, maj, min, patch),
                    SAME_AS_VERSION_SHORT => format!("{}-{}.{}.txt", name, maj, min),
                    sequential_version => format!("{}-{}.txt", name, sequential_version),
                };

                (UnicodeVersion(maj, min, patch), PathBuf::from(filename))
            })
    }
}

impl TryFrom<&UnicodeVersion> for UnicodeVersionWithUpdate {
    type Error = eyre::Report;

    fn try_from(value: &UnicodeVersion) -> Result<Self, Self::Error> {
        EARLY_VERSIONS
            .iter()
            .find(|info| &info.version == value)
            .map(|info| info.version_with_update)
            .ok_or_else(|| {
                eyre!(
                    "version {} does not correspond to an early UCD version",
                    value
                )
            })
    }
}

impl FromStr for UnicodeVersionWithUpdate {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse::finish(Self::parse(s))
            .inspect_err(|err| error!("cannot parse {} as unicode version with update: {}", s, err))
            .wrap_err("invalid unicode version with update")
    }
}

impl fmt::Debug for UnicodeVersionWithUpdate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.2 > 0 {
            write!(f, "{}.{}-Update{}", self.0, self.1, self.2)
        } else {
            write!(f, "{}.{}-Update", self.0, self.1)
        }
    }
}

impl Display for UnicodeVersionWithUpdate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
