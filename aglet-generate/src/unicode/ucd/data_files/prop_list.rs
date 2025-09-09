use std::collections::HashMap;
use std::path::PathBuf;

use eyre::eyre;
use aglet_text::CharRange;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::{LoadFromFile, ParseFromFile, common};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct PropList {
    pub prop_ranges: common::RangeMap,
}

impl PropList {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParseFromFile for PropList {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("PropList").ok_or_else(|| {
            eyre!(
                "no prop list data could be found for version {}",
                version,
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        let (i, prop_ranges) = common::property_ranges(input)?;
        Ok((i, PropList { prop_ranges }))
    }
}

impl LoadFromFile for PropList {}