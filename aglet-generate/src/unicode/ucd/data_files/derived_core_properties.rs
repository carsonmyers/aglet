use std::collections::HashMap;
use std::path::PathBuf;

use eyre::eyre;
use aglet_text::CharRange;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::{ParseFromFile, LoadFromFile, common};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct DerivedCoreProperties {
    pub prop_ranges: common::RangeMap,
}

impl DerivedCoreProperties {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParseFromFile for DerivedCoreProperties {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("DerivedCoreProperties").ok_or_else(|| {
            eyre!(
                "no derived core properties data could be found for version {}",
                version,
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        let (i, prop_ranges) = common::property_ranges(input)?;
        Ok((i, DerivedCoreProperties { prop_ranges }))
    }
}

impl LoadFromFile for DerivedCoreProperties {}