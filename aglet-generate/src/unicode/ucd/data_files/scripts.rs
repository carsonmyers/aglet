use std::collections::HashMap;
use std::path::PathBuf;

use aglet_text::CharRange;
use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::{LoadFromFile, ParseFromFile, common};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct Scripts {
    pub script_ranges: common::RangeMap,
}

impl Scripts {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParseFromFile for Scripts {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("Scripts").ok_or_else(|| {
            eyre!(
                "no scripts data could be found for version {}",
                version,
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        let (i, script_ranges) = common::property_ranges(input)?;
        Ok((i, Scripts { script_ranges }))
    }
}

impl LoadFromFile for Scripts {}