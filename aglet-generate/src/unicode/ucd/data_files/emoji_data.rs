use std::collections::HashMap;
use std::path::PathBuf;

use aglet_text::CharRange;
use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::{LoadFromFile, ParseFromFile, common};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct EmojiData {
    pub prop_ranges: common::RangeMap,
}

impl EmojiData {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParseFromFile for EmojiData {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("emoji/emoji-data").ok_or_else(|| {
            eyre!(
                "no emoji-data found for version {}",
                version,
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        let (i, prop_ranges) = common::property_ranges(input)?;
        Ok((i, EmojiData { prop_ranges }))
    }
}

impl LoadFromFile for EmojiData {}