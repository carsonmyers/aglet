use std::path::PathBuf;
use std::sync::WaitTimeoutResult;
use aglet_text::CharRange;
use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::{LoadFromFile, ParseFromFile, common};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct Blocks {
    pub blocks: Vec<(CharRange, String)>,
}

impl Blocks {
    pub fn new() -> Self {
        Default::default()
    }
}

impl FromIterator<(CharRange, String)> for Blocks {
    fn from_iter<I: IntoIterator<Item=(CharRange, String)>>(iter: I) -> Self {
        let mut blocks = Blocks::new();
        
        for block in iter {
            blocks.blocks.push(block);
        }
        
        blocks
    }
    
}

impl ParseFromFile for Blocks {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("Blocks").ok_or_else(|| {
            eyre!(
                "no blocks data could be found for version {}",
                version
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use parse::ucd::value;

        let (i, lines) = common::char_range_single_properties(value).parse(input)?;
        let blocks = lines
            .into_iter()
            .filter_map(|(range, block_name)| {
                range.map(|range| (range, block_name.to_string()))
            })
            .collect();
        
        Ok((i, blocks))
    }
}

impl LoadFromFile for Blocks {}