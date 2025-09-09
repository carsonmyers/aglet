use std::collections::HashMap;
use std::path::PathBuf;
use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::parse_from_file::ParseFromFile;
use crate::unicode::ucd::LoadFromFile;
use crate::unicode::UnicodeVersion;

#[derive(Debug, Default)]
pub struct DerivedNormalizationProps {
    pub casefold_map: HashMap<char, Vec<char>>,
    pub simple_casefold_map: HashMap<char, Vec<char>>,
}

impl DerivedNormalizationProps {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ParseFromFile for DerivedNormalizationProps {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("DerivedNormalizationProps").ok_or_else(|| {
            eyre!(
                "no derived normalization property data found for version {}",
                version,
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use nom::branch::alt;
        use nom::combinator::{map, value};
        use parse::ucd::{self, char_range, chars, name, ucd_lines};
        
        let fields = (
            char_range,
            name,
            alt((
                map(chars, Some),
                value(None, ucd::value),
            ))
        );
        
        let mut props = Self::new();
        let line_parser = |(range, name, mapping)| {
            let map = match name {
                "NFKC_CF" => &mut props.casefold_map,
                "NFKC_SCF" => &mut props.simple_casefold_map,
                _ => return,
            };

            let Some(mapping) = mapping else {
                return;
            };

            for c in range {
                map.insert(c, mapping.clone());
            }
        };
        
        let (i, _) = ucd_lines(fields, line_parser).parse(input)?;
        
        Ok((i, props))
    }
}

impl LoadFromFile for DerivedNormalizationProps {}