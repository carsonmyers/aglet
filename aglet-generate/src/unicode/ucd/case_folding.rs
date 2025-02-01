use std::collections::HashMap;
use std::path::PathBuf;

use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::parse_from_file::ParseFromFile;
use crate::unicode::ucd::{LoadFromFile, UcdParseError};
use crate::unicode::UnicodeVersion;

#[derive(Default)]
pub struct CaseFolding {
    simple_mapping: HashMap<u32, u32>,
    full_mapping: HashMap<u32, Vec<u32>>,
    turkic_mapping: HashMap<u32, u32>,
}

impl CaseFolding {
    pub fn new() -> Self {
        Default::default()
    }
}

enum CaseFoldingStatus {
    Common,
    Full,
    Simple,
    Turkic,
}

impl ParseFromFile for CaseFolding {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version
            .filename("CaseFolding")
            .ok_or_else(|| eyre!("no case folding data could be found for version {version}"))
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use nom::combinator::{all_consuming, map_res};

        use parse::ucd::{codepoint, codepoints, name, ucd_lines};
        use CaseFoldingStatus::*;

        let line_parser = (codepoint, name, codepoints);

        all_consuming(map_res(ucd_lines(line_parser), |entries| {
            let mut case_folding = CaseFolding::new();
            for entry in entries {
                let (code, status, folding) = entry;

                let status = match status {
                    "C" => Common,
                    "S" => Simple,
                    "F" => Full,
                    "T" => Turkic,
                    invalid => {
                        return Err(UcdParseError::UnsupportedCaseFoldingStatus(
                            invalid.to_string(),
                        ))
                    },
                };

                match (status, &folding[..]) {
                    (Common, [c]) => {
                        case_folding.simple_mapping.insert(code, *c);
                        case_folding.full_mapping.insert(code, folding);
                    },
                    (Common, _) => {
                        return Err(UcdParseError::CommonCaseFoldingChangesWidth(code, folding))
                    },
                    (Simple, [c]) => {
                        case_folding.simple_mapping.insert(code, *c);
                    },
                    (Simple, _) => {
                        return Err(UcdParseError::SimpleCaseFoldingChangesWidth(code, folding))
                    },
                    (Full, _) => {
                        case_folding.full_mapping.insert(code, folding);
                    },
                    (Turkic, [c]) => {
                        case_folding.turkic_mapping.insert(code, *c);
                    },
                    (Turkic, _) => {
                        return Err(UcdParseError::TurkicCaseFoldingChangesWidth(code, folding));
                    },
                }
            }

            Ok(case_folding)
        }))
        .parse(input)
    }
}

impl LoadFromFile for CaseFolding {}
