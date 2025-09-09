use std::collections::HashMap;
use std::path::PathBuf;

use eyre::eyre;
use nom::Parser;

use crate::parse;
use crate::unicode::ucd::parse_from_file::ParseFromFile;
use crate::unicode::ucd::{LoadFromFile, UcdParseError};
use crate::unicode::UnicodeVersion;

#[derive(Debug, Copy)]
enum CaseFoldingStatus {
    Common,
    Full,
    Simple,
    Turkic,
}

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

impl FromIterator<(u32, CaseFoldingStatus, Vec<u32>)> for CaseFolding {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (u32, CaseFoldingStatus, Vec<u32>)>,
    {
        let mut case_folding = CaseFolding::new();
        for (code, status, folding) in iter {
            match (status, &folding[..]) {
                (CaseFoldingStatus::Common, [c]) => {
                    case_folding.simple_mapping.insert(code, *c);
                    case_folding.full_mapping.insert(code, folding);
                },
                (CaseFoldingStatus::Simple, [c]) => {
                    case_folding.simple_mapping.insert(code, *c);
                },
                (CaseFoldingStatus::Full, _) => {
                    case_folding.full_mapping.insert(code, folding);
                },
                (CaseFoldingStatus::Turkic, [c]) => {
                    case_folding.turkic_mapping.insert(code, *c);
                },
                _ => panic!("invalid case folding entry"),
            }
        }
        
        case_folding
    }
}

impl ParseFromFile for CaseFolding {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version
            .filename("CaseFolding")
            .ok_or_else(|| eyre!("no case folding data could be found for version {version}"))
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use parse::ucd::{codepoint, codepoints, name, ucd_lines};
        
        let line_parser = |(code, status, folding)| {
            let status = match status {
                "C" => CaseFoldingStatus::Common,
                "S" => CaseFoldingStatus::Simple,
                "F" => CaseFoldingStatus::Full,
                "T" => CaseFoldingStatus::Turkic,
                invalid => {
                    return Err(UcdParseError::UnsupportedCaseFoldingStatus(
                        invalid.to_string(),
                    ))
                },
            };
            
            match (status, &folding[..]) {
                (CaseFoldingStatus::Common, [_, _, ..]) => {
                    return Err(UcdParseError::CommonCaseFoldingChangesWidth(code, folding))
                },
                (CaseFoldingStatus::Simple, [_, _, ..]) => {
                    return Err(UcdParseError::SimpleCaseFoldingChangesWidth(code, folding))
                },
                (CaseFoldingStatus::Turkic, [_, _, ..]) => {
                    return Err(UcdParseError::TurkicCaseFoldingChangesWidth(code, folding));
                },
                _ => Ok((code, status, folding)),
            }
        };
        
        let (i, lines) = ucd_lines((codepoint, name, codepoints), line_parser).parse(input)?;
        let (_, case_folding) = lines.into_iter().collect::<parse::Result<CaseFolding>>()?;
        
        Ok((i, case_folding))
    }
}

impl LoadFromFile for CaseFolding {}
