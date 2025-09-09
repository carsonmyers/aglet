use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::unicode::ucd::{LoadFromFile, ParseFromFile};
use crate::parse;
use crate::unicode::UnicodeVersion;
use aglet_text::{CharGroup, CharRange};
use eyre::eyre;
use nom::Parser;
use tracing::warn;

pub struct UnicodeData {
    pub general_category: HashMap<String, CharGroup>,
    pub simple_uppercase_mapping: HashMap<u32, u32>,
    pub simple_lowercase_mapping: HashMap<u32, u32>,
    pub simple_titlecase_mapping: HashMap<u32, u32>,
}

impl UnicodeData {
    pub fn new() -> Self {
        Self {
            general_category: HashMap::new(),
            simple_uppercase_mapping: HashMap::new(),
            simple_lowercase_mapping: HashMap::new(),
            simple_titlecase_mapping: HashMap::new(),
        }
    }
}

impl ParseFromFile for UnicodeData {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("UnicodeData").ok_or_else(|| {
            eyre!(
                "no unicode data filename could be determined for version {}",
                version
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use nom::combinator::{all_consuming, map, opt};
        use parse::ucd::{codepoint, name, ucd_lines, value};
        
        // range; name; gc; <others>; upper; lower; title
        // 001C;<control>;Cc;0;B;;;;;N;INFORMATION SEPARATOR FOUR;;;;
        let fields = (
            codepoint,      // single code-point: ranges are specified differently in UnicodeData.txt
            value,          // Name
            name,           // General_Category
            value,          // Canonical_Combining_Class
            value,          // Bidi_Class
            opt(value),     // Decomposition_Type, Decomposition_Mapping
            opt(value),     // Decimal value
            opt(value),     // Digit value
            opt(value),     // Numeric value
            value,          // Bidi_Mirrored
            opt(value),     // Unicode_1_Name (obsolete as of 6.2.0)
            opt(value),     // ISO_Comment (obsolete as of 5.2.0, deprecated as of 6.0.0)
            opt(codepoint), // Simple_Uppercase_Mapping
            opt(codepoint), // Simple_Lowercase_Mapping
            opt(codepoint), // Simple_Titlecase_Mapping
        );
        
        let mut unicode_data = Self::new();
        let line_parser = |(cp, _, gc, _, _, _, _, _, _, _, _, _, upper, lower, title)| {
            // the surrogate codepoints are encoded in UnicodeData.txt, but are invalid
            // codepoints and so will fail to be constructed into a CharRange. Just skip them
            let Ok(range) = CharRange::try_from(cp) else {
                return;
            };

            if let Some(group) = unicode_data.general_category.get_mut(gc) {
                group.add_range(range)
            } else {
                let mut group = CharGroup::new();
                group.add_range(range);
                unicode_data.general_category.insert(gc.to_string(), group);
            }

            if let Some(upper) = upper {
                unicode_data.simple_uppercase_mapping.insert(cp, upper);
            }
            if let Some(lower) = lower {
                unicode_data.simple_lowercase_mapping.insert(cp, lower);
            }
            if let Some(title) = title {
                unicode_data.simple_titlecase_mapping.insert(cp, title);
            } else if let Some(upper) = upper {
                unicode_data.simple_titlecase_mapping.insert(cp, upper);
            }
        };
        
        let (i, _) = ucd_lines(fields, line_parser).parse(input)?;
        Ok((i, unicode_data))
    }
}

impl LoadFromFile for UnicodeData {}
