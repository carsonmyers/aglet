use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::{LoadFromFile, ParseFromFile};
use crate::parse;
use crate::unicode::UnicodeVersion;
use aglet_text::CharGroup;
use eyre::eyre;
use nom::Parser;
use tracing::warn;

pub struct UnicodeData {
    pub general_category: HashMap<String, CharGroup>,
}

impl UnicodeData {
    pub fn new() -> Self {
        Self {
            general_category: HashMap::new(),
        }
    }
}

impl ParseFromFile for UnicodeData {
    fn filename(base_path: &Path, version: &UnicodeVersion) -> eyre::Result<PathBuf> {
        Ok(base_path.join(version.filename("UnicodeData")))
    }

    fn parse<'a>(input: &'a str, _: &UnicodeVersion) -> parse::Result<'a, Self> {
        use nom::combinator::{all_consuming, map};
        use parse::ucd::{char_range, many1_values, name, ucd_lines, value};

        // range; name; gc; <rest...>
        // 001C;<control>;Cc;0;B;;;;;N;INFORMATION SEPARATOR FOUR;;;;
        let line_parser = (char_range, value, name, many1_values(value));

        all_consuming(map(ucd_lines(line_parser), |entries| {
            let mut res = Self::new();

            for (range, _, gc, _) in entries {
                if let Some(group) = res.general_category.get_mut(gc) {
                    group.add_range(range)
                } else {
                    let mut group = CharGroup::new();
                    group.add_range(range);
                    res.general_category.insert(gc.to_string(), group);
                }
            }

            res
        }))
        .parse(input)
    }
}

impl LoadFromFile for UnicodeData {}
