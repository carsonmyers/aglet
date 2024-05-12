use std::collections::HashMap;
use std::path::{Path, PathBuf};

use eyre::eyre;
use nom::Parser;

use super::{LoadFromFile, ParseFromFile};
use crate::parse;
use crate::unicode::{ver, UnicodeVersion};

#[derive(Debug)]
pub struct PropertyNames {
    pub aliases: HashMap<String, String>,
    pub names: HashMap<String, Vec<String>>,
}

impl PropertyNames {
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
            names: HashMap::new(),
        }
    }
}

impl ParseFromFile for PropertyNames {
    fn filename(base_path: &Path, version: &UnicodeVersion) -> eyre::Result<PathBuf> {
        if version < ver!(3, 2) {
            return Err(eyre!(
                "property name aliases are not published before version 3.2.0"
            ));
        }

        Ok(base_path.join(version.filename("PropertyAliases")))
    }

    fn parse<'a>(input: &'a str, _: &UnicodeVersion) -> parse::Result<'a, Self> {
        use nom::combinator::{all_consuming, map};
        use parse::ucd::{many1_values, name, ucd_lines};

        // Name ; Alias
        // Name ; Alias ; Alias2
        let line_parser = (name, many1_values(name));

        all_consuming(map(ucd_lines(line_parser), |entries| {
            let mut res = Self::new();

            for (name, aliases) in entries {
                res.aliases.insert(name.to_string(), name.to_string());
                for alias in &aliases {
                    res.aliases.insert(alias.to_string(), name.to_string());
                }

                res.names.insert(
                    name.to_string(),
                    aliases.into_iter().map(|name| name.to_string()).collect(),
                );
            }

            res
        }))
        .parse(input)
    }
}

impl LoadFromFile for PropertyNames {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        assert!(PropertyNames::parse("", ver!()).is_err());
    }

    #[test]
    fn test_only_comment() {
        let input = "#hello world\n#this is a comment\n\n# more comment\n";
        assert!(PropertyNames::parse(input, ver!()).is_err());
    }

    #[test]
    fn test_parse() {
        let input = r"
        # hello world
        name1   ;name2; name3 #comment  
        abc ; def   ; ghi#
        # hello world again
        # yes good
        Name    ; Another_Name  ; Name_again";

        let res = PropertyNames::parse(input, ver!());
        assert!(res.is_ok());

        let (input, names) = res.unwrap();
        assert_eq!(input, "");
        assert_eq!(names.aliases.len(), 9);
        assert_eq!(names.aliases.get("name1").unwrap().as_str(), "name1");
        assert_eq!(names.aliases.get("name2").unwrap().as_str(), "name1");
        assert_eq!(names.aliases.get("name3").unwrap().as_str(), "name1");
        assert_eq!(names.aliases.get("abc").unwrap().as_str(), "abc");
        assert_eq!(names.aliases.get("def").unwrap().as_str(), "abc");
        assert_eq!(names.aliases.get("ghi").unwrap().as_str(), "abc");
        assert_eq!(names.aliases.get("Name").unwrap().as_str(), "Name");
        assert_eq!(names.aliases.get("Another_Name").unwrap().as_str(), "Name");
        assert_eq!(names.aliases.get("Name_again").unwrap().as_str(), "Name");
    }

    #[test]
    fn test_parse_comments_at_end() {
        let input = "abc;def\n#comment\n\n#comment\n#comment\n";

        let res = PropertyNames::parse(input, ver!());
        assert!(res.is_ok());

        let (input, names) = res.unwrap();
        assert_eq!(input, "");
        assert_eq!(names.aliases.len(), 2);
        assert_eq!(names.aliases.get("abc").unwrap().as_str(), "abc");
        assert_eq!(names.aliases.get("def").unwrap().as_str(), "abc");
    }
}
