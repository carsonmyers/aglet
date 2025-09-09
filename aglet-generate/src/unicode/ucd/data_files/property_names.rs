use std::collections::HashMap;
use std::path::PathBuf;

use eyre::eyre;
use nom::Parser;

use crate::unicode::ucd::{LoadFromFile, ParseFromFile};
use crate::unicode::UnicodeVersion;
use crate::{parse, ver};

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
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        if version < ver!(3, 2) {
            return Err(eyre!(
                "property name aliases are not published before version 3.2.0"
            ));
        }

        version.filename("PropertyAliases").ok_or_else(|| {
            eyre!(
                "no property name aliases filename could be determined for version {}",
                version
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use nom::combinator::{all_consuming, map};
        use parse::ucd::{name, ucd_lines_rest};

        // Name ; Alias
        // Name ; Alias ; Alias2
        let line_parser = ucd_lines_rest((name,), name);
        all_consuming(map(line_parser, |entries| {
            let mut res = Self::new();

            for ((name,), aliases) in entries {
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
        let (_, props) = PropertyNames::parse("", ver!()).unwrap();
        assert!(props.aliases.is_empty());
        assert!(props.names.is_empty());
    }

    #[test]
    fn test_only_comment() {
        let input = "#hello world\n#this is a comment\n\n# more comment\n";
        let (_, props) = PropertyNames::parse(input, ver!()).unwrap();
        assert!(props.aliases.is_empty());
        assert!(props.names.is_empty());
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
        let res = parse::finish(res);
        if let Err(e) = &res {
            eprintln!("{e}");
        }

        assert!(res.is_ok());

        let names = res.unwrap();
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
