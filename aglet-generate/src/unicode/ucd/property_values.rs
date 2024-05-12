use std::collections::HashMap;
use std::path::{Path, PathBuf};

use eyre::eyre;
use nom::Parser;

use super::{LoadFromFile, ParseFromFile};
use crate::parse;
use crate::unicode::ucd::keys::{BorrowKey2, BorrowedKey2, Key2};
use crate::unicode::{ver, UnicodeVersion};

#[derive(Debug)]
pub struct PropertyValues {
    pub alias_map: HashMap<Key2, String>,
    pub values: HashMap<Key2, Vec<String>>,
    pub values_by_property_name: HashMap<String, Vec<String>>,
}

impl PropertyValues {
    pub fn new() -> Self {
        Self {
            alias_map: HashMap::new(),
            values: HashMap::new(),
            values_by_property_name: HashMap::new(),
        }
    }

    pub fn resolve_value<'a, 'b: 'a, 'c: 'a>(
        &'a self,
        property_name: &'b str,
        value_alias: &'c str,
    ) -> Option<&'a String> {
        let key = BorrowedKey2(property_name, value_alias);
        self.alias_map.get(&key as &dyn BorrowKey2)
    }

    pub fn values_for(&self, property_name: &str) -> Option<&[String]> {
        self.values_by_property_name
            .get(property_name)
            .map(|values| values.as_slice())
    }

    pub fn aliases_of(&self, property_name: &str, value: &str) -> Option<&[String]> {
        let key = BorrowedKey2(property_name, value);
        self.values
            .get(&key as &dyn BorrowKey2)
            .map(|aliases| aliases.as_slice())
    }
}

impl ParseFromFile for PropertyValues {
    fn filename(base_path: &Path, version: &UnicodeVersion) -> eyre::Result<PathBuf> {
        if version < ver!(3, 2) {
            Err(eyre!(
                "property value aliases are not published before version 3.2.0"
            ))
        } else {
            Ok(base_path.join(version.filename("PropertyValueAliases")))
        }
    }

    fn parse<'a>(input: &'a str, _: &UnicodeVersion) -> parse::Result<'a, Self> {
        use nom::combinator::{all_consuming, map};
        use parse::ucd::{many1_values, name, ucd_lines, value};

        // prop ; value ; value_alias
        // prop ; value ; value_alias_1 ; value_alias_2
        let line_parser = (name, many1_values(value));

        all_consuming(map(ucd_lines(line_parser), |entries| {
            let mut res = Self::new();

            for (name, values) in entries {
                for &alias in &values {
                    let key = Key2(name.into(), alias.into());
                    res.alias_map.insert(key, values[0].into());
                }

                res.values_by_property_name
                    .entry(name.into())
                    .and_modify(|v| v.push(values[0].into()))
                    .or_insert_with(|| vec![values[0].into()]);

                let key = Key2(name.into(), values[0].into());
                let values = values.into_iter().map(str::to_owned).collect::<Vec<_>>();

                res.values.insert(key, values);
            }

            res
        }))
        .parse(input)
    }
}

impl LoadFromFile for PropertyValues {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        assert!(PropertyValues::parse("", ver!()).is_err())
    }

    #[test]
    fn test_only_comment() {
        let input = "#hello world\n#this is a comment\n\n# more comment\n";
        assert!(PropertyValues::parse(input, ver!()).is_err());
    }

    #[test]
    fn test_parse() {
        let input = r"
        #hello world
        name1   ; val1  ; val2  ; val3 #comment
        name1;hw;hello_world;hello###
        
        #some comments
        name2;val1;val2;val3";

        let res = PropertyValues::parse(input, ver!());
        assert!(res.is_ok());

        let (input, values) = res.unwrap();
        assert_eq!(input, "");
        assert_eq!(values.alias_map.len(), 9);
        assert_eq!(values.resolve_value("name1", "val1").unwrap(), "val1");
    }
}
