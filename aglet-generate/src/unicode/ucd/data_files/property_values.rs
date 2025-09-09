use std::collections::HashMap;
use std::iter;
use std::path::PathBuf;

use eyre::eyre;
use nom::Parser;

use crate::unicode::ucd::{LoadFromFile, ParseFromFile};
use crate::unicode::ucd::keys::{BorrowKey2, BorrowedKey2, Key2};
use crate::unicode::UnicodeVersion;
use crate::{parse, ver};

#[derive(Debug)]
pub struct PropertyValues {
    pub values_by_alias: HashMap<Key2, String>,
    pub aliases_by_value: HashMap<Key2, Vec<String>>,
    pub values_by_property: HashMap<String, Vec<String>>,
    pub aliases_by_property: HashMap<String, Vec<String>>,
}

impl PropertyValues {
    pub fn new() -> Self {
        Self {
            values_by_alias: HashMap::new(),
            aliases_by_value: HashMap::new(),
            values_by_property: HashMap::new(),
            aliases_by_property: HashMap::new(),
        }
    }

    pub fn resolve_value<'a, 'b: 'a, 'c: 'a>(
        &'a self,
        property_name: &'b str,
        value_alias: &'c str,
    ) -> Option<&'a String> {
        let key = BorrowedKey2(property_name, value_alias);
        self.values_by_alias.get(&key as &dyn BorrowKey2)
    }

    pub fn values_for(&self, property_name: &str) -> Option<&[String]> {
        self.values_by_property
            .get(property_name)
            .map(|values| values.as_slice())
    }
    
    pub fn aliases_for(&self, property_name: &str) -> Option<&[String]> {
        self.aliases_by_property
            .get(property_name)
            .map(|values| values.as_slice())
    }

    pub fn aliases_of(&self, property_name: &str, value: &str) -> Option<&[String]> {
        let key = BorrowedKey2(property_name, value);
        self.aliases_by_value
            .get(&key as &dyn BorrowKey2)
            .map(|aliases| aliases.as_slice())
    }
}

impl ParseFromFile for PropertyValues {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        if version < ver!(3, 2) {
            return Err(eyre!(
                "property value aliases are not published before version 3.2.0"
            ));
        }

        version.filename("PropertyValueAliases").ok_or_else(|| {
            eyre!(
                "no property value aliases filename could be determined for version {}",
                version
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use parse::ucd::{name, ucd_lines_rest, value};
        
        let mut property_values = Self::new();
        let line_parser = |((name, value), aliases)| {
            let value_key = Key2(name.into(), value.into());
            for alias in iter::once(value).chain(aliases) {
                let alias_key = Key2(name.into(), alias.into());
                property_values.values_by_alias.insert(alias_key.clone(), value.into());
                property_values.aliases_by_value
                    .entry(value_key.clone())
                    .and_modify(|a| a.push(alias.into()))
                    .or_insert_with(|| vec![alias.into()]);
                property_values.aliases_by_property
                    .entry(name.into())
                    .and_modify(|v| v.push(alias.into()))
                    .or_insert_with(|| vec![alias.into()]);
            }

            // add the value as a possible option for the property name
            property_values.values_by_property
                .entry(name.into())
                .and_modify(|v| v.push(value.into()))
                .or_insert_with(|| vec![value.into()]);
        };
        
        let (i, _) = ucd_lines_rest((name, value), value, line_parser).parse(input)?;
        Ok((i, property_values))
    }
}

impl LoadFromFile for PropertyValues {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let (_, props) = PropertyValues::parse("", ver!()).unwrap();
        assert!(props.values_by_alias.is_empty());
        assert!(props.aliases_by_value.is_empty());
        assert!(props.values_by_property.is_empty());
        assert!(props.aliases_by_property.is_empty());
    }

    #[test]
    fn test_only_comment() {
        let input = "#hello world\n#this is a comment\n\n# more comment\n";
        let (_, props) = PropertyValues::parse(input, ver!()).unwrap();
        assert!(props.values_by_alias.is_empty());
        assert!(props.aliases_by_value.is_empty());
        assert!(props.values_by_property.is_empty());
        assert!(props.aliases_by_property.is_empty());
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
        let res = parse::finish(res);
        if let Err(e) = &res {
            eprintln!("{e}");
        }

        assert!(res.is_ok());

        let values = res.unwrap();
        assert_eq!(values.values_by_alias.len(), 9);
        assert_eq!(values.resolve_value("name1", "val1").unwrap(), "val1");
    }
}
