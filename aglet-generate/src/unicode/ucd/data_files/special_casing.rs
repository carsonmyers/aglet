use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use aglet_text::UnicodeContext;
use eyre::eyre;
use nom::Parser;

use crate::unicode::ucd::{LoadFromFile, ParseFromFile};
use crate::parse;
use crate::unicode::UnicodeVersion;

#[derive(Default)]
pub struct SpecialCasing {
    pub mappings: HashMap<Conditions, CaseMappings>,
}

#[derive(Default, Clone, Hash, Eq, PartialEq)]
pub struct Conditions {
    pub locale: Option<String>,
    pub contexts: Vec<UnicodeContext>,
}

impl Conditions {
    pub fn new(locale: Option<String>, contexts: Vec<UnicodeContext>) -> Self {
        Self { locale, contexts }
    }

    pub fn has_conditions(&self) -> bool {
        self.locale.is_some() || !self.contexts.is_empty()
    }
}

pub const NO_CONDITIONS: Conditions = Conditions {
    locale: None,
    contexts: Vec::new(),
};

impl Display for Conditions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut components = vec![];
        if let Some(locale) = &self.locale {
            components.push(format!("locale={}", locale));
        }
        for context in &self.contexts {
            components.push(context.to_string());
        }

        if !components.is_empty() {
            write!(f, "conditional [{}]", components.join(" "))
        } else {
            write!(f, "unconditional")
        }
    }
}

#[derive(Default)]
pub struct CaseMappings {
    pub lowercase_mapping: HashMap<u32, Vec<u32>>,
    pub uppercase_mapping: HashMap<u32, Vec<u32>>,
    pub titlecase_mapping: HashMap<u32, Vec<u32>>,
}

impl SpecialCasing {
    pub fn new() -> Self {
        Default::default()
    }

    fn insert_mapping(
        &mut self,
        code: u32,
        lowercase: Option<Vec<u32>>,
        uppercase: Option<Vec<u32>>,
        titlecase: Option<Vec<u32>>,
        conditions: Conditions,
    ) {
        let mapping = self.mappings.entry(conditions.clone()).or_default();

        if let Some(lowercase) = lowercase {
            mapping.lowercase_mapping.insert(code, lowercase);
        }
        if let Some(uppercase) = uppercase {
            mapping.uppercase_mapping.insert(code, uppercase);
        }
        if let Some(titlecase) = titlecase {
            mapping.titlecase_mapping.insert(code, titlecase);
        }
    }
}

impl ParseFromFile for SpecialCasing {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)> {
        version.filename("SpecialCasing").ok_or_else(|| {
            eyre!(
                "no special casing data could be found for version {}",
                version
            )
        })
    }

    fn parse(input: &str, _: UnicodeVersion) -> parse::Result<Self> {
        use nom::combinator::opt;
        use parse::ucd::{codepoint, codepoints, condition_list, ucd_lines};
        
        let fields = (
            codepoint,
            opt(codepoints),
            opt(codepoints),
            opt(codepoints),
            opt(condition_list),
        );
        
        let mut casing = Self::new();
        let line_parser = |(code, lowercase, uppercase, titlecase, conditions)| {
            let conditions = conditions
                .map(|(locale, contexts)| Conditions {
                    locale: locale.map(|l| l.to_string()),
                    contexts,
                })
                .unwrap_or_default();

            casing.insert_mapping(code, lowercase, uppercase, titlecase, conditions);
        };
        
        let (i, _) = ucd_lines(fields, line_parser).parse(input)?;
        Ok((i, casing))
    }
}

impl LoadFromFile for SpecialCasing {}

#[cfg(test)]
mod tests {
    use aglet_text::UnicodeContextKind;

    use super::*;
    use crate::ver;

    fn parse(input: &str) -> SpecialCasing {
        let res = SpecialCasing::parse(input, ver!());
        let res = parse::finish(res);
        if let Err(ref err) = res {
            eprintln!("{err}");
        }

        assert!(res.is_ok());
        res.unwrap()
    }

    #[test]
    fn test_simple_line() {
        let input = "1234; 1335; 1336; 1337; # comment";

        let casing = parse(input);
        assert_eq!(casing.mappings.len(), 1);

        let mappings = casing.mappings.get(&NO_CONDITIONS);
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 1);
        assert_eq!(mappings.uppercase_mapping.len(), 1);
        assert_eq!(mappings.titlecase_mapping.len(), 1);

        assert_eq!(mappings.lowercase_mapping.get(&0x1234), Some(&vec![0x1335]));
        assert_eq!(mappings.uppercase_mapping.get(&0x1234), Some(&vec![0x1336]));
        assert_eq!(mappings.titlecase_mapping.get(&0x1234), Some(&vec![0x1337]));
    }

    #[test]
    fn test_multibyte_mappings() {
        let input = "1234;1011 1012;1021 1022 1023;1100 1001 1002; #comment \n";

        let casing = parse(input);
        assert_eq!(casing.mappings.len(), 1);

        let mappings = casing.mappings.get(&NO_CONDITIONS);
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 1);
        assert_eq!(mappings.uppercase_mapping.len(), 1);
        assert_eq!(mappings.titlecase_mapping.len(), 1);

        assert_eq!(
            mappings.lowercase_mapping.get(&0x1234),
            Some(&vec![0x1011, 0x1012])
        );
        assert_eq!(
            mappings.uppercase_mapping.get(&0x1234),
            Some(&vec![0x1021, 0x1022, 0x1023])
        );
        assert_eq!(
            mappings.titlecase_mapping.get(&0x1234),
            Some(&vec![0x1100, 0x1001, 0x1002])
        );
    }

    #[test]
    fn test_conditional_mappings() {
        let input = &[
            "1234; 1111; 1112; 1113; en; #comment",
            "1234 ; ; 1221; ; en after_I ;#comment",
        ]
        .join("\n");

        let casing = parse(input);
        assert_eq!(casing.mappings.len(), 2);

        let mappings = casing.mappings.get(&Conditions {
            locale: Some("en".to_string()),
            contexts: vec![],
        });
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 1);
        assert_eq!(mappings.uppercase_mapping.len(), 1);
        assert_eq!(mappings.titlecase_mapping.len(), 1);

        assert_eq!(mappings.lowercase_mapping.get(&0x1234), Some(&vec![0x1111]));
        assert_eq!(mappings.uppercase_mapping.get(&0x1234), Some(&vec![0x1112]));
        assert_eq!(mappings.titlecase_mapping.get(&0x1234), Some(&vec![0x1113]));

        let mappings = casing.mappings.get(&Conditions {
            locale: Some("en".to_string()),
            contexts: vec![UnicodeContext {
                kind: UnicodeContextKind::AfterI,
                negated: false,
            }],
        });
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 0);
        assert_eq!(mappings.uppercase_mapping.len(), 1);
        assert_eq!(mappings.titlecase_mapping.len(), 0);

        assert_eq!(mappings.uppercase_mapping.get(&0x1234), Some(&vec![0x1221]));
    }

    #[test]
    fn test_data_rows() {
        let input = r#"
        # Remove DOT ABOVE after "i" with upper or titlecase

        0307; 0307; ; ; lt After_Soft_Dotted; # COMBINING DOT ABOVE

        # Introduce an explicit dot above when lowercasing capital I's and J's
        # whenever there are more accents above.
        # (of the accents used in Lithuanian: grave, acute, tilde above, and ogonek)

        0049; 0069 0307; 0049; 0049; lt More_Above; # LATIN CAPITAL LETTER I
        004A; 006A 0307; 004A; 004A; lt More_Above; # LATIN CAPITAL LETTER J"#;

        let casings = parse(input);

        let mappings = casings.mappings.get(&NO_CONDITIONS);
        assert!(mappings.is_none());

        let mappings = casings.mappings.get(&Conditions {
            locale: Some("lt".to_string()),
            contexts: vec![UnicodeContext {
                kind: UnicodeContextKind::AfterSoftDotted,
                negated: false,
            }],
        });
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 1);
        assert_eq!(mappings.uppercase_mapping.len(), 0);
        assert_eq!(mappings.titlecase_mapping.len(), 0);

        assert_eq!(mappings.lowercase_mapping.get(&0x0307), Some(&vec![0x0307]));
        assert_eq!(mappings.uppercase_mapping.get(&0x0307), None);
        assert_eq!(mappings.titlecase_mapping.get(&0x0307), None);

        let mappings = casings.mappings.get(&Conditions {
            locale: Some("lt".to_string()),
            contexts: vec![UnicodeContext {
                kind: UnicodeContextKind::MoreAbove,
                negated: false,
            }],
        });
        assert!(mappings.is_some());

        let mappings = mappings.unwrap();
        assert_eq!(mappings.lowercase_mapping.len(), 2);
        assert_eq!(mappings.uppercase_mapping.len(), 2);
        assert_eq!(mappings.titlecase_mapping.len(), 2);

        assert_eq!(
            mappings.lowercase_mapping.get(&0x0049),
            Some(&vec![0x0069, 0x0307])
        );
        assert_eq!(mappings.uppercase_mapping.get(&0x0049), Some(&vec![0x0049]));
        assert_eq!(mappings.titlecase_mapping.get(&0x0049), Some(&vec![0x0049]));

        assert_eq!(
            mappings.lowercase_mapping.get(&0x004A),
            Some(&vec![0x006A, 0x0307])
        );
        assert_eq!(mappings.uppercase_mapping.get(&0x004A), Some(&vec![0x004A]));
        assert_eq!(mappings.titlecase_mapping.get(&0x004A), Some(&vec![0x004A]));
    }
}
