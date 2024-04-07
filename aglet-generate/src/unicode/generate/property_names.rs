use std::collections::HashSet;

use crate::parse;

pub struct PropertyName {
    pub aliases: HashSet<String>,
}

impl PropertyName {
    pub fn new<T, U>(aliases: T) -> Self
    where
        T: AsRef<[U]>,
        U: AsRef<str>,
    {
        let mut set = HashSet::new();
        for alias in aliases.as_ref() {
            set.insert(alias.as_ref().to_string());
        }

        Self { aliases: set }
    }

    pub fn parse(input: &str) -> parse::Result<Option<Self>> {
        use nom::branch::alt;

        alt((Self::parse_blank_line, Self::parse_data_line))(input)
    }

    fn parse_blank_line(input: &str) -> parse::Result<Option<Self>> {
        use nom::combinator::{map, opt};
        use parse::comment;

        map(opt(comment), |_| None)(input)
    }

    fn parse_data_line(input: &str) -> parse::Result<Option<Self>> {
        use nom::bytes::complete::tag;
        use nom::combinator::{map, opt};
        use nom::multi::separated_list1;
        use nom::sequence::{terminated, tuple};
        use parse::{comment, identifier, skip_space};

        map(
            terminated(
                separated_list1(
                    tuple((skip_space, tag(";"), skip_space)),
                    map(identifier, str::to_string),
                ),
                opt(comment),
            ),
            |values| Some(Self::new(values)),
        )(input)
    }
}
