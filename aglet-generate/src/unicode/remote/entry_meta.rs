use std::str::FromStr;

use eyre::eyre;

use crate::parse;

#[derive(Debug, Clone)]
pub enum Entry {
    File(File),
    Directory(Directory),
    Link(Link),
}

impl Entry {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::branch::alt;
        use nom::bytes::complete::tag;
        use nom::combinator::map;
        use nom::sequence::preceded;

        alt((
            preceded(tag("-"), map(File::parse, Entry::File)),
            preceded(tag("d"), map(Directory::parse, Entry::Directory)),
            preceded(tag("l"), map(Link::parse, Entry::Link)),
        ))(input)
    }

    pub fn is_file(&self) -> bool {
        matches!(self, Entry::File(_))
    }

    pub fn is_directory(&self) -> bool {
        matches!(self, Entry::Directory(_))
    }

    pub fn is_link(&self) -> bool {
        matches!(self, Entry::Link(_))
    }

    pub fn file(self) -> Option<File> {
        match self {
            Entry::File(file) => Some(file),
            _ => None,
        }
    }

    pub fn directory(self) -> Option<Directory> {
        match self {
            Entry::Directory(directory) => Some(directory),
            _ => None,
        }
    }

    pub fn link(self) -> Option<Link> {
        match self {
            Entry::Link(link) => Some(link),
            _ => None,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Entry::File(file) => file.name.as_str(),
            Entry::Directory(dir) => dir.name.as_str(),
            Entry::Link(link) => link.name.as_str(),
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Entry::File(file) => Some(file.size),
            _ => None,
        }
    }

    pub fn link_target(&self) -> Option<&str> {
        match self {
            Entry::Link(link) => Some(link.target.as_str()),
            _ => None,
        }
    }
}

impl FromStr for Entry {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::Finish;

        match Self::parse(s).finish() {
            Ok((_, entry)) => Ok(entry),
            Err(nom::error::Error { input, code }) => {
                Err(eyre!("invalid line (...{}): {:?}", input, code))
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub name: String,
    pub size: usize,
}

impl File {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::combinator::map;
        use nom::sequence::{preceded, tuple};
        use parse::non_space;
        use parse::skip_space;

        map(
            tuple((
                parse_common,
                preceded(skip_space, map(non_space, str::to_string)),
            )),
            |(size, name)| Self { name, size },
        )(input)
    }
}

#[derive(Debug, Clone)]
pub struct Directory {
    pub name: String,
}

impl Directory {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::combinator::map;
        use nom::sequence::preceded;
        use parse::non_space;
        use parse::skip_space;

        map(
            preceded(
                parse_common,
                preceded(skip_space, map(non_space, str::to_string)),
            ),
            |name| Self { name },
        )(input)
    }
}

#[derive(Debug, Clone)]
pub struct Link {
    pub name: String,
    pub target: String,
}

impl Link {
    pub fn parse(input: &str) -> parse::Result<Self> {
        use nom::bytes::complete::tag;
        use nom::combinator::map;
        use nom::sequence::{preceded, separated_pair};
        use parse::non_space;
        use parse::skip_space;

        map(
            preceded(
                parse_common,
                preceded(
                    skip_space,
                    separated_pair(
                        map(non_space, str::to_string),
                        tag(" -> "),
                        map(non_space, str::to_string),
                    ),
                ),
            ),
            |(name, target)| Self { name, target },
        )(input)
    }
}

pub fn parse_common(input: &str) -> parse::Result<usize> {
    use nom::branch::alt;
    use nom::character::complete::digit1;
    use nom::combinator::map_res;
    use nom::sequence::{delimited, tuple};
    use parse::find_digit;
    use parse::month;
    use parse::skip_space;
    use parse::time;

    delimited(
        tuple((find_digit, digit1, find_digit)),
        map_res(digit1, str::parse::<usize>),
        tuple((
            skip_space,
            month,
            skip_space,
            digit1,
            skip_space,
            alt((time, digit1)),
        )),
    )(input)
}
