use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

use aglet_text::Error as AgletTextError;
use nom::error::{FromExternalError, ParseError};
use nom::IResult;

use crate::unicode::ucd::UcdParseError;

pub type Result<'a, T> = IResult<&'a str, T, Error<'a>>;

#[derive(thiserror::Error, Debug, PartialEq)]
pub struct Error<'a> {
    errors: Vec<(&'a str, ErrorKind)>,
}

impl<'a> Error<'a> {
    pub fn range(input: &'a str) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Range)],
        }
    }

    pub fn with_context(mut self, input: &'a str, context: &'static str) -> Self {
        self.errors.push((input, ErrorKind::Context(context)));
        self
    }

    pub fn append_context(mut self, context: &'static str) -> Self {
        match self.errors.last() {
            Some((i, _)) => self.errors.push((*i, ErrorKind::Context(context))),
            None => panic!("no error to which to attach a context"),
        };
        self
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} parse error(s):", self.errors.len())?;
        for (input, err) in &self.errors {
            write!(f, "\n\t{:?}: {}", err, trim_input(input, 50))?;
        }

        Ok(())
    }
}

impl<'a> FromExternalError<&'a str, ParseIntError> for Error<'a> {
    fn from_external_error(input: &'a str, _: nom::error::ErrorKind, _: ParseIntError) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Codepoint)],
        }
    }
}

impl<'a> FromExternalError<&'a str, AgletTextError> for Error<'a> {
    fn from_external_error(input: &'a str, _: nom::error::ErrorKind, e: AgletTextError) -> Self {
        let kind = match e {
            AgletTextError::UnsupportedUnicodeContext(_) => ErrorKind::UnicodeContext,
            _ => ErrorKind::Unknown,
        };

        Self {
            errors: vec![(input, kind)],
        }
    }
}

impl<'a> FromExternalError<&'a str, UcdParseError> for Error<'a> {
    fn from_external_error(input: &'a str, _: nom::error::ErrorKind, e: UcdParseError) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Ucd(e))],
        }
    }
}

impl<'a> ParseError<&'a str> for Error<'a> {
    fn from_error_kind(input: &'a str, kind: nom::error::ErrorKind) -> Self {
        Self {
            errors: vec![(input, ErrorKind::Nom(kind))],
        }
    }

    fn append(input: &'a str, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::Nom(kind)));
        other
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Context(&'static str),
    Nom(nom::error::ErrorKind),
    Range,
    Codepoint,
    UnicodeContext,
    Ucd(UcdParseError),
    Unknown,
}

fn trim_input(input: &str, len: usize) -> &str {
    for (i, c) in input.chars().enumerate() {
        if i >= len {
            return &input[..i];
        }

        if c == '\n' || c == '\r' {
            return &input[..i];
        }
    }

    input
}
