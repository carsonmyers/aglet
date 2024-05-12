use std::fmt::{Display, Formatter};
use std::num::ParseIntError;

use nom::error::{FromExternalError, ParseError};
use nom::IResult;

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
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} parse errors:", self.errors.len())?;
        for (input, err) in &self.errors {
            writeln!(f, "\t{:?}: {}", err, trim_input(input, 50))?;
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
    Nom(nom::error::ErrorKind),
    Range,
    Codepoint,
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

    return input;
}
