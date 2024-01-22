use std::convert::From;
use std::{fmt, result};

use aglet_text::Span;

use crate::parse;

pub type Result<T> = result::Result<T, Error>;

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}) {}", self.span, self.kind)
    }
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("parse error: {0}")]
    ParseError(parse::ErrorKind),
    #[error("not implemented")]
    NotImplemented,
}

impl From<parse::Error> for Error {
    fn from(value: parse::Error) -> Self {
        Error {
            span: value.span,
            kind: ErrorKind::ParseError(value.kind),
        }
    }
}
