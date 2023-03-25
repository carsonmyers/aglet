use std::convert::From;
use std::fmt;
use std::result;

use aglet_text::Span;
use thiserror::Error;

use crate::tokenize;

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

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("tokenizer error: {0}")]
    TokenizeError(tokenize::ErrorKind),

    #[error("unexpected end of input: expected {0}")]
    UnexpectedEOF(String),

    #[error("unexpected token {0:?}: expected {1}")]
    UnexpectedToken(tokenize::TokenKind, String),

    #[error("empty character class")]
    EmptyClass,

    #[error("unexpected token: {0}")]
    TokenConvertError(TokenConvertError),

    #[error("not implemented")]
    NotImplemented,
}

impl From<tokenize::Error> for Error {
    fn from(value: tokenize::Error) -> Self {
        Error {
            span: value.span,
            kind: ErrorKind::TokenizeError(value.kind),
        }
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum TokenConvertError {
    #[error("invalid token for boundary: {0:?}")]
    InvalidTokenForBoundary(tokenize::TokenKind),

    #[error("invalid token for repetition: {0:?}")]
    InvalidTokenForRepetition(tokenize::TokenKind),

    #[error("invalid posix class: {0}")]
    InvalidPosixClass(String),

    #[error("invalid flag: {0}")]
    InvalidFlag(char),
}
