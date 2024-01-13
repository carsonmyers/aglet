use std::convert::From;
use std::{fmt, result};

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

impl From<tokenize::Error> for Error {
    fn from(value: tokenize::Error) -> Self {
        Error {
            span: value.span,
            kind: ErrorKind::TokenizeError(value.cause),
        }
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("tokenizer error: {0}")]
    TokenizeError(tokenize::ErrorCause),

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

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum TokenConvertError {
    #[error("invalid token for boundary: {0:?}")]
    InvalidTokenForBoundary(tokenize::TokenKind),

    #[error("invalid token for repetition: {0:?}")]
    InvalidTokenForRepetition(tokenize::TokenKind),

    #[error("invalid posix class: {0}")]
    InvalidPosixClass(String),
}
