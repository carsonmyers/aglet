use std::convert::From;
use std::result;

use aglet_text::Span;
use thiserror::Error;

use crate::tokenize;

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("tokenizer error: {0}")]
    TokenizeError(tokenize::ErrorKind),

    #[error("internal state error: {0}")]
    InternalStateError(StateError),

    #[error("unexpected end of input: {0}")]
    UnexpectedEOF(String),

    #[error("unexpected token: {0:?}")]
    UnexpectedToken(tokenize::TokenKind, String),

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
pub enum StateError {
    #[error("no state on tokenizer stack")]
    NoStateOnStack,

    #[error("cannot pop final state from tokenizer stack")]
    PoppedFinalState,
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
