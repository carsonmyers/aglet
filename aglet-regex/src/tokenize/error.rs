use std::fmt;
use std::result;

use aglet_derive::*;
use aglet_pretty::SpanPrinter;
use aglet_text::{Cursor, Span};

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
    #[error("end of file")]
    EndOfFile,

    #[error("unexpected end of file: {0}")]
    UnexpectedEOF(String),

    #[error("unexpected `{0}`")]
    UnexpectedChar(char),

    #[error("state stack is empty")]
    EmptyStateStack,

    #[error("invalid hex digit `{0}`")]
    InvalidHexDigit(char),

    #[error("invalid character code `{0}`")]
    InvalidCharCode(String),

    #[error("unrecognized escape sequence: \\{0}")]
    UnrecognizedEscape(char),

    #[error("unrecognized flag: {0}")]
    UnrecognizedFlag(char),

    #[error("not implemented")]
    NotImplemented,

    #[error("internal state error: {0}")]
    InternalStateError(StateError),
}

impl From<StateError> for ErrorKind {
    fn from(err: StateError) -> Self {
        ErrorKind::InternalStateError(err)
    }
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum StateError {
    #[error("no state on tokenizer stack")]
    NoStateOnStack,

    #[error("cannot pop final state from tokenizer stack")]
    PoppedFinalState,
}
