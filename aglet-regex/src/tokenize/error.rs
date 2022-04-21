use std::result;

use aglet_text::Span;
use thiserror::Error;

pub type Result<T> = result::Result<T, TokenizeError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenizeError {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
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
    fn from(err: StateError) -> Self { ErrorKind::InternalStateError(err) }
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum StateError {
    #[error("no state on tokenizer stack")]
    NoStateOnStack,

    #[error("cannot pop final state from tokenizer stack")]
    PoppedFinalState,
}
