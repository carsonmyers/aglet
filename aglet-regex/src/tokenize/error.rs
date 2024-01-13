use std::{fmt, ops, result};

use aglet_text::Span;

use crate::tokenize::state::StateStack;

pub type Result<T> = result::Result<T, Error>;
pub type StackResult<T> = result::Result<T, StackError>;

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub span:  Span,
    pub cause: ErrorCause,
}

impl Error {
    pub fn new(span: Span, cause: ErrorCause) -> Self {
        Error { span, cause }
    }

    pub fn is_fatal(&self) -> bool {
        matches!(self.cause, ErrorCause::FatalError(_))
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.cause, ErrorCause::Error(ErrorKind::EndOfFile))
    }

    pub fn is_any_eof(&self) -> bool {
        matches!(
            self.cause,
            ErrorCause::Error(ErrorKind::EndOfFile | ErrorKind::UnexpectedEOF(_))
        )
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}) {}", self.span, self.cause)
    }
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub struct StackError {
    pub error: Error,
    pub stack: StateStack,
}

impl StackError {
    pub fn from_error(error: Error, stack: StateStack) -> Self {
        StackError { error, stack }
    }
}

impl fmt::Display for StackError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}) {}", self.span, self.cause)
    }
}

impl ops::Deref for StackError {
    type Target = Error;

    fn deref(&self) -> &Self::Target {
        &self.error
    }
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorCause {
    #[error("{0}")]
    Error(#[from] ErrorKind),

    #[error("fatal error: {0}")]
    FatalError(#[from] FatalErrorKind),
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("end of file")]
    EndOfFile,

    #[error("unexpected end of file: {0}")]
    UnexpectedEOF(String),

    #[error("unexpected `{0}`")]
    UnexpectedChar(char),

    #[error("invalid hex digit `{0}`")]
    InvalidHexDigit(char),

    #[error("invalid character code `{0}`")]
    InvalidCharCode(String),

    #[error("unrecognized escape sequence: \\{0}")]
    UnrecognizedEscape(char),

    #[error("unrecognized flag: {0}")]
    UnrecognizedFlag(char),
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum FatalErrorKind {
    #[error("state stack is empty")]
    EmptyStateStack,

    #[error("internal state error: {0}")]
    InternalStateError(StateError),

    #[error("not implemented")]
    NotImplemented,
}

impl From<StateError> for ErrorCause {
    fn from(value: StateError) -> Self {
        ErrorCause::FatalError(value.into())
    }
}

impl From<StateError> for FatalErrorKind {
    fn from(err: StateError) -> Self {
        FatalErrorKind::InternalStateError(err)
    }
}

#[derive(thiserror::Error, Clone, Debug, Eq, PartialEq)]
pub enum StateError {
    #[error("no state on tokenizer stack")]
    NoStateOnStack,

    #[error("cannot pop final state from tokenizer stack")]
    PoppedFinalState,
}
