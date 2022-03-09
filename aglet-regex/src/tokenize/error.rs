use thiserror::Error;
use std::result;

pub type Result<T> = result::Result<T, TokenizeError>;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum TokenizeError {
    #[error("end of file")]
    EndOfFile,

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
}