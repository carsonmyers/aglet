use std::result;

use aglet_text::Span;
use thiserror::Error;

use crate::tokenize::TokenizeError;

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    #[error("tokenize error: {0}")]
    TokenizeError(TokenizeError),

    #[error("internal state error: {0}")]
    InternalStateError(StateError),

    #[error("not implemented")]
    NotImplemented,
}

#[derive(Error, CLone, Debug, Eq, PartialEq)]
pub enum StateError {
    #[error("no state on tokenizer stack")]
    NoStateOnStack,

    #[error("cannot pop final state from tokenizer stack")]
    PoppedFinalState,
}
