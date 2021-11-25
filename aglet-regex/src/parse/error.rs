use thiserror::Error;
use std::result;

pub type Result<T> = result::Result<T, ParseError>;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ParseError {
    #[error("end of file")]
    EndOfFile,
}