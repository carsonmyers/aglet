use std::fmt;

pub type Result = std::result::Result<(), Error>;

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum Error {
    #[error("format error: {0}")]
    FormatError(#[from] fmt::Error),

    #[error("children have already been written")]
    ChildrenAlreadyWritten,
}
