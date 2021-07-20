use color_eyre::{
    eyre::{eyre, Report}
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum IngestError {
    #[error("input exhausted")]
    InputExhausted,
    #[error("end of file")]
    EndOfFile,
}