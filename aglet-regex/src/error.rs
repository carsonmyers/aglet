use crate::{parse, tokenize};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("tokenizer error: {0}")]
    TokenizerError(#[from] tokenize::Error),

    #[error("parser error: {0}")]
    ParserError(#[from] parse::Error),
}
