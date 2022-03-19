#![feature(result_option_inspect)]

mod parse;
pub mod tokenize;

use parse::error::TokenizeError;
use tokenize::token::Token;

use eyre::Result;

pub fn tokenize<S>(input: &str) -> Result<Vec<Token>> {
    Err(TokenizeError::EndOfFile.into())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
