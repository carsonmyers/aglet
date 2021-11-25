mod parse;

use parse::token::Token;
use parse::error::ParseError;
use eyre::Result;

pub fn tokenize<S>(input: &str) -> Result<Vec<Token>> {
    Err(ParseError::EndOfFile.into())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
