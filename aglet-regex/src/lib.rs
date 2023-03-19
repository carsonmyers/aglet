mod parse;
pub mod tokenize;

use aglet_text::Span;
use tokenize::token::Token;

pub fn tokenize<S>(_: &str) -> Result<Vec<Token>, tokenize::Error> {
    Err(tokenize::Error {
        span: Span::new(),
        kind: tokenize::ErrorKind::EndOfFile,
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
