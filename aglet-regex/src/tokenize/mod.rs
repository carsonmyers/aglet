pub mod error;
pub(crate) mod input;
pub(crate) mod state;
pub mod token;
pub mod tokenizer;

pub use error::{Result, TokenizeError};
pub use token::{Token, TokenKind};
pub use tokenizer::Tokenizer;

#[cfg(test)]
pub(crate) fn assert_tokens(mut tr: Tokenizer, tokens: Vec<TokenKind>) {
    let total_expected = tokens.len();
    let mut count = 0;
    for expected in tokens {
        if let Ok(actual) = tr.next_token() {
            assert_eq!(actual.kind, expected);
        } else {
            assert_eq!(count, total_expected);
        }
        count += 1;
    }

    assert!(matches!(tr.next_token(), Err(TokenizeError::EndOfFile)));
}