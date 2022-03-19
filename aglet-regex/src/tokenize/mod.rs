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

#[cfg(test)]
pub(crate) fn assert_tokens_result(
    mut tr: Tokenizer,
    results: Vec<std::result::Result<TokenKind, TokenizeError>>
) {
    let total_expected = results.len();
    let mut count = 0;
    for expected_results in results {
        match (tr.next_token(), expected_results) {
            (Ok(actual), Ok(expected)) => {
                assert_eq!(actual.kind, expected);
            },
            (Err(actual), Err(expected)) => {
                assert_eq!(actual, expected);
            }
            _ => panic!("value does not match expected"),
        }
    }
}