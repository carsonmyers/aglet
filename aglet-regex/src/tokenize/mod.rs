pub mod error;
mod pretty;
mod state;
pub mod token;
pub mod tokenizer;

pub use error::{Error, ErrorKind, Result};
pub use token::{Token, TokenKind};
pub use tokenizer::Tokenizer;

#[cfg(test)]
pub(crate) fn assert_tokens(mut tr: Tokenizer, tokens: Vec<TokenKind>) {
    let total_expected = tokens.len();
    let mut count = 0;
    for expected in tokens {
        let actual = tr
            .next()
            .expect("ran out of tokens")
            .expect("received error")
            .kind;
        assert_eq!(expected, actual);
        count += 1;
    }

    assert_eq!(total_expected, count);
    assert_next_none!(tr);
}

#[cfg(test)]
macro_rules! assert_next_tok {
    ( $tokenizer:expr , $kind:pat ) => {
        assert!(matches!(
            $tokenizer.next(),
            Some(Ok(Token { kind: $kind, .. }))
        ));
    };
}

#[cfg(test)]
macro_rules! assert_next_err {
    ( $tokenizer:expr , $kind:pat ) => {
        assert!(matches!(
            $tokenizer.next(),
            Some(Err(Error { kind: $kind, .. }))
        ));
    };
}

#[cfg(test)]
macro_rules! assert_next_none {
    ( $tokenizer:expr ) => {
        assert!(matches!($tokenizer.next(), None));
    };
}

#[cfg(test)]
pub(crate) use assert_next_err;
#[cfg(test)]
pub(crate) use assert_next_none;
#[cfg(test)]
pub(crate) use assert_next_tok;
