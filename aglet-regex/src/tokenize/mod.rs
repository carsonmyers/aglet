pub mod error;
mod pretty;
mod state;
pub mod token;
pub mod tokenizer;

pub use error::{Error, ErrorKind, Result};
pub use token::{Flag, Token, TokenKind};
pub use tokenizer::Tokenizer;

#[cfg(test)]
macro_rules! assert_next_tok {
    ( $tokenizer:expr , $kind:pat ) => {
        let next = $tokenizer.next();
        if !matches!(
            next,
            Some(Ok(Token { kind: $kind, .. }))
        ) {
            panic!("{:?} does not match token {}", next, stringify!($kind));
        }
    };
}

#[cfg(test)]
macro_rules! assert_next_err {
    ( $tokenizer:expr , $kind:pat ) => {
        let next = $tokenizer.next();
        if !matches!(
            next,
            Some(Err(Error { kind: $kind, .. }))
        ) && !matches!(
            next,
            Some(Ok(Token { kind: TokenKind::Error(Error { kind: $kind, .. }), .. }))
        ) {
            panic!("{:?} does not match error {}", next, stringify!($kind));
        }
    };
}

#[cfg(test)]
macro_rules! assert_next_none {
    ( $tokenizer:expr ) => {
        let next = $tokenizer.next();
        if !next.is_none() {
            panic!("{:?} is not None", next);
        }
    };
}

#[cfg(test)]
pub(crate) fn assert_tokens(mut tr: Tokenizer, tokens: Vec<TokenKind>) {
    let total_expected = tokens.len();
    let mut count = 0;
    for (i, expected) in tokens.iter().enumerate() {
        let actual = tr
            .next()
            .expect(&format!("ran out of tokens; expecting {:?} ({})", expected, i))
            .unwrap_or_else(|err| panic!("received error {:?}; expecting {:?} ({})", err, expected, i))
            .kind;

        assert_eq!(expected, &actual, "failed to match token {}", i);
        count += 1;
    }

    assert_eq!(total_expected, count);
    assert_next_none!(tr);
}

#[cfg(test)]
pub(crate) use assert_next_err;
#[cfg(test)]
pub(crate) use assert_next_none;
#[cfg(test)]
pub(crate) use assert_next_tok;