pub mod ast;
pub mod error;
mod input;
pub mod parser;
mod parser_macros;
mod pretty;

#[cfg(test)]
use aglet_text::Span;
pub use error::{Error, ErrorKind};
pub use parser::Parser;

#[cfg(test)]
use crate::tokenize::{self, Token, TokenKind};

#[cfg(test)]
pub(crate) fn token_iter(tokens: Vec<TokenKind>) -> impl Iterator<Item = tokenize::Result<Token>> {
    tokens.into_iter().map(|kind| {
        Ok(Token {
            span: Span::new(0, 0),
            kind,
        })
    })
}

#[cfg(test)]
pub(crate) fn span_token_iter(
    span_start: usize,
    tokens: Vec<(usize, TokenKind)>,
) -> impl Iterator<Item = tokenize::Result<Token>> {
    tokens
        .into_iter()
        .scan(span_start, |span_start, (span_len, kind)| {
            let start = *span_start;
            *span_start += span_len;
            Some((start, span_len, kind))
        })
        .map(|(span_start, span_len, kind)| {
            let tok = Token {
                span: Span::new(span_start, span_start + span_len),
                kind,
            };

            Ok(tok)
        })
}
