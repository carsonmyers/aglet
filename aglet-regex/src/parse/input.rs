use std::iter::Peekable;

use aglet_text::Span;

use crate::parse::error::*;
use crate::tokenize::{self, Token, TokenKind};

pub(crate) struct Input<'a> {
    data: Peekable<Box<dyn Iterator<Item = tokenize::Result<Token>> + 'a>>,
    span: Span,
}

impl<'a> Input<'a> {
    pub fn new<T>(data: T) -> Self
    where
        T: Iterator<Item = tokenize::Result<Token>> + 'a,
    {
        let data = Box::new(data) as Box<dyn Iterator<Item = tokenize::Result<Token>>>;
        Self {
            data: data.peekable(),
            span: Span::new(0, 0),
        }
    }

    pub fn position(&self) -> usize {
        self.span.end
    }

    pub fn peek(&mut self) -> Option<Result<&Token>> {
        match self.data.peek() {
            None => None,
            Some(Ok(tok)) => Some(Ok(tok)),
            Some(Err(err)) => Some(Err(Error::from(err.clone()))),
        }
    }

    pub fn peek_kind(&mut self) -> Option<Result<&TokenKind>> {
        match self.data.peek() {
            None => None,
            Some(Ok(tok)) => Some(Ok(&tok.kind)),
            Some(Err(err)) => Some(Err(Error::from(err.clone()))),
        }
    }

    #[inline]
    pub fn has_where<F>(&mut self, f: F) -> Result<bool>
    where
        F: Fn(&TokenKind) -> bool,
    {
        match self.peek_kind() {
            None => Ok(false),
            Some(Err(err)) => Err(err),
            Some(Ok(kind)) => Ok(f(kind)),
        }
    }

    pub fn next(&mut self) -> Option<Result<Token>> {
        let item = self
            .data
            .next()
            .map(|res| res.map_err(|err| Error::from(err)));

        if let Some(Ok(tok)) = &item {
            self.span = tok.span;
        }

        item
    }

    #[inline]
    pub fn match_where<F>(&mut self, f: F) -> Result<Option<Token>>
    where
        F: Fn(&TokenKind) -> bool,
    {
        if self.has_where(f)? {
            Ok(Some(self
                .next()
                .expect("next token matches previous peek")
                .expect("next token matches previous peek")))
        } else {
            Ok(None)
        }
    }

    pub fn error(&self, kind: ErrorKind) -> Error {
        Error {
            span: self.span,
            kind,
        }
    }
}
