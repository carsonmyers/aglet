use std::iter::Peekable;

use aglet_text::Span;

use crate::parse::error::*;
use crate::tokenize::{self, Token};

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

    pub fn error(&self, kind: ErrorKind) -> Error {
        Error {
            span: self.span,
            kind,
        }
    }
}

