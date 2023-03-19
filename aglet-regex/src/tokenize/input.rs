use std::iter::Peekable;
use std::str::Chars;

use aglet_text::{Position, Span};

use crate::tokenize::error::*;
use crate::tokenize::token::*;

pub(crate) struct Input<'a> {
    data: Peekable<Chars<'a>>,
    n:    Position,
    t:    Position,
}

impl<'a> Input<'a> {
    pub fn new<T: Into<&'a str>>(data: T) -> Self {
        Input {
            data: data.into().chars().peekable(),
            n:    Position::new(),
            t:    Position::new(),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.data.peek().map(|c| *c)
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.data.next() {
            if c == '\n' {
                self.n.newline();
            } else {
                self.n.forward();
            }

            Some(c)
        } else {
            None
        }
    }

    pub fn token(&mut self, kind: TokenKind) -> Token {
        let span = Span::from(self.t, self.n);
        self.t.set(&self.n);

        Token { span, kind }
    }

    pub fn error(&mut self, kind: ErrorKind) -> Error {
        let span = Span::from(self.t, self.n);
        self.t.set(&self.n);

        Error { span, kind }
    }

    pub fn expect(&mut self, c: char) -> Result<()> {
        match self.next() {
            Some(found) if found == c => Ok(()),
            Some(c) => Err(self.error(ErrorKind::UnexpectedChar(c))),
            None => Err(self.error(ErrorKind::EndOfFile)),
        }
    }

    pub fn matches(&mut self, c: char) -> bool {
        match self.peek() {
            Some(found) if found == c => {
                self.next();
                true
            },
            _ => false,
        }
    }
}
