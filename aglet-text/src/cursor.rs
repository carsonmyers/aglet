use std::convert::From;
use std::mem;
use std::str::Chars;

use crate::Span;

pub struct Cursor<'a> {
    src: Chars<'a>,
    tok: Chars<'a>,
    n:   usize,
    t:   usize,
}

impl<'a> Cursor<'a> {
    pub fn new<T: Into<&'a str>>(src: T) -> Self {
        let chars = src.into().chars();
        Cursor {
            src: chars.clone(),
            tok: chars,
            n:   0,
            t:   0,
        }
    }

    pub fn first(&self) -> Option<char> {
        self.src.clone().next()
    }

    pub fn second(&self) -> Option<char> {
        self.src.clone().skip(1).next()
    }

    pub fn next(&mut self) -> Option<char> {
        match self.src.next() {
            Some(c) => {
                self.n += 1;
                Some(c)
            },
            _ => None,
        }
    }

    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    pub fn bump(&mut self) {
        match self.src.next() {
            Some(_) => self.n += 1,
            _ => (),
        }
    }

    pub fn span(&self) -> Span {
        Span::new(self.t, self.n)
    }

    pub fn take_span(&mut self) -> Span {
        let span = Span::new(self.t, self.n);
        self.t = self.n;

        span
    }

    pub fn text(&self) -> String {
        self.tok.clone().take(self.n - self.t).collect::<String>()
    }

    pub fn take_text(&mut self) -> String {
        let mut next_tok = self.src.clone();
        mem::swap(&mut next_tok, &mut self.tok);
        let text = next_tok.take(self.n - self.t).collect::<String>();
        self.t = self.n;

        text
    }

    pub fn map<B, F>(&mut self, f: F) -> B
    where
        F: FnOnce(Span, String) -> B,
    {
        let span = self.span();
        let text = self.take_text();

        f(span, text)
    }

    pub fn map_span<B, F>(&mut self, f: F) -> B
    where
        F: FnOnce(Span) -> B,
    {
        f(self.take_span())
    }

    pub fn matches(&mut self, c: char) -> bool {
        match self.first() {
            Some(found) if found == c => {
                self.skip(1);
                true
            },
            _ => false,
        }
    }

    pub fn reset(&mut self) {
        self.tok = self.src.clone();
        self.t = self.n;
    }
}

impl<'a, T: Into<&'a str>> From<T> for Cursor<'a> {
    fn from(value: T) -> Self {
        Cursor::new(value)
    }
}
