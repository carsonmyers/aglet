use super::{
    span::{Span, Position},
    error::{Result, ParseError},
};
use std::str::Chars;

pub trait Reader {
    fn next(&mut self) -> Result<char>;
    fn peek(&mut self) -> Result<char>;

    fn take_lexeme(&mut self) -> Span;
    fn peek_lexeme(&mut self) -> Span;
    fn discard_lexeme(&mut self);
}

struct StringReader<'a> {
    text: &'a str,
    chars: Chars<'a>,
    n: Position,
    p: Position,
    l: Position,
}

impl <'a> StringReader<'a> {
    pub fn new<T>(data: T) -> StringReader<'a>
        where T: Into<&'a str>
    {
        let text = data.into();

        StringReader {
            text: text,
            chars: text.chars(),
            n: Position::new(),
            p: Position::new(),
            l: Position::new(),
        }
    }

    pub fn next(&mut self) -> Result<char> {
        let c = self.chars.nth(self.n.idx)
            .ok_or(ParseError::EndOfFile)?;

        self.n.advance(c);
        self.p.set(self.n);

        Ok(c)
    }

    pub fn peek(&mut self) -> Result<char> {
        let c = self.chars.nth(self.p.idx)
            .ok_or(ParseError::EndOfFile)?;

        self.p.advance(c);

        Ok(c)
    }

    pub fn take_lexeme(&mut self) -> Span {
        let result = Span {
            text: &self.text[self.l.idx..self.n.idx],
            start: self.l,
            end: self.n,
        };

        self.l.set(self.n);
        self.p.set(self.n);

        result
    }

    pub fn peek_lexeme(&mut self) -> Span {
        Span {
            text: &self.text[self.l.idx..self.p.idx],
            start: self.l,
            end: self.n,
        }
    }

    pub fn discard_lexeme(&mut self) {
        self.l.set(self.n);
        self.p.set(self.n);
    }

    fn advance_ptr(&self, ptr: &mut Position, c: char) {
        ptr.idx += 1;

        if c == '\n' {
            ptr.line += 1;
            ptr.col = 1;
        } else {
            ptr.col += 1;
        }
    }

    fn to_n(&self, pos: &mut Position) {
        pos.idx = self.n.idx;
        pos.line = self.n.line;
        pos.col = self.n.col;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut reader = StringReader::new("abcd");
        assert_eq!(reader.next().ok(), Some('a'));
        assert_eq!(reader.next().ok(), Some('b'));
        assert_eq!(reader.next().ok(), Some('c'));
        assert_eq!(reader.next().ok(), Some('d'));
        assert_eq!(reader.next().err(), Some(ParseError::EndOfFile));
    }

    #[test]
    fn text_peek() {
        let mut reader = StringReader::new("abcd");
        assert_eq!(reader.next().ok(), Some('a'));
        assert_eq!(reader.peek().ok(), Some('b'));
        assert_eq!(reader.peek().ok(), Some('c'));
        assert_eq!(reader.peek().ok(), Some('d'));
        assert_eq!(reader.next().ok(), Some('b'));
        assert_eq!(reader.peek().ok(), Some('c'));
        assert_eq!(reader.peek().ok(), Some('d'));
        assert_eq!(reader.peek().err(), Some(ParseError::EndOfFile));
    }
}