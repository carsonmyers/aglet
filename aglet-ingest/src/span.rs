use crate::position::Position;

pub struct Span<'a> {
    pub text: &'a str,
    pub start: Position,
    pub end: Position,
}

impl <'a> Span<'a> {
    pub fn new(text: &'a str) -> Self {
        Span {
            text,
            start: Position::new(),
            end: Position::new(),
        }
    }
}
