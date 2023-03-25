use std::fmt;

#[derive(Clone, Copy, Eq, PartialEq, Default)]
pub struct Span {
    pub start: Position,
    pub end:   Position,
}

impl Span {
    pub fn new() -> Self {
        Span {
            start: Position::new(),
            end:   Position::new(),
        }
    }

    pub fn from_offsets(start: usize, end: usize) -> Self {
        Span {
            start: Position::from_offset(start),
            end:   Position::from_offset(end),
        }
    }

    pub fn from(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    pub fn wrap(start: Span, end: Span) -> Self {
        Span {
            start: start.start,
            end:   end.end,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.start)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Default)]
pub struct Position {
    pub offset: usize,
    pub line:   usize,
    pub column: usize,
}

impl Position {
    pub fn new() -> Self {
        Position {
            offset: 0,
            line:   1,
            column: 1,
        }
    }

    pub fn from_offset(offset: usize) -> Self {
        Position {
            offset,
            line: 1,
            column: offset + 1,
        }
    }

    pub fn forward(&mut self) {
        self.offset += 1;
        self.column += 1;
    }

    pub fn newline(&mut self) {
        self.offset += 1;
        self.line += 1;
        self.column = 1;
    }

    pub fn set(&mut self, other: &Self) {
        self.offset = other.offset;
        self.line = other.line;
        self.column = other.column;
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}[{}]", self.line, self.column, self.offset)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, col {}", self.line, self.column)
    }
}
