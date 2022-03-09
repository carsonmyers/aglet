#[derive(Copy, Clone)]
pub struct Position {
    pub idx: usize,
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub fn new() -> Self {
        Position {
            idx: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn set(&mut self, other: Position) {
        self.idx = other.idx;
        self.line = other.line;
        self.col = other.col;
    }

    pub fn advance(&mut self, c: char) {
        self.idx += 1;

        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}
