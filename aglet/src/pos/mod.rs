pub struct Pos {
    pub idx: u64,
    pub row: u64,
    pub col: u64,
}

pub struct Span {
    pub start: Pos,
    pub end: Pos,
}