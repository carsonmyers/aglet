use std::default::Default;
use std::fmt;

use crate::source_map::FileId;
use crate::DefaultWithSpan;

#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end:   usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn wrap(start: &Span, end: &Span) -> Self {
        Span {
            start: start.start,
            end:   end.end,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub struct FileSpan {
    pub file_id: FileId,
    pub start:   FilePosition,
    pub end:     FilePosition,
}

impl fmt::Debug for FileSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<file_id {}> {:?}-{:?}",
            self.file_id, self.start, self.end
        )
    }
}

impl fmt::Display for FileSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub struct FilePosition {
    pub offset: usize,
    pub line:   usize,
    pub column: usize,
}

impl fmt::Debug for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}[{}]", self.line, self.column, self.offset)
    }
}

impl fmt::Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Default, Debug)]
pub struct StringSpan {
    pub span:  Span,
    pub value: String,
}

impl DefaultWithSpan for StringSpan {
    fn default_with_span(span: Span) -> Self {
        Self {
            span,
            ..Default::default()
        }
    }
}
