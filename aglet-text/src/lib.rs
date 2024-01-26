mod cursor;
mod char_range;
mod error;
mod source_map;
mod span;

use std::default::Default;

pub use cursor::Cursor;
pub use error::Error;
pub use source_map::{FileId, SourceFile, SourceMap};
pub use span::{FilePosition, FileSpan, Span, StringSpan};

pub trait DefaultWithSpan
where
    Self: Default,
{
    fn default_with_span(span: Span) -> Self;
}
