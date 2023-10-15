mod cursor;
mod error;
mod source_map;
mod span;

use std::default::Default;

pub use cursor::Cursor;
pub use error::Error;
pub use source_map::{FileId, SourceMap, SourceFile};
pub use span::{FilePosition, FileSpan, Span};

pub trait DefaultWithSpan where Self: Default {
    fn default_with_span(span: Span) -> Self;
}