mod ast_printer;
mod error;
mod writer;

use std::fmt::{self, Write};

use ast_printer::AstPrinter;
pub use error::*;
use writer::Writer;

pub struct PrettyPrinter {
    indent: String,
}

impl PrettyPrinter {
    pub fn new(indent: &'_ str) -> Self {
        Self {
            indent: indent.to_string(),
        }
    }

    pub fn print(&self, item: &impl Pretty) -> Result {
        let out = self.print_buf(item)?;
        println!("{}", out);
        Ok(())
    }

    fn print_buf(&self, item: &impl Pretty) -> std::result::Result<String, Error> {
        let mut buf = String::new();
        let mut col = String::new();

        let mut writer = Writer::new(&mut buf, &mut col).with_indent(&self.indent);
        item.print(&mut writer)?;

        let col_lines = col.lines();
        let max_length = col_lines
            .clone()
            .map(|line| line.len())
            .max()
            .unwrap_or_default();

        Ok(col_lines
            .zip(buf.lines())
            .map(|(col, buf)| format!("{:<max_length$}\t{}", col, buf))
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

pub trait Pretty {
    fn print(&self, w: &mut Writer<'_>) -> Result;
}

impl<T: fmt::Debug> Pretty for Vec<T> {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        write!(w, "{:?}", self).map_err(|err| err.into())
    }
}
