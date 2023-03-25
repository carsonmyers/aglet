mod ast_printer;
mod error;
mod writer;

use ast_printer::AstPrinter;
pub use error::*;
pub use writer::Writer;

pub struct PrettyPrinter {
    indent: String,
    color:  Color,
}

impl PrettyPrinter {
    pub fn new(indent: &'_ str, color: Color) -> Self {
        Self {
            indent: indent.to_string(),
            color,
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
        match self.color {
            Color::Always => {
                writer.use_color = true;
            },
            Color::Never => {
                writer.use_color = false;
            },
            _ => (),
        }

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

pub enum Color {
    Always,
    Auto,
    Never,
}

pub trait Pretty {
    fn print(&self, w: &mut Writer<'_>) -> Result;
}
