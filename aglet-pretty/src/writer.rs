use std::fmt::{self, Write};

use aglet_text::Span;
use colored::control::ShouldColorize;
use colored::Color;

use crate::AstPrinter;
use crate::Pretty;
use crate::Result;

pub struct Writer<'a> {
    buf:                  &'a mut (dyn Write + 'a),
    on_newline:           bool,
    indent:               String,
    pub(crate) spans:     Vec<Option<Span>>,
    pub(crate) level:     u32,
    pub(crate) use_color: bool,
}

impl<'a> Writer<'a> {
    pub fn new<T>(buf: &'a mut T) -> Self
    where
        T: Write + 'a,
    {
        Self {
            buf,
            on_newline: true,
            indent: "\t".to_string(),
            spans: Vec::new(),
            level: 0,
            use_color: ShouldColorize::from_env().should_colorize(),
        }
    }

    pub fn with_indent(mut self, indent: &str) -> Self {
        self.indent = indent.to_string();
        self
    }

    pub fn print_indent(&mut self) -> fmt::Result {
        for _ in 0..self.level {
            self.buf.write_str(self.indent.as_str())?;
        }

        Ok(())
    }

    pub fn print_ast<'b>(
        &'b mut self,
        name: &str,
        span: Option<Span>,
        color: Option<Color>,
    ) -> AstPrinter<'b, 'a> {
        AstPrinter::new(self, name, span, color)
    }

    pub fn print(&mut self, item: &impl Pretty) -> Result {
        item.print(self)
    }
}

impl Write for Writer<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.on_newline {
                self.print_indent()?;
            }

            self.on_newline = line.ends_with('\n');
            self.buf.write_str(line)?;
        }

        Ok(())
    }
}
