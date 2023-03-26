use std::fmt::{self, Write};

use aglet_text::Span;
use colored::control::ShouldColorize;
use colored::Color;

use crate::AstPrinter;
use crate::Pretty;
use crate::Result;
use crate::TokenPrinter;

/// Writer for pretty-printing parser structures
///
/// The writer keeps track of the indentation of nested structures, as well as
/// spans and metadata that should be reported along with the main output.
pub struct Writer<'a> {
    buf:                  &'a mut (dyn Write + 'a),
    spans:                &'a mut Vec<Option<Span>>,
    meta:                 &'a mut Vec<Option<String>>,
    on_newline:           bool,
    indent:               String,
    pub(crate) level:     u32,
    pub(crate) use_color: bool,
}

impl<'a> Writer<'a> {
    pub fn new<T>(
        buf: &'a mut T,
        spans: &'a mut Vec<Option<Span>>,
        meta: &'a mut Vec<Option<String>>,
    ) -> Self
    where
        T: Write + 'a,
    {
        Self {
            buf,
            spans,
            meta,
            on_newline: true,
            indent: "\t".to_string(),
            level: 0,
            use_color: ShouldColorize::from_env().should_colorize(),
        }
    }

    /// Set the indentation style
    ///
    /// The provided string will be repeated by the number of indents
    /// for any indented lines in the output
    pub fn with_indent(mut self, indent: &str) -> Self {
        self.indent = indent.to_string();
        self
    }

    /// Print the indentation for the current level
    pub fn print_indent(&mut self) -> fmt::Result {
        for _ in 0..self.level {
            self.buf.write_str(self.indent.as_str())?;
        }

        Ok(())
    }

    /// Create an printer for a nested abstract syntax tree structure.
    ///
    /// # Arguments
    ///
    /// * `name` - the display name of the AST node
    /// * `span` - the text span of the node's origin in the input
    /// * `color` - the color to print the node's name in
    pub fn print_ast<'b>(
        &'b mut self,
        name: &str,
        span: Option<Span>,
        color: Option<Color>,
    ) -> AstPrinter<'b, 'a> {
        AstPrinter::new(self, name, span, color)
    }

    /// Create a printer for a token stream.
    ///
    /// # Arguments
    ///
    /// * `name` - the display name of the token
    /// * `span` - the text span of the token's origin in the input
    /// * `stack` - the state stack information when the token was produced
    /// * `color` - the color to print the token's name in
    pub fn print_token<'b>(
        &'b mut self,
        name: &str,
        span: Option<Span>,
        stack: Option<String>,
        color: Option<Color>,
    ) -> TokenPrinter<'b, 'a> {
        TokenPrinter::new(self, name, span, stack, color)
    }

    /// Pretty-print a structure
    pub fn print(&mut self, item: &impl Pretty) -> Result {
        item.print(self)
    }

    /// Add a span to the output
    pub fn add_span(&mut self, span: Option<Span>) {
        self.spans.push(span);
    }

    /// Add a line of metadata to the output
    pub fn add_meta(&mut self, meta: Option<String>) {
        self.meta.push(meta);
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
