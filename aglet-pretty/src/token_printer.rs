use std::fmt::{self, Write};

use aglet_text::Span;
use colored::{Color, Colorize};

use crate::Result;
use crate::Writer;

const DEFAULT_COLOR: Color = Color::White;
const PARAMETER_COLOR: Color = Color::TrueColor {
    r: 150,
    g: 150,
    b: 150,
};

///Printer for a tokenizer token
///
/// Tokens can optionally be printed with properties. They are printed all on one
/// line, with no nesting components.
///
/// # Example
///
/// A literal token might be printed like this:
///
/// ```tok
/// [Literal value='a']
/// ```
pub struct TokenPrinter<'a, 'b: 'a> {
    writer: &'a mut Writer<'b>,
    result: Result,
}

impl<'a, 'b: 'a> TokenPrinter<'a, 'b> {
    /// Begin printing the token = its name will be printed, and its span will be added
    /// to the output at this stage. If the caller is printing the tokenizer's state stack
    /// along with the token, it can be passed here as well and it will be printed
    /// in the metadata column.
    pub fn new(
        writer: &'a mut Writer<'b>,
        name: &str,
        span: Option<Span>,
        stack: Option<String>,
        color: Option<Color>,
    ) -> Self {
        let result = Ok(()).and_then(|_| {
            if writer.use_color {
                let color = color.unwrap_or(DEFAULT_COLOR);
                write!(writer, "[{}", name.color(color))?;
            } else {
                write!(writer, "[{}", name)?;
            }

            Ok(())
        });

        writer.add_span(span);
        writer.add_meta(stack);

        TokenPrinter { writer, result }
    }

    /// Add a property to the token.
    ///
    /// Properties can be optionally prefixed with a name
    pub fn property(
        &mut self,
        name: Option<&str>,
        value: &impl fmt::Debug,
        color: Option<Color>,
    ) -> &mut Self {
        self.result = self.result.and_then(|_| {
            let name_text = if let Some(name) = name {
                format!("{}=", name)
            } else {
                String::new()
            };

            if self.writer.use_color {
                let color = color.unwrap_or(DEFAULT_COLOR);
                write!(
                    self.writer,
                    " {}{}",
                    name_text.color(PARAMETER_COLOR),
                    format!("{:?}", value).color(color)
                )?;
            } else {
                write!(self.writer, " {}{:?}", name_text, value)?;
            }

            Ok(())
        });

        self
    }

    /// Maybe add a property to the token.
    ///
    /// The property will be added if `value` is not `None`. It can be optionally
    /// prefixed with a property name.
    pub fn maybe_property(
        &mut self,
        name: Option<&str>,
        value: Option<&impl fmt::Debug>,
        color: Option<Color>,
    ) -> &mut Self {
        if let Some(value) = value {
            self.property(name, value, color)
        } else {
            self
        }
    }

    /// Finish printing the token
    ///
    /// Writes the closing `]` of the token and and returns the result of printing all of
    /// its parts. If errors occurred, only the first will be returned (and no printing will
    /// have taken place since it occurred).
    pub fn finish(&mut self) -> Result {
        self.result.and_then(|_| {
            write!(self.writer, "]\n")?;
            Ok(())
        })
    }
}
