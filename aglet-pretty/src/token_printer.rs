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

pub struct TokenPrinter<'a, 'b: 'a> {
    writer: &'a mut Writer<'b>,
    result: Result,
}

impl<'a, 'b: 'a> TokenPrinter<'a, 'b> {
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

    pub fn finish(&mut self) -> Result {
        self.result.and_then(|_| {
            write!(self.writer, "]\n")?;
            Ok(())
        })
    }
}
