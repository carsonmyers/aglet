mod ast_printer;
mod error;
mod writer;

use ast_printer::AstPrinter;
use colored::Colorize;
pub use error::*;
pub use writer::Writer;

pub struct PrettyPrinter {
    settings: PrettyPrintSettings,
}

impl PrettyPrinter {
    pub fn new(settings: PrettyPrintSettings) -> Self {
        Self { settings }
    }

    pub fn print(&self, item: &impl Pretty) -> Result {
        let out = self.print_buf(item)?;
        println!("{}", out);
        Ok(())
    }

    fn print_buf(&self, item: &impl Pretty) -> std::result::Result<String, Error> {
        let mut main = String::new();

        let mut writer = Writer::new(&mut main).with_indent(&self.settings.indent);
        match self.settings.color_when {
            ColorWhen::Always => {
                writer.use_color = true;
            },
            ColorWhen::Never => {
                writer.use_color = false;
            },
            _ => (),
        }

        item.print(&mut writer)?;

        if !self.settings.include_spans {
            return Ok(main);
        }

        let span_lines = writer
            .spans
            .iter()
            .map(|maybe_span| {
                maybe_span.map(|span| (format!("{:?}", span.start), format!("{:?}", span.end)))
            })
            .collect::<Vec<_>>();

        let (max_start, max_end) = if self.settings.align {
            span_lines
                .iter()
                .fold((0, 0), |(acc_left, acc_right), line| {
                    let (left, right) = line
                        .as_ref()
                        .map(|(l, r)| (l.len(), r.len()))
                        .unwrap_or((acc_left, acc_right));
                    (usize::max(left, acc_left), usize::max(right, acc_right))
                })
        } else {
            (0, 0)
        };

        let use_color = writer.use_color;
        Ok(span_lines
            .into_iter()
            .zip(main.lines())
            .map(|(span, main)| {
                let (span_left, span_right) =
                    span.unwrap_or_else(|| (String::new(), String::new()));
                let mut span_text =
                    format!("{:<max_start$} - {:>max_end$}:", span_left, span_right);
                if use_color {
                    span_text = span_text.truecolor(150, 150, 150).to_string();
                }
                format!("{}\t{}", span_text, main)
            })
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

pub struct PrettyPrintSettings {
    align:         bool,
    include_spans: bool,
    color_when:    ColorWhen,
    indent:        String,
}

impl PrettyPrintSettings {
    pub fn align(mut self, value: bool) -> Self {
        self.align = value;
        self
    }

    pub fn include_spans(mut self, value: bool) -> Self {
        self.include_spans = value;
        self
    }

    pub fn color_when(mut self, value: ColorWhen) -> Self {
        self.color_when = value;
        self
    }

    pub fn indent(mut self, value: &str) -> Self {
        self.indent = value.to_string();
        self
    }
}

impl Default for PrettyPrintSettings {
    fn default() -> Self {
        Self {
            align:         true,
            include_spans: true,
            color_when:    ColorWhen::Auto,
            indent:        "  ".to_string(),
        }
    }
}

pub enum ColorWhen {
    Always,
    Auto,
    Never,
}

pub trait Pretty {
    fn print(&self, w: &mut Writer<'_>) -> Result;
}
