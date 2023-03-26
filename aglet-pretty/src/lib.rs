mod ast_printer;
mod error;
mod token_printer;
mod writer;

use std::iter;
use std::result;

use aglet_text::Span;
use ast_printer::AstPrinter;
use colored::control::ShouldColorize;
use colored::{Color, Colorize};
pub use error::*;
use token_printer::TokenPrinter;
pub use writer::Writer;

const EXTRA_COLOR: Color = Color::TrueColor {
    r: 150,
    g: 150,
    b: 150,
};

pub struct PrettyPrinter {
    settings: PrettyPrintSettings,
    main:     String,
    spans:    Vec<Option<Span>>,
    meta:     Vec<Option<String>>,
}

impl PrettyPrinter {
    pub fn new(settings: PrettyPrintSettings) -> Self {
        let main = String::new();

        Self {
            settings,
            main,
            spans: Vec::new(),
            meta: Vec::new(),
        }
    }

    pub fn print(&mut self, item: &impl Pretty) -> result::Result<&Self, Error> {
        let mut writer = Writer::new(&mut self.main, &mut self.spans, &mut self.meta)
            .with_indent(&self.settings.indent);
        match self.settings.color_when {
            ColorWhen::Always => {
                writer.use_color = true;
            },
            ColorWhen::Never => {
                writer.use_color = false;
            },
            _ => (),
        };

        item.print(&mut writer)?;

        Ok(self)
    }

    pub fn finish(&self) -> result::Result<String, Error> {
        let use_color = match self.settings.color_when {
            ColorWhen::Always => true,
            ColorWhen::Auto => ShouldColorize::from_env().should_colorize(),
            ColorWhen::Never => false,
        };

        let span_lines = self
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

        let main_lines = self
            .main
            .lines()
            .map(|line| (line, len_clean(line)))
            .collect::<Vec<_>>();
        let max_main = main_lines.iter().map(|(_, len)| *len).max().unwrap_or(0);

        let meta_lines = self.meta.iter().chain(iter::repeat(&None));

        Ok(span_lines
            .into_iter()
            .zip(main_lines)
            .zip(meta_lines)
            .map(|((span, (main, main_len)), meta)| {
                let span_column = if self.settings.include_spans {
                    span.map(|(span_left, span_right)| {
                        format!("{:<max_start$} - {:>max_end$}:\t", span_left, span_right)
                    })
                    .map(|span| {
                        if use_color {
                            span.color(EXTRA_COLOR).to_string()
                        } else {
                            span
                        }
                    })
                    .unwrap_or_else(|| {
                        if self.settings.align {
                            format!("{}:\t", " ".repeat(max_start + max_end + 3))
                        } else {
                            String::new()
                        }
                    })
                } else {
                    String::new()
                };

                let main_column = if self.settings.include_meta {
                    format!("{}{}", main, " ".repeat(max_main - main_len))
                } else {
                    main.to_string()
                };

                let meta_column = if self.settings.include_meta {
                    meta.as_ref()
                        .map(|meta| format!("{}", meta))
                        .map(|meta| {
                            if use_color {
                                meta.color(EXTRA_COLOR).to_string()
                            } else {
                                meta
                            }
                        })
                        .unwrap_or_else(|| String::new())
                } else {
                    String::new()
                };

                format!("{}{}{}", span_column, main_column, meta_column)
            })
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

fn len_clean(string: &str) -> usize {
    enum State {
        Copy,
        Match,
        Filter,
    }

    let mut result = 0;
    let mut state = State::Copy;

    for c in string.chars() {
        match state {
            State::Copy => {
                if c == '\x1b' {
                    state = State::Match;
                    continue;
                }

                result += 1;
            },
            State::Match => {
                if c == '[' {
                    state = State::Filter;
                    continue;
                }

                result += 2;
            },
            State::Filter => {
                if c == 'm' {
                    state = State::Copy;
                }
            },
        }
    }

    result
}

#[derive(Debug, Clone)]
pub struct PrettyPrintSettings {
    align:         bool,
    include_spans: bool,
    include_meta:  bool,
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

    pub fn include_meta(mut self, value: bool) -> Self {
        self.include_meta = value;
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
            include_meta:  true,
            color_when:    ColorWhen::Auto,
            indent:        "  ".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ColorWhen {
    Always,
    Auto,
    Never,
}

pub trait Pretty {
    fn print(&self, w: &mut Writer<'_>) -> Result;
}
