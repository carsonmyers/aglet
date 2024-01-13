mod ast_printer;
mod error;
mod span_printer;
mod token_printer;
mod writer;

use std::{iter, result};

use aglet_text::{SourceFile, Span};
use ast_printer::AstPrinter;
use colored::control::ShouldColorize;
use colored::{Color, Colorize};
pub use error::*;
pub use span_printer::SpanPrinter;
use token_printer::TokenPrinter;
pub use writer::Writer;

const EXTRA_COLOR: Color = Color::TrueColor {
    r: 150,
    g: 150,
    b: 150,
};

/// Pretty printer for parser data
///
/// The debug trait doesn't allow for pretty-printing outside of its struct,
/// tuple, etc. printers. To print an AST without a lot of extraneous closing
/// braces on their own lines, this separate pretty-printer provides the auto
/// indentation for custom printers.
///
/// Printers can also supply text spans for each printed line, which will be
/// displayed to the left of the main structure, as well as metadata which will
/// be displayed to the right.
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

    /// Print a pretty-printable structure
    ///
    /// The output string is kept internally until [`finish`](PrettyPrinter::finish) is
    /// called, because while a nested structure like an AST only requires one call to
    /// `print`, a list of structures like `tokens` requires many prints. Storing the
    /// output as internal state across `print` invocations allows the printer to maintain
    /// column widths until the entire dataset has been processed.
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

    /// Finish printing a dataset and return the formatted results
    pub fn finish(&self) -> result::Result<String, Error> {
        let use_color = match self.settings.color_when {
            ColorWhen::Always => true,
            ColorWhen::Auto => ShouldColorize::from_env().should_colorize(),
            ColorWhen::Never => false,
        };

        // each span is transformed into a tuple of two formatted positions in order to
        // align them in the output (`start` aligned to the left, `end` to the right,
        // with the dash separating them in the center)
        let span_lines = self
            .spans
            .iter()
            .map(|maybe_span| {
                maybe_span.as_ref().map(|span| {
                    self.settings
                        .source
                        .as_ref()
                        .and_then(|file| {
                            let filename = file.filename.clone();
                            let start = file.file_position_from_offset(&span.start);
                            let end = file.file_position_from_offset(&span.end);

                            start.and_then(|start| end.map(|end| (filename, start, end)))
                        })
                        .map(|(filename, start, end)| {
                            (format!("{} {:?}", filename, start), format!("{:?}", end))
                        })
                        .unwrap_or_else(|| (format!("{:?}", span.start), format!("{:?}", span.end)))
                })
            })
            // spans are optional, so chain them with a never-ending string of `None`
            // so that the main output isn't cut short if they're missing
            .collect::<Vec<_>>();

        // calculate the maximum width of the start and end spans to align the entire set
        // to the same width
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
            // if the align setting is turned off, using 0 as a width has the same effect as
            // not using alignment formatting at all
            (0, 0)
        };

        // colored strings interfere with alignment, so the clean length (without any terminal
        // color/style markers) of each line of the main column is added as context
        let main_lines = self
            .main
            .lines()
            .map(|line| (line, len_clean(line)))
            .collect::<Vec<_>>();
        let max_main = main_lines.iter().map(|(_, len)| *len).max().unwrap_or(0);

        // span and meta lines are optional, so chain them with a neverending stream of `None`
        // so they don't cut the output short if they're missing.
        let span_lines = span_lines.into_iter().chain(iter::repeat(None));
        let meta_lines = self.meta.iter().chain(iter::repeat(&None));

        Ok(main_lines
            .into_iter()
            .zip(span_lines)
            .zip(meta_lines)
            .map(|(((main, main_len), span), meta)| {
                let span_column = if self.settings.include_spans {
                    // format each span line into aligned columns. Terminal colors are added after
                    // formatting, so the `{:<max_start$}` formatters are fine here (plus each
                    // line is colored in the same way so they shouldn't interfere regardless)
                    span.map(|(span_left, span_right)| {
                        format!("{:<max_start$} - {:>max_end$}:\t", span_left, span_right)
                    })
                    // colorize the span column if applicable
                    .map(|span| {
                        if use_color {
                            span.color(EXTRA_COLOR).to_string()
                        } else {
                            span
                        }
                    })
                    // account for lines with no span information
                    .unwrap_or_else(|| {
                        if self.settings.align {
                            // if alignment is being used, output enough spaces to maintain
                            // the column width (the max start and end width, plus 3 for " - ")
                            format!("{}:\t", " ".repeat(max_start + max_end + 3))
                        } else {
                            String::new()
                        }
                    })
                } else {
                    String::new()
                };

                let main_column = if self.settings.include_meta {
                    // Using `{:<max_main$}` doesn't account for the invisible formatting
                    // characters, so the right number of spaces for alignment need to be
                    // manually output for the main column data
                    format!("{}{}", main, " ".repeat(max_main - main_len))
                } else {
                    main.to_string()
                };

                let meta_column = if self.settings.include_meta {
                    meta.as_ref()
                        .map(|meta| {
                            if use_color {
                                meta.color(EXTRA_COLOR).to_string()
                            } else {
                                meta.to_string()
                            }
                        })
                        .unwrap_or_else(|| String::new())
                } else {
                    String::new()
                };

                // final output of all three columns. Some may be empty
                format!("{}{}{}", span_column, main_column, meta_column)
            })
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

/// Clean length of a string potentially containing terminal color markers
fn len_clean(string: &str) -> usize {
    enum State {
        Count,
        Match,
        Filter,
    }

    let mut result = 0;
    let mut state = State::Count;

    // count each character not part of the terminal formatting syntax.
    // terminal formatting is done with the sequence `\x1b[`, a series of
    // color/style options expressed as bytes, and `m`.
    for c in string.chars() {
        match state {
            // count each character until the beginning of a format sequence `\x1b` is
            // encountered, and then switch to the match state to confirm the beginning
            // of a sequence by finding `[`
            State::Count => {
                if c == '\x1b' {
                    state = State::Match;
                    continue;
                }

                result += 1;
            },
            // the beginning of a format sequence `\x1b` has been found, but unless it
            // is immediately followed by `[` a format sequence has not been found; so
            // either match it here or allow `\x1b` to be counted. If a sequence has been
            // found, switch to the filter state to ignore all characters in the sequence.
            State::Match => {
                if c == '[' {
                    state = State::Filter;
                    continue;
                }

                // count `\x1b` and whatever was found after it
                result += 2;
            },
            // ignore all bytes in a format sequence until the terminating byte `m`
            // has been found
            State::Filter => {
                if c == 'm' {
                    state = State::Count;
                }
            },
        }
    }

    result
}

#[derive(Debug, Clone)]
pub struct PrettyPrintSettings {
    source:        Option<SourceFile>,
    align:         bool,
    include_spans: bool,
    include_meta:  bool,
    color_when:    ColorWhen,
    indent:        String,
}

impl PrettyPrintSettings {
    /// Add a source file to include line/column number information with spans
    pub fn source(mut self, source: SourceFile) -> Self {
        self.source = Some(source);
        self
    }

    /// Control whether the pretty printer should align its output columns
    /// with extra spaces to ensure they line up visually.
    pub fn align(mut self, value: bool) -> Self {
        self.align = value;
        self
    }

    /// Control whether spans are printed in a column to the left of the main output
    pub fn include_spans(mut self, value: bool) -> Self {
        self.include_spans = value;
        self
    }

    /// Control whether optional metadata is printed in a column to the right of the main output
    pub fn include_meta(mut self, value: bool) -> Self {
        self.include_meta = value;
        self
    }

    /// Control whether output is printed in color
    pub fn color_when(mut self, value: ColorWhen) -> Self {
        self.color_when = value;
        self
    }

    /// Control how nested structures are indented in the output.
    pub fn indent(mut self, value: &str) -> Self {
        self.indent = value.to_string();
        self
    }
}

impl Default for PrettyPrintSettings {
    fn default() -> Self {
        Self {
            source:        None,
            align:         true,
            include_spans: true,
            include_meta:  true,
            color_when:    ColorWhen::Auto,
            indent:        "  ".to_string(),
        }
    }
}

/// When to print output in color
#[derive(Debug, Clone, Copy)]
pub enum ColorWhen {
    /// Force output coloring
    Always,

    /// Use colored output when the terminal supports it, and/or delegate the decision to
    /// the `CLICOLOR_FORCE`, `NO_COLOR`, and `CLICOLOR` environment variables
    Auto,

    // Turn off output colorin
    Never,
}

/// Support pretty-printing
pub trait Pretty {
    /// Pretty-print a value to a [`Writer`](crate::Writer)
    fn print(&self, w: &mut Writer<'_>) -> Result;
}
