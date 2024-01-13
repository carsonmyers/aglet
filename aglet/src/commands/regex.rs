use std::path::PathBuf;

use aglet_pretty::{ColorWhen, PrettyPrintSettings, PrettyPrinter};
use aglet_regex::{Parser, Tokenizer};
use aglet_text::SourceFile;
use clap::Args;
use colored::control::ShouldColorize;
use colored::Colorize;
use eyre::{eyre, Result};
use itertools::Itertools;

use crate::{CliInput, Input};

#[derive(Debug, Args)]
pub struct RegexArgs {
    /// Print the token stream
    #[arg(short, long)]
    tokens: bool,

    /// Print the resulting abstract syntax tree
    #[arg(short, long)]
    ast: bool,

    /// Display the span of each token or AST node
    #[arg(short, long)]
    spans: bool,

    /// Include metadata column for each token or AST node
    #[arg(short, long)]
    meta: bool,

    /// Remove alignment spacing in the spans column
    #[arg(long)]
    no_align: bool,

    /// Disable colour output
    #[arg(long)]
    no_color: bool,

    /// Force color output
    #[arg(long)]
    force_color: bool,

    /// Use test mode - generate test output or test input data.
    ///
    /// Implies --no_color, --no_align, --ast, and --tokens
    #[arg(short = 'T', long)]
    test: bool,

    /// Output file
    #[arg(short, long)]
    out: Option<PathBuf>,
}

pub fn run(input: Input, args: RegexArgs) -> Result<()> {
    let is_stdin = input.is_stdin();
    let iter = SectionIterator::new(Box::new(input), !is_stdin);

    let color_supported = ShouldColorize::from_env().should_colorize();
    let color_when = match (color_supported, args.no_color, args.force_color) {
        (true, false, false) => ColorWhen::Auto,
        (_, _, true) => ColorWhen::Always,
        _ => ColorWhen::Never,
    };
    let printer_settings = PrettyPrintSettings::default()
        .indent("\u{254E}   ".bright_black().to_string().as_ref())
        .color_when(color_when)
        .align(!args.no_align)
        .include_spans(args.spans);

    if is_stdin && (!args.tokens && !args.ast) {
        println!("Warning: no output options used (try --ast or --tokens)");
    }

    for section in iter {
        let section = section?;
        let printer_settings = printer_settings.clone().source(section.source.clone());
        run_section(&section, &printer_settings, is_stdin, &args)?;
    }

    Ok(())
}

fn run_section(
    section: &InputSection,
    printer_settings: &PrettyPrintSettings,
    is_stdin: bool,
    args: &RegexArgs,
) -> Result<()> {
    if !is_stdin {
        println!("{}\n", section.source.src);
    }

    if args.tokens {
        print_tokens(section, printer_settings.clone(), args)?;
    }

    if args.ast {
        print_ast(section, printer_settings.clone())?;
    }

    Ok(())
}

fn print_tokens(
    section: &InputSection,
    printer_settings: PrettyPrintSettings,
    args: &RegexArgs,
) -> Result<()> {
    let mut printer = PrettyPrinter::new(printer_settings);

    let tokenizer = Tokenizer::new(section.source.src.as_ref());
    if args.meta {
        for token_stack in tokenizer.into_token_stack_iter() {
            match token_stack {
                Ok(stack) => {
                    printer.print(&stack)?;
                },
                Err(stack_error) => {
                    printer.print(&stack_error)?;
                },
            }
        }
    } else {
        for token in tokenizer {
            match token {
                Ok(tok) => {
                    printer.print(&tok)?;
                },
                Err(e) => {
                    printer.print(&e)?;
                },
            }
        }
    }

    println!("TOKENS:\n{}\n", printer.finish()?);

    Ok(())
}

fn print_ast(section: &InputSection, printer_settings: PrettyPrintSettings) -> Result<()> {
    let mut printer = PrettyPrinter::new(printer_settings);

    let tokens = Tokenizer::new(section.source.src.as_ref()).collect_vec();
    let parse_result = Parser::new(tokens.into_iter()).parse();
    println!("AST:\n{}\n", printer.print(&parse_result.ast)?.finish()?);
    if parse_result.errors.len() > 0 {
        println!(
            "PARSE ERR:\n{}\n",
            parse_result.errors.into_iter().join("\n")
        );
    }

    Ok(())
}

struct InputSection {
    source: SourceFile,
    tests:  Vec<TestKind>,
}

enum TestKind {
    TokenTest(String),
    AstTest(String),
}

struct SectionIterator {
    input:        CliInput,
    last_section: Option<InputSection>,
    allow_tests:  bool,
}

impl SectionIterator {
    fn new(input: CliInput, allow_tests: bool) -> Self {
        Self {
            input: condense_input(input),
            last_section: None,
            allow_tests,
        }
    }

    fn next_section(&mut self) -> Result<Option<InputSection>> {
        loop {
            let data = self.next_block()?;

            if data.len() == 0 {
                return Ok(self.last_section.take());
            }

            let header = data[0].to_lowercase();
            if self.allow_tests && header.starts_with("token") {
                self.add_test(TestKind::TokenTest(data[1..].join("\n")))?;
            } else if self.allow_tests && header.starts_with("ast") {
                self.add_test(TestKind::AstTest(data[1..].join("\n")))?;
            } else {
                let section = self.start_section(data.join("\n"));
                if section.is_some() {
                    return Ok(section);
                }
                if !self.allow_tests {
                    return Ok(self.last_section.take());
                }
            }
        }
    }

    fn next_block(&mut self) -> Result<Vec<String>> {
        let data = self
            .input
            .by_ref()
            .take_while(|line| match line {
                Ok(line) => line.len() > 0,
                _ => true,
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(data)
    }

    fn add_test(&mut self, test: TestKind) -> Result<()> {
        let Some(section) = &mut self.last_section else {
            return Err(eyre!("no section for test"));
        };

        section.tests.push(test);
        Ok(())
    }

    fn start_section(&mut self, input: String) -> Option<InputSection> {
        self.last_section.replace(InputSection {
            source: SourceFile::new_from_source("".to_string(), "<stdin>".to_string(), input),
            tests:  Vec::new(),
        })
    }
}

impl Iterator for SectionIterator {
    type Item = Result<InputSection>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_section() {
            Ok(Some(section)) => Some(Ok(section)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

fn condense_input(input: CliInput) -> CliInput {
    let input = input
        .scan(None, |last_len: &mut Option<usize>, item| {
            match (item, *last_len) {
                (Ok(line), Some(len)) => {
                    let should_filter = line.len() == 0 && len == 0;
                    *last_len = Some(line.len());
                    Some((should_filter, Ok(line)))
                },
                (Ok(line), None) => {
                    *last_len = Some(line.len());
                    Some((false, Ok(line)))
                },
                (res, _) => {
                    *last_len = None;
                    Some((false, res))
                },
            }
        })
        .filter_map(|(should_filter, item)| if should_filter { None } else { Some(item) });

    Box::new(input)
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_condense_input() {
        let input = vec![
            Ok("line 1".to_string()),
            Ok("line 2".to_string()),
            Ok("".to_string()),
            Ok("".to_string()),
            Ok("".to_string()),
            Ok("line 3".to_string()),
            Ok("line 4".to_string()),
            Err(eyre!("err 1")),
            Err(eyre!("err 2")),
            Ok("".to_string()),
            Err(eyre!("err 3")),
            Ok("".to_string()),
            Ok("".to_string()),
        ]
        .into_iter();

        let expected = vec![
            Ok("line 1".to_string()),
            Ok("line 2".to_string()),
            Ok("".to_string()),
            Ok("line 3".to_string()),
            Ok("line 4".to_string()),
            Err(eyre!("err 1")),
            Err(eyre!("err 2")),
            Ok("".to_string()),
            Err(eyre!("err 3")),
            Ok("".to_string()),
        ];

        let actual = condense_input(Box::new(input)).collect_vec();
        assert_eq!(expected.len(), actual.len());

        for (expected, actual) in expected.into_iter().zip(actual) {
            if let (Ok(expected), Ok(actual)) = (&expected, &actual) {
                assert_eq!(expected, actual);
            } else if let (Err(expected), Err(actual)) = (&expected, &actual) {
                assert_eq!(expected.to_string(), actual.to_string());
            } else {
                panic!("can't compare {:?} and {:?}", expected, actual);
            }
        }
    }

    #[test]
    fn test_input_iter() {
        let input = vec![
            "abc",
            "def",
            "",
            "",
            "TOKENS:",
            "token test",
            "data",
            "",
            "ghi",
            "jkl",
            "",
            "TOKENS ===",
            "",
            "AST ===",
            "ast data",
        ]
        .into_iter()
        .map(|line| Ok(line.to_string()) as Result<String>);

        let mut iter = SectionIterator::new(Box::new(input), true);

        let section = iter.next().unwrap().unwrap();
        assert_eq!(section.source.src, "abc\ndef".to_string());
        assert_eq!(section.tests.len(), 1);
        assert!(matches!(section.tests[0], TestKind::TokenTest(_)));
        let TestKind::TokenTest(data) = &section.tests[0] else {
            panic!("not a token test");
        };
        assert_eq!(data, "token test\ndata");

        let section = iter.next().unwrap().unwrap();
        assert_eq!(section.source.src, "ghi\njkl");
        assert_eq!(section.tests.len(), 2);
        assert!(matches!(section.tests[0], TestKind::TokenTest(_)));
        let TestKind::TokenTest(data) = &section.tests[0] else {
            panic!("not a token test");
        };
        assert_eq!(data, "");
        assert!(matches!(section.tests[1], TestKind::AstTest(_)));
        let TestKind::AstTest(data) = &section.tests[1] else {
            panic!("not an ast test");
        };
        assert_eq!(data, "ast data");

        let section = iter.next();
        assert!(section.is_none());
    }

    #[test]
    fn test_input_iter_no_tests() {
        let input = vec!["abc", "def", ""]
            .into_iter()
            .map(|line| Ok(line.to_string()) as Result<String>);

        let mut iter = SectionIterator::new(Box::new(input), false);
        let section = iter.next();
        assert!(section.is_some());

        let input = vec![
            "abc",
            "def",
            "",
            "TOKENS:",
            "misinterpreted",
            "test data",
            "",
        ]
        .into_iter()
        .map(|line| Ok(line.to_string()));

        let mut iter = SectionIterator::new(Box::new(input), false);

        let section = iter.next();
        assert!(section.is_some());
        let Some(Ok(section)) = section else {
            panic!("error reading section");
        };
        assert_eq!(section.source.src, "abc\ndef".to_string());
        assert_eq!(section.tests.len(), 0);

        let section = iter.next();
        assert!(section.is_some());
        let Some(Ok(section)) = section else {
            panic!("error reading section");
        };
        assert_eq!(section.source.src, "TOKENS:\nmisinterpreted\ntest data");
        assert_eq!(section.tests.len(), 0);

        let section = iter.next();
        assert!(section.is_none());
    }
}
