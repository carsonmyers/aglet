mod commands;

use std::fs::File;
use std::io::{self, BufRead};
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use commands::regex::RegexArgs;
use eyre::Result;

#[derive(Parser, Debug)]
#[command(name = "aglet")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Input file. Uses stdin if omitted
    #[arg(short, long)]
    input: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Tools for inspecting and debugging regular expressions
    #[command(name = "regex")]
    Regex(RegexArgs),
}

type CliInput = Box<dyn Iterator<Item = Result<String>>>;

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Cli::parse();

    let input = Input::new(args.input)?;
    match args.command {
        Commands::Regex(args) => commands::regex::run(input, args),
    }?;

    Ok(())
}

pub struct Input {
    input:    CliInput,
    filename: String,
    is_stdin: bool,
}

impl Input {
    pub fn new(input_path: Option<PathBuf>) -> Result<Self> {
        let input = if let Some(input_path) = input_path {
            let file = File::open(input_path.clone())?;
            let iter = io::BufReader::new(file)
                .lines()
                .map(|res| res.map_err(|err| err.into()));

            Self {
                input: Box::new(iter) as CliInput,
                filename: input_path.to_string_lossy().to_string(),
                is_stdin: false,
            }
        } else {
            let iter = std::io::stdin()
                .lines()
                .map(|res| res.map_err(|err| err.into()));

            Self {
                input: Box::new(iter) as CliInput,
                filename: "<stdin>".to_string(),
                is_stdin: true
            }
        };

        Ok(input)
    }

    #[inline]
    pub fn is_stdin(&self) -> bool {
        self.is_stdin
    }

    #[inline]
    pub fn filename(&self) -> &str {
        &self.filename
    }
}

impl Iterator for Input {
    type Item = Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next()
    }
}
