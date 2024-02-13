mod parse;
mod unicode;

use clap::{Parser, Subcommand};
use eyre::Result;

#[derive(Parser, Debug)]
#[command(name = "ag-gen")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Generate unicode tables from the Unicode Character Database
    #[command(name = "unicode")]
    Unicode(unicode::UnicodeArgs),
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Cli::parse();
    match args.command {
        Commands::Unicode(args) => unicode::run(args).await,
    }?;

    Ok(())
}
