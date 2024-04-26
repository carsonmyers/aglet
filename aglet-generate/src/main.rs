mod parse;
mod progress;
mod task;
mod unicode;

use clap::{Parser, Subcommand};
use eyre::Result;
use tracing::warn;
use tracing_subscriber::filter::EnvFilter;

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
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_env("AGLET_LEVEL"))
        .init();

    color_eyre::install()?;

    let args = Cli::parse();
    match args.command {
        Commands::Unicode(args) => unicode::run(args).await,
    }?;

    Ok(())
}
