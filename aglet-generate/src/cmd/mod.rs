mod unicode;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::cache::Cache;

#[derive(Parser, Debug)]
#[command(name = "ag-gen")]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    #[arg(short, long = "data", default_value = ".ag-gen-cache")]
    pub data_dir: PathBuf,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Generate unicode tables from the Unicode Character Database
    #[command(name = "unicode")]
    Unicode(unicode::UnicodeArgs),
}

pub async fn run(args: Cli) -> eyre::Result<()> {
    let mut cache = Cache::load_or_init(&args.data_dir).await?;

    match args.command {
        Commands::Unicode(args) => unicode::run(args, &mut cache).await,
    }?;

    cache.save().await?;

    Ok(())
}
