mod fetch;
mod generate;
mod list;
mod show;
mod use_version;

use std::str::FromStr;

use clap::{Args, Subcommand};

use crate::cache::Cache;
use crate::unicode::SelectVersion;

#[derive(Args, Debug)]
pub struct UnicodeArgs {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Args, Debug)]
pub struct CommonArgs {
    /// Select version by tag (e.g. Latest, Draft), version name (e.g. 15.1), or hash
    #[arg(value_parser = parse_select_version)]
    pub version: Option<SelectVersion>,

    /// Max number of FTP connections to use for fetching remote data
    #[arg(long, short = 'C', default_value = "5")]
    pub max_connections: usize,

    #[arg(short, long)]
    pub quiet: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(name = "show")]
    Show(CommonArgs),
    #[command(name = "use")]
    Use(CommonArgs),
    #[command(name = "list")]
    List(list::ListArgs),
    #[command(name = "fetch")]
    Fetch(fetch::FetchArgs),
    #[command(name = "generate")]
    Generate(generate::GenerateArgs),
}

pub async fn run(args: UnicodeArgs, cache: &mut Cache) -> eyre::Result<()> {
    match args.command {
        Commands::Show(args) => show::run(args, cache).await?,
        Commands::Use(args) => use_version::run(args, cache).await?,
        Commands::List(args) => list::run(args, cache).await?,
        Commands::Fetch(args) => fetch::run(args, cache).await?,
        Commands::Generate(args) => generate::run(args, cache).await?,
    }

    Ok(())
}

fn parse_select_version(arg: &str) -> eyre::Result<SelectVersion> {
    SelectVersion::from_str(arg)
}
