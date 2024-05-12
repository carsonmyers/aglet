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

impl UnicodeArgs {
    pub fn common_args(&self) -> &CommonArgs {
        match &self.command {
            Commands::Show(args) => args,
            Commands::Use(args) => args,
            Commands::List(args) => &args.common,
            Commands::Fetch(args) => &args.common,
            Commands::Generate(args) => &args.common,
        }
    }
}

#[derive(Args, Debug)]
pub struct CommonArgs {
    #[arg(value_parser = parse_select_version, default_value = "latest")]
    version: SelectVersion,

    #[arg(long, short = 'C', default_value = "5")]
    pub max_connections: usize,
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
