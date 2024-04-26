mod cache;
mod fetch;
mod generate;
mod list;
pub mod local;
mod remote;
pub mod version;

use std::fmt;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::str::FromStr;

use clap::{Args, Subcommand};

pub use self::version::UnicodeVersion;

#[derive(Args, Debug)]
pub struct UnicodeArgs {
    #[command(subcommand)]
    command: Commands,
}

impl UnicodeArgs {
    pub fn common_args(&self) -> &CommonArgs {
        match &self.command {
            Commands::Show(args) => args,
            Commands::List(args) => &args.common,
            Commands::Fetch(args) => &args.common,
            Commands::Generate(args) => &args.common,
        }
    }
}

#[derive(Args, Debug)]
pub struct CommonArgs {
    #[arg(short, long = "data", default_value = ".ucd-data")]
    data_dir: PathBuf,

    #[arg(value_parser = parse_select_version, default_value = "latest")]
    version: SelectVersion,
}

#[derive(Debug, Copy, Clone)]
pub enum SelectVersion {
    Latest,
    Draft,
    Version(UnicodeVersion),
}

impl SelectVersion {
    pub fn is_latest(&self) -> bool {
        matches!(self, &Self::Latest)
    }

    pub fn is_draft(&self) -> bool {
        matches!(self, &Self::Draft)
    }
}

impl fmt::Display for SelectVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Latest => write!(f, "latest"),
            Self::Draft => write!(f, "draft"),
            Self::Version(v) => write!(f, "{}", v),
        }
    }
}

impl FromStr for SelectVersion {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "latest" => Ok(SelectVersion::Latest),
            "draft" => Ok(SelectVersion::Draft),
            value => Ok(SelectVersion::Version(UnicodeVersion::from_str(value)?)),
        }
    }
}

fn parse_select_version(arg: &str) -> eyre::Result<SelectVersion> {
    SelectVersion::from_str(arg)
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(name = "show")]
    Show(CommonArgs),
    #[command(name = "list")]
    List(list::ListArgs),
    #[command(name = "fetch")]
    Fetch(fetch::FetchArgs),
    #[command(name = "generate")]
    Generate(generate::GenerateArgs),
}

pub async fn run(args: UnicodeArgs) -> eyre::Result<()> {
    let mut cache = cache::Cache::load_or_init(&args.common_args().data_dir).await?;

    match args.command {
        Commands::Show(args) => show(args, &mut cache).await?,
        Commands::List(args) => list::run(args, &mut cache).await?,
        Commands::Fetch(args) => fetch::run(args, &mut cache).await?,
        Commands::Generate(args) => generate::run(args, &mut cache).await?,
    }

    cache.save().await?;
    Ok(())
}

async fn show(_args: CommonArgs, cache: &mut cache::Cache) -> eyre::Result<()> {
    // let mut ftp = remote::ftp::login().await?;
    //
    // let versions = remote::list_versions(&mut ftp).await?;
    // dbg!(&versions);
    //
    // let latest = remote::get_latest_version(&mut ftp).await?;
    // let draft = remote::get_draft_version(&versions, &latest)?;
    //
    // dbg!(latest);
    // dbg!(draft);

    dbg!(&cache);

    Ok(())
}
