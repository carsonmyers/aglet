mod cache;
mod fetch;
mod list;
pub mod local;
mod remote;
pub mod version;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::path::PathBuf;

use clap::{Args, Subcommand};
use eyre::Result;

use self::cache::{StoredVersion, StoredVersionTag};
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
        }
    }
}

#[derive(Args, Debug)]
pub struct CommonArgs {
    #[arg(long = "crate", default_value = "aglet-unicode")]
    crate_name: String,

    #[arg(short, long = "data", default_value = ".ucd-data")]
    data_dir: PathBuf,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(name = "show")]
    Show(CommonArgs),
    #[command(name = "list")]
    List(list::ListArgs),
    #[command(name = "fetch")]
    Fetch(fetch::FetchArgs),
}

pub async fn run(args: UnicodeArgs) -> Result<()> {
    let mut cache = cache::Cache::load_or_init(&args.common_args().data_dir).await?;

    match args.command {
        Commands::Show(args) => show(args, &mut cache).await?,
        Commands::List(args) => list::run(args, &mut cache).await?,
        Commands::Fetch(args) => fetch::run(args, &mut cache).await?,
    }

    cache.save().await?;
    Ok(())
}

async fn show(_args: CommonArgs, cache: &mut cache::Cache) -> Result<()> {
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
