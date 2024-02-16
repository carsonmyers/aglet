mod remote;
pub mod version;

use std::path::PathBuf;

use clap::{Args, Subcommand};
use eyre::Result;

pub use self::version::UnicodeVersion;

#[derive(Args, Debug)]
pub struct UnicodeArgs {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Args, Debug)]
pub struct CommonArgs {
    #[arg(long = "crate", default_value = "aglet-unicode")]
    crate_name: String,

    #[arg(short, long = "data", default_value = "./")]
    data_dir: PathBuf,

    #[arg(short, long, default_value = "latest")]
    version: UnicodeVersion,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(name = "show")]
    Show(CommonArgs),
}

pub async fn run(args: UnicodeArgs) -> Result<()> {
    println!("unicode cmd - {:?}", args);

    match args.command {
        Commands::Show(args) => show(args),
    }
    .await?;

    Ok(())
}

async fn show(args: CommonArgs) -> Result<()> {
    let versions = remote::list_versions().await?;
    dbg!(versions);

    Ok(())
}
