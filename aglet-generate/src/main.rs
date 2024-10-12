mod cache;
mod cmd;
mod ftp;
mod parse;
mod progress;
mod task;
mod unicode;

use clap::Parser;
use eyre::Result;
use tracing_subscriber::filter::EnvFilter;

const LOG_ENV_VAR: &str = "AGLET_LOG";

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_env(LOG_ENV_VAR))
        .init();

    let args = cmd::Cli::parse();
    cmd::run(args).await?;

    Ok(())
}
