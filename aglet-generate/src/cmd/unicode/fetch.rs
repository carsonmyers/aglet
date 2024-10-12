use std::path::PathBuf;
use std::sync::Arc;

use clap::Args;
use console::style;
use eyre::eyre;
use tokio::fs;
use tracing::debug;

use crate::cache::{Cache, HashFiles, HashPhase, StoredVersionTag, REMOTE_LISTING_TTL};
use crate::cmd::unicode::CommonArgs;
use crate::ftp::{self, new_pool};
use crate::progress::{self, phase};
use crate::unicode::SelectVersion;

#[derive(Args, Debug)]
pub struct FetchArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(long)]
    pub dry_run: bool,
}

pub async fn run(args: FetchArgs, cache: &mut Cache) -> eyre::Result<()> {
    let local = cache.get_tmp_dir().await?;
    let res = run_inner(args, cache, local.clone()).await;

    if local.is_dir() {
        fs::remove_dir_all(local).await?;
    }

    res
}

async fn run_inner(args: FetchArgs, cache: &mut Cache, local: PathBuf) -> eyre::Result<()> {
    if let Some(SelectVersion::Hash(_)) = args.common.version {
        let text = "Cannot fetch remote UCD version by hash: use a version like `Latest` or `15` instead";
        eprintln!("{}", style(text).bright().red());
        return Ok(());
    }

    let select = args.common.version.unwrap_or(SelectVersion::Latest);

    let pool = Arc::new(new_pool(args.common.max_connections));
    let progress = progress::Manager::new();

    // resolve the selected version (like Latest or 15.1.0) into a numbered version which is
    // known to exist on the unicode.org FTP server
    let options = ftp::ListVersionOptions::new().with_pool(pool.clone());
    let list_versions = ftp::ListVersions::new(options);
    progress.phase(phase("Resolving version..."))?;
    let remote_listing = cache
        .metadata
        .remote_listing
        .get_or_replace(REMOTE_LISTING_TTL, list_versions.list_versions())
        .await?;

    debug!("got cached listing: {} items", remote_listing.len());

    let version = remote_listing
        .into_iter()
        .find(|&version| version.selected_by(&select))
        .ok_or_else(|| eyre!("no version found matching {}", select))?
        .version;

    debug!("got version {}", version);

    // get a listing of remote files to be downloaded for the selected version
    let remote = version.remote_dir();
    let options = ftp::ListOptions::new()
        .with_pool(pool.clone())
        .exclude_ext("zip")
        .exclude_ext("pdf");
    let list_files = ftp::ListFiles::new(remote, options);
    progress.phase(ftp::ListProgress::new_phase(&list_files))?;
    let files = list_files.list_files().await?;

    debug!("got listing: {} items", files.len());

    // if a dry-run was selected, finish without downloading any files
    if args.dry_run {
        progress.finish().await?;
        return Ok(());
    }

    // download the listed files to a local temp directory
    let options = ftp::DownloadOptions::new()
        .with_pool(pool)
        .to(local.clone());
    let download = ftp::Download::new(files, options);
    progress.phase(ftp::DownloadProgress::new_phase(&download))?;
    let files = download.download().await?;

    debug!("downloaded files: {} files", files.len());

    // ucd a hash of the downloaded files, so that it can be interned into the cache
    let hash = HashFiles::new(&local, files)?;
    progress.phase(HashPhase::phase(&hash))?;
    let hash = hash.hash_files().await?;

    // cache the downloaded files
    let tag = StoredVersionTag::try_from(select).ok();
    progress.phase(phase("Intern downloaded files..."))?;
    cache
        .intern_version(&local, version, hash.clone(), tag)
        .await?;

    progress.finish().await?;

    Ok(())
}
