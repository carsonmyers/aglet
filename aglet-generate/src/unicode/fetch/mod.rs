use std::path::PathBuf;

use tokio::fs;

use crate::progress::{self, phase};
use crate::unicode::cache::{HashFiles, HashPhase, StoredVersionTag};
use crate::unicode::remote::{
    DownloadOptions, DownloadPhase, EnumeratePhase, ResolveSelectedVersion, VersionPhase,
};
use crate::unicode::{cache, CommonArgs};

pub async fn run(args: CommonArgs, cache: &mut cache::Cache) -> eyre::Result<()> {
    let local = cache.get_tmp_dir().await?;
    let res = run_inner(args, cache, local.clone()).await;

    if local.is_dir() {
        fs::remove_dir_all(local).await?;
    }

    res
}

async fn run_inner(args: CommonArgs, cache: &mut cache::Cache, local: PathBuf) -> eyre::Result<()> {
    let progress = progress::Manager::new();

    let resolve_version = ResolveSelectedVersion::new(args.version, 2);
    progress.phase(VersionPhase::phase(&resolve_version))?;
    let version = resolve_version.resolve_selected_version().await?;

    let remote = version.remote_dir();
    let options = DownloadOptions::new(remote, local.clone())
        .with_max_connections(5)
        .exclude_ext("zip")
        .exclude_ext("pdf");

    let download = options.build();
    progress.phase(EnumeratePhase::phase(&download))?;
    let download = download.enumerate_remote_files().await?;

    progress.phase(DownloadPhase::phase(&download))?;
    let files = download.download_remote_files().await?;

    let hash = HashFiles::new(&local, files)?;
    progress.phase(HashPhase::phase(&hash))?;
    let hash = hash.hash_files().await?;

    let tag = StoredVersionTag::try_from(args.version).ok();
    progress.phase(phase("Intern downloaded files..."))?;
    cache
        .intern_version(&local, version, hash.clone(), tag)
        .await?;

    progress.finish()?;

    Ok(())
}
