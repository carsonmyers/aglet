use std::sync::Arc;

use chrono::Duration;

use crate::cache::Cache;
use crate::progress::{self, phase};
use crate::unicode::remote::ListVersions;
use crate::unicode::{remote, CommonArgs};

pub async fn run(args: CommonArgs, cache: &mut Cache) -> eyre::Result<()> {
    let progress = progress::Manager::new();
    progress.phase(phase("Listing versions from unicode.org..."))?;

    let pool = Arc::new(remote::new_pool(args.max_connections));
    let list_versions = ListVersions::new(pool);
    let remote_listing = cache
        .metadata
        .remote_listing
        .get_or_replace(Duration::days(15), list_versions.list_versions())
        .await?;

    Ok(())
}
