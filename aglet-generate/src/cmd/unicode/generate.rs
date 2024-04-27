use chrono::{TimeDelta, Utc};
use clap::Args;
use eyre::eyre;

use crate::cache::{Cache, StoredVersionTag};
use crate::cmd::unicode::{CommonArgs, SelectVersion};

#[derive(Args, Debug)]
pub struct GenerateArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(long = "crate", default_value = "aglet-unicode")]
    target_crate: String,
}

pub async fn run(args: GenerateArgs, cache: &mut Cache) -> eyre::Result<()> {
    let version = cache
        .metadata
        .stored_versions
        .iter()
        .find(|stored| match args.common.version {
            SelectVersion::Version(version) => stored.version == version,
            SelectVersion::Draft => stored.tags.contains(&StoredVersionTag::Draft),
            SelectVersion::Latest => stored.tags.contains(&StoredVersionTag::Latest),
        });

    let Some(version) = version else {
        if cache.metadata.stored_versions.is_empty() {
            return Err(eyre!("no versions of the unicode database have been downloaded; use `ag-gen unicode fetch.rs` to get the latest version"));
        }

        return Err(match args.common.version {
            SelectVersion::Version(version) => eyre!("version {0} has not been downloaded; use `ag-gen unicode fetch.rs {0}`", version),
            SelectVersion::Latest => eyre!("the latest version has not been downloaded; use `ag-gen unicode fetch.rs` to get it"),
            SelectVersion::Draft => eyre!("the draft version has not been downloaded; use `ag-gen unicode fetch.rs draft` to get it"),
        });
    };

    if version.tags.contains(&StoredVersionTag::Draft)
        && Utc::now() - version.fetched_at > TimeDelta::days(30)
    {
        return Err(eyre!("the stored draft version is more than 30 days old; use `ag-gen unicode fetch.rs draft` to get a fresh copy"));
    }

    if matches!(args.common.version, SelectVersion::Latest)
        && Utc::now() - version.fetched_at > TimeDelta::days(90)
    {
        return Err(eyre!("the stored latest version is more than 90 days old; use `ag-gen unicode fetch.rs` to get a fresh copy"));
    }

    Ok(())
}
