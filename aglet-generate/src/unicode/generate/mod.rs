mod property_names;
mod property_values;

use chrono::{TimeDelta, Utc};
use clap::Args;
use eyre::eyre;

use crate::unicode::cache::{Cache, StoredVersionTag};
use crate::unicode::{CommonArgs, SelectVersion};

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
        if cache.metadata.stored_versions.len() == 0 {
            return Err(eyre!("no versions of the unicode database have been downloaded; use `ag-gen unicode fetch` to get the latest version"));
        }

        return Err(match args.common.version {
            SelectVersion::Version(version) => eyre!("version {0} has not been downloaded; use `ag-gen unicode fetch {0}`", version),
            SelectVersion::Latest => eyre!("the latest version has not been downloaded; use `ag-gen unicode fetch` to get it"),
            SelectVersion::Draft => eyre!("the draft version has not been downloaded; use `ag-gen unicode fetch draft` to get it"),
        });
    };

    if version.tags.contains(&StoredVersionTag::Draft)
        && Utc::now() - version.fetched_at > TimeDelta::days(30)
    {
        return Err(eyre!("the stored draft version is more than 30 days old; use `ag-gen unicode fetch draft` to get a fresh copy"));
    }

    if matches!(args.common.version, SelectVersion::Latest)
        && Utc::now() - version.fetched_at > TimeDelta::days(90)
    {
        return Err(eyre!("the stored latest version is more than 90 days old; use `ag-gen unicode fetch` to get a fresh copy"));
    }

    let ucd_path = args.common.data_dir.join("data").join(&version.hash);
    if !ucd_path.exists() {
        return Err(eyre!(
            "could not load the UCD data; the data stored in {} may be corrupt",
            args.common.data_dir.display()
        ));
    }

    println!("OK: {}", ucd_path.display());

    Ok(())
}
