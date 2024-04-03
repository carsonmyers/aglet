use std::cmp::Ordering;
use std::collections::HashMap;

use clap::Args;

use crate::unicode::cache::{StoredVersion, StoredVersionTag};
use crate::unicode::{cache, remote, CommonArgs, UnicodeVersion};

#[derive(Args, Debug)]
pub struct ListArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(short, long)]
    remote: bool,

    #[arg(short, long)]
    all: bool,
}

pub async fn run(args: ListArgs, cache: &mut cache::Cache) -> eyre::Result<()> {
    if args.remote {
        return list_with_remote(args, cache).await;
    }

    let mut local_versions = cache
        .metadata
        .stored_versions
        .iter()
        .filter(|ver| args.all || ver.tags.contains(&StoredVersionTag::Current))
        .collect::<Vec<_>>();

    local_versions.sort_by(|a, b| match a.version.cmp(&b.version) {
        Ordering::Equal => a.fetched_at.cmp(&b.fetched_at),
        ord => ord,
    });

    if local_versions.is_empty() {
        println!("No UCD versions found");
        println!("Use `ag-gen unicode fetch latest` to fetch the latest version");
        return Ok(());
    }

    for version in &cache.metadata.stored_versions {
        let tag = local_tag(version);
        println!("\t{:?}{}", version.version, tag);
    }

    Ok(())
}

async fn list_with_remote(args: ListArgs, cache: &mut cache::Cache) -> eyre::Result<()> {
    let mut ftp = remote::ftp::login().await?;
    let remote_versions = remote::list_versions(&mut ftp).await?;
    let latest_version = remote::get_latest_version(&mut ftp).await?;
    let draft_version = remote::get_draft_version(&remote_versions, &latest_version)?;

    let mut version_map = HashMap::new();
    for version in &cache.metadata.stored_versions {
        if !version.tags.contains(&StoredVersionTag::Current) {
            continue;
        }

        version_map.insert(&version.version, version);
    }

    for version in remote_versions {
        let remote_tag = remote_tag(&version, &latest_version, draft_version.as_ref());
        let remote_line = format!("{:?}{}", &version, remote_tag);

        match version_map.get(&version) {
            Some(&local_version) => {
                let local_tag = local_tag(local_version);
                println!(
                    "\t * {} -> fetched {:?}{}",
                    remote_line, local_version.fetched_at, local_tag
                );
            },
            None => {
                println!("\t   {}", remote_line);
            },
        }
    }

    Ok(())
}

fn local_tag(version: &StoredVersion) -> String {
    if version.tags.contains(&StoredVersionTag::Latest) {
        Some("latest")
    } else if version.tags.contains(&StoredVersionTag::Draft) {
        Some("draft")
    } else {
        None
    }
    .map(|tag| format!(" ({})", tag))
    .unwrap_or_default()
}

fn remote_tag(
    version: &UnicodeVersion,
    latest_version: &UnicodeVersion,
    draft_version: Option<&UnicodeVersion>,
) -> String {
    if latest_version == version {
        Some("latest")
    } else if draft_version.is_some_and(|d| d == version) {
        Some("draft")
    } else {
        None
    }
    .map(|tag| format!(" ({})", tag))
    .unwrap_or_default()
}
