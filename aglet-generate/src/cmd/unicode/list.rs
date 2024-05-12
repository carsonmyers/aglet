use std::collections::HashMap;
use std::sync::Arc;

use clap::Args;
use console::style;
use itertools::Itertools;

use crate::cache::{Cache, StoredVersion, REMOTE_LISTING_TTL};
use crate::cmd::unicode::CommonArgs;
use crate::ftp::{self, new_pool, RemoteVersion, RemoteVersionTag};
use crate::progress;
use crate::progress::phase;
use crate::unicode::UnicodeVersion;

#[derive(Args, Debug)]
pub struct ListArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(short, long)]
    remote: bool,

    #[arg(short, long)]
    all: bool,
}

pub async fn run(args: ListArgs, cache: &mut Cache) -> eyre::Result<()> {
    let mut version_map = HashMap::new();
    if args.remote {
        let pool = Arc::new(new_pool(args.common.max_connections));
        let progress = progress::Manager::new();

        progress.phase(phase("Listing versions from unicode.org..."))?;
        let options = ftp::ListVersionOptions::new().with_pool(pool);
        let list_versions = ftp::ListVersions::new(options);
        let remote_listing = cache
            .metadata
            .remote_listing
            .get_or_replace(REMOTE_LISTING_TTL, list_versions.list_versions())
            .await?;

        for &version in remote_listing {
            version_map.insert(
                version.version,
                VersionListing::from_remote_version(version),
            );
        }

        progress.finish().await?;
    }

    let remote_copies = version_map.len();

    for version in &cache.metadata.stored_versions {
        let listing = version_map
            .entry(version.version)
            .or_insert(VersionListing::new(version.version));

        listing.cached.push(version);
    }

    if version_map.is_empty() {
        let text = "No UCD versions found\nUse `ag-gen unicode fetch` to download and cache the unicode database";

        eprintln!("{}", style(text).black().bright());
        return Ok(());
    }

    if cache.metadata.stored_versions.is_empty() {
        let text = format!(
            "No UCD copies in local cache\n{} versions fetched from unicode.org",
            version_map.len()
        );

        eprintln!("{}", style(text).black().bright());
    } else {
        let local_versions = cache.versions().len();
        let local_copies = cache.metadata.stored_versions.len();
        let local_line = format!(
            "{} valid UCD versions in cache ({} copies in total)",
            local_versions, local_copies,
        );
        let remote_line = if remote_copies > 0 {
            format!("\n{} versions listed from unicode.org", remote_copies)
        } else {
            String::new()
        };

        eprintln!(
            "{}{}",
            style(local_line).white(),
            style(remote_line).bright().black()
        );
    }

    let current_version = cache.default_version().or_else(|err| {
        let text = format!("error reading current version: {}", err);
        eprintln!("{}", style(text).yellow());

        <Result<_, eyre::Report>>::Ok(None)
    })?;

    for version in version_map.keys().sorted() {
        let listing = version_map.get(version).expect("version should be present");

        let version_str = match listing.remote_tag {
            Some(tag) => format!("{} ({})", tag, listing.version),
            None => listing.version.to_string(),
        };

        if listing.cached.is_empty() {
            let version_str = style(version_str).bright().black();
            println!("  {}", version_str);
        } else {
            let current_indicator = match current_version {
                Some(current) if version == &current.version => "*",
                _ => " ",
            };

            let version_line = style(format!("{} {}", current_indicator, version_str));
            if listing.cached.iter().any(|v| !v.is_expired()) {
                println!("{}", version_line.bright().green());
            } else {
                println!("{}", version_line.red());
            }

            for cached_version in &listing.cached {
                let current_indicator = match current_version {
                    Some(current) if cached_version.hash == current.hash => "*",
                    _ => " ",
                };

                let version_line = style(format!("{}  {}", current_indicator, cached_version));

                if cached_version.is_expired() && cached_version.is_current() {
                    println!("{}", version_line.bright().red());
                } else if cached_version.is_current() {
                    println!("{}", version_line.bright().green());
                } else {
                    println!("{}", version_line.bright().black());
                }
            }
        }
    }

    Ok(())
}

struct VersionListing<'a> {
    version:    UnicodeVersion,
    remote_tag: Option<RemoteVersionTag>,
    cached:     Vec<&'a StoredVersion>,
}

impl<'a> VersionListing<'a> {
    fn new(version: UnicodeVersion) -> Self {
        Self {
            version,
            remote_tag: None,
            cached: Vec::new(),
        }
    }

    fn from_remote_version(version: RemoteVersion) -> Self {
        Self {
            version:    version.version,
            remote_tag: version.tag,
            cached:     Vec::new(),
        }
    }
}
