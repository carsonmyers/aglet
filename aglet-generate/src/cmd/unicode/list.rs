use std::collections::HashMap;
use std::sync::Arc;

use chrono::{Duration, Local};
use clap::Args;
use console::style;
use itertools::Itertools;

use crate::cache::{Cache, StoredVersion, StoredVersionTag};
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
            .get_or_replace(Duration::days(15), list_versions.list_versions())
            .await?;

        for &version in remote_listing {
            version_map.insert(
                version.version,
                VersionListing::from_remote_version(version),
            );
        }

        progress.finish().await?;
    }

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
        let local_versions = cache.metadata.versions().len();
        let local_copies = cache.metadata.stored_versions.len();
        let text = format!("{} UCD versions cached locally ({} copies in total)\n{} versions listed from unicode.org", local_versions, local_copies, version_map.len());

        eprintln!("{}", style(text).black().bright());
    }

    eprintln!();

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
            let version_str = if listing.cached.iter().any(|v| !v.is_expired()) {
                style(version_str).bright().green()
            } else {
                style(version_str).red()
            };

            println!("  {}", version_str);

            for cached_version in &listing.cached {
                let fetched_at = cached_version.fetched_at.with_timezone(&Local);
                let mut tags = Vec::new();
                if cached_version.tags.contains(&StoredVersionTag::Latest) {
                    tags.push("Latest".to_string());
                }
                if cached_version.tags.contains(&StoredVersionTag::Draft) {
                    tags.push("Draft".to_string());
                }
                if cached_version.tags.contains(&StoredVersionTag::Current) {
                    tags.push("Current".to_string());
                }
                if cached_version.is_expired() {
                    tags.push("Expired".to_string());
                }

                let tags = if tags.is_empty() {
                    String::new()
                } else {
                    format!(" ({})", tags.join(", "))
                };

                let expires = if cached_version.is_current() {
                    let expires_at = cached_version.expires_at.with_timezone(&Local);
                    if cached_version.is_expired() {
                        format!(", expired {}", expires_at.format("%c"))
                    } else {
                        let expires_in = expires_at - Local::now();
                        let expires_in_str = match (
                            expires_in.num_weeks(),
                            expires_in.num_days(),
                            expires_in.num_hours(),
                        ) {
                            (w, _, _) if w > 0 => format!("{} weeks", w),
                            (0, d, _) if d > 0 => format!("{} days", d),
                            (0, 0, h) if h > 0 => format!("{} hours", h),
                            _ => format!(
                                "{}:{}",
                                expires_in.num_minutes(),
                                expires_in.num_seconds() % 60
                            ),
                        };

                        format!(", expires in {}", expires_in_str)
                    }
                } else {
                    String::new()
                };

                let version_line = style(format!(
                    "   - {} fetched {}{}{}",
                    &cached_version.hash[..7],
                    fetched_at.format("%c"),
                    expires,
                    tags,
                ));

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
