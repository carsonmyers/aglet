use std::str::FromStr;
use std::sync::Arc;

use eyre::{eyre, OptionExt};
use tokio::task::JoinHandle;

use crate::ftp::list_versions::ListVersionOptions;
use crate::ftp::{new_pool, Entry, Pool, RemoteVersion};
use crate::unicode::UnicodeVersion;

pub struct ListVersions {
    pool: Arc<Pool>,
}

impl ListVersions {
    pub fn new(options: ListVersionOptions) -> Self {
        let pool = options.get_pool().unwrap_or_else(|| Arc::new(new_pool(1)));

        Self { pool }
    }

    pub async fn list_versions(self) -> eyre::Result<Vec<RemoteVersion>> {
        let pool = self.pool.clone();
        let versions: JoinHandle<eyre::Result<Vec<UnicodeVersion>>> = tokio::spawn(async move {
            let mut ftp = pool.get().await?;

            let entries = ftp
                .list(Some("/Public"))
                .await?
                .into_iter()
                .map(|line| Entry::from_str(&line))
                .collect::<Result<Vec<_>, _>>()?;

            let mut versions = entries
                .into_iter()
                .filter_map(Entry::directory)
                .filter_map(|dir| UnicodeVersion::from_str(&dir.name).ok())
                .collect::<Vec<_>>();

            versions.sort();

            Ok(versions)
        });

        let pool = self.pool.clone();
        let latest_version: JoinHandle<eyre::Result<UnicodeVersion>> = tokio::spawn(async move {
            let mut ftp = pool.get().await?;

            let entries = ftp
                .list(Some("/Public/UCD"))
                .await?
                .into_iter()
                .map(|line| Entry::from_str(&line))
                .collect::<Result<Vec<_>, _>>()?;

            let latest_link = entries
                .into_iter()
                .find(|entry| entry.name() == "latest")
                .ok_or_eyre("could not find latest version: /Public/UCD/latest not found")
                .map(Entry::link)?
                .ok_or_eyre("could not find latest version: /Public/UCD/latest is not a link")?;

            let latest = latest_link
                .target
                .rsplit_once('/')
                .map(|(_, name)| UnicodeVersion::from_str(name))
                .ok_or_eyre(
                    "could not find latest version: /Public/UCD/latest doesn't link to a version",
                )??;

            Ok(latest)
        });

        let versions = versions.await??;
        let latest_version = latest_version.await??;

        let mut after_latest = versions
            .iter()
            .skip_while(|&version| version <= &latest_version);

        let draft_version = after_latest.next().cloned();

        if after_latest.next().is_some() {
            return Err(eyre!("multiple draft versions detected"));
        }

        let versions = versions
            .into_iter()
            .map(|version| RemoteVersion::new(version, &latest_version, &draft_version))
            .collect();

        Ok(versions)
    }
}
