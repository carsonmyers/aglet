use std::sync::Arc;

use eyre::eyre;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

use crate::unicode::remote::pool::{new_pool, Pool};
use crate::unicode::{remote, SelectVersion, UnicodeVersion};

pub struct ResolveSelectedVersion {
    select_version: Arc<SelectVersion>,
    resolved_version: Arc<Mutex<Option<UnicodeVersion>>>,
    pool: Arc<Pool>,
}

impl ResolveSelectedVersion {
    pub fn new(select_version: SelectVersion, max_connections: usize) -> Self {
        let pool = new_pool(max_connections);

        Self {
            select_version: Arc::new(select_version),
            resolved_version: Arc::new(Mutex::new(None)),
            pool: Arc::new(pool),
        }
    }

    pub fn select_version(&self) -> Arc<SelectVersion> {
        self.select_version.clone()
    }

    pub fn resolved_version(&self) -> Arc<Mutex<Option<UnicodeVersion>>> {
        self.resolved_version.clone()
    }

    pub async fn resolve_selected_version(mut self) -> eyre::Result<UnicodeVersion> {
        let pool = self.pool.clone();
        let remote_versions: JoinHandle<eyre::Result<_>> = tokio::spawn(async move {
            let mut ftp = pool.get().await?;
            let versions = remote::list_versions(&mut ftp).await?;

            Ok(versions)
        });

        let pool = self.pool.clone();
        let latest_version: JoinHandle<eyre::Result<_>> = tokio::spawn(async move {
            let mut ftp = pool.get().await?;
            let latest = remote::get_latest_version(&mut ftp).await?;

            Ok(latest)
        });

        let remote_versions = remote_versions.await??;
        let latest_version = latest_version.await??;
        let draft_version = remote::get_draft_version(&remote_versions, &latest_version)?;

        let version = match *self.select_version {
            SelectVersion::Latest => latest_version,
            SelectVersion::Draft => match draft_version {
                Some(draft_version) => draft_version,
                None => return Err(eyre!("No draft version found")),
            },
            SelectVersion::Version(version) => version,
        };

        if draft_version.is_some_and(|draft_version| version == draft_version) {
            return Err(eyre!(
                "downloading a UCD draft version is not yet supported"
            ));
        }

        let version_exists = remote_versions
            .iter()
            .any(|remote_version| remote_version == &version);

        if !version_exists {
            return Err(eyre!("Version {:?} does not exist", version));
        }

        let mut resolved_version = self.resolved_version.lock().await;
        resolved_version.replace(version);

        Ok(version)
    }
}
