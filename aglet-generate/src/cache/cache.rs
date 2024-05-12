use std::path::{Path, PathBuf};

use chrono::Duration;
use eyre::eyre;
use rand::distributions::{Alphanumeric, DistString};
use tokio::fs;
use tracing::{error, info};

use crate::cache::{Metadata, StoredVersion, StoredVersionTag};
use crate::unicode::{SelectVersion, UnicodeVersion};

pub const LATEST_VERSION_TTL: Duration = Duration::weeks(6);
pub const DRAFT_VERSION_TTL: Duration = Duration::weeks(1);
pub const FORMER_VERSION_TTL: Duration = Duration::weeks(52);
pub const REMOTE_LISTING_TTL: Duration = Duration::days(15);

#[derive(Debug)]
pub struct Cache {
    path:         PathBuf,
    pub metadata: Metadata,
}

impl Cache {
    pub fn new<P: Into<PathBuf>>(path: P) -> Self {
        Self {
            path:     path.into(),
            metadata: Default::default(),
        }
    }

    pub async fn init<P: Into<PathBuf>>(path: P) -> eyre::Result<Self> {
        let mut cache = Self::new(path);

        info!("initialize cache at {}", &cache.path.display());

        if fs::try_exists(&cache.path).await? {
            error!("failed to initialize cache: path already exists");
            return Err(eyre!(
                "file or directory {} already exists",
                cache.path.display()
            ));
        }

        fs::create_dir_all(cache.path.join("data")).await?;
        cache.write_metadata().await?;

        Ok(cache)
    }

    pub async fn load<P: Into<PathBuf>>(path: P) -> eyre::Result<Self> {
        let mut cache = Self::new(path);

        info!("load cache from {}", &cache.path.display());

        if !fs::try_exists(&cache.path).await? {
            error!("failed to load cache: path does not exist");
            return Err(eyre!("cache {} does not exist", cache.path.display()));
        }

        cache.load_metadata().await?;

        Ok(cache)
    }

    pub async fn load_or_init<P: Into<PathBuf>>(path: P) -> eyre::Result<Self> {
        let path = path.into();

        if fs::try_exists(&path).await? {
            Self::load(path).await
        } else {
            Self::init(path).await
        }
    }

    pub async fn save(&mut self) -> eyre::Result<()> {
        info!("save cache");
        self.write_metadata().await?;

        Ok(())
    }

    pub async fn get_tmp_dir(&mut self) -> eyre::Result<PathBuf> {
        let path = self.tmp_filename(None);

        fs::create_dir_all(&path).await?;
        info!("created tmp dir {}", &path.display());

        Ok(path)
    }

    fn tmp_filename(&self, ext: Option<&'static str>) -> PathBuf {
        let random_part = Alphanumeric.sample_string(&mut rand::thread_rng(), 16);
        let filename = format!(
            "tmp_{}{}",
            random_part,
            ext.map(|ext| format!(".{}", ext)).unwrap_or_default()
        );

        self.path.join(filename)
    }

    pub fn versions(&self) -> Vec<UnicodeVersion> {
        self.metadata
            .stored_versions
            .iter()
            .filter(|v| v.is_current() && v.is_valid())
            .map(|v| v.version)
            .collect()
    }

    pub fn version_or_default(
        &self,
        select: &Option<SelectVersion>,
    ) -> eyre::Result<Option<&StoredVersion>> {
        let Some(select) = select else {
            return self.default_version();
        };

        let target_version = self.version(select)?;

        Ok(Some(target_version))
    }

    pub fn version(&self, select: &SelectVersion) -> eyre::Result<&StoredVersion> {
        let mut candidate_versions = self
            .metadata
            .stored_versions
            .iter()
            .filter(|version| version.selected_by(select))
            .collect::<Vec<_>>();

        let target_version = candidate_versions.pop();
        let Some(target_version) = target_version else {
            return Err(eyre!("no stored version matching {}", select));
        };

        Ok(target_version)
    }

    pub fn default_version(&self) -> eyre::Result<Option<&StoredVersion>> {
        let Some(select) = self.metadata.use_version.as_ref() else {
            let target_version =
                self.metadata.stored_versions.iter().rev().find(|version| {
                    version.is_valid() && version.is_current() && !version.is_draft()
                });
            return Ok(target_version);
        };

        let target_version = self.version(select)?;

        Ok(Some(target_version))
    }

    pub fn version_path(&self, version: &StoredVersion) -> eyre::Result<PathBuf> {
        let path = self.path.join("data").join(&version.hash);
        if !path.is_dir() {
            return Err(eyre!(
                "data for stored version {} ({}) does not exist in cache ({})",
                version.version,
                &version.hash[..7],
                path.display()
            ));
        }

        Ok(path)
    }

    pub async fn intern_version<P: AsRef<Path>>(
        &mut self,
        local: P,
        version: UnicodeVersion,
        hash: String,
        tag: Option<StoredVersionTag>,
    ) -> eyre::Result<()> {
        let local = local.as_ref();

        let cache_path = self.path.join("data").join(&hash);
        if cache_path.is_dir() {
            info!(
                "version {} already seems to be interned {}",
                version,
                cache_path.display()
            );
            return Ok(());
        }

        fs::rename(local, cache_path).await?;
        let stored_entry = StoredVersion::new_current(version, hash, tag);

        for existing in self.metadata.stored_versions.iter_mut() {
            if existing.version == version {
                info!(
                    "remove `Current` tag from {} ({})",
                    existing.version, existing.hash
                );
                existing.tags.remove(&StoredVersionTag::Current);
            }
        }

        self.metadata.stored_versions.push(stored_entry);

        Ok(())
    }

    async fn load_metadata(&mut self) -> eyre::Result<()> {
        self.metadata = Metadata::load(self.path.join("metadata.toml")).await?;

        Ok(())
    }

    async fn write_metadata(&mut self) -> eyre::Result<()> {
        self.metadata.write(self.path.join("metadata.toml")).await
    }
}
