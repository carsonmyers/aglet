mod cached_value;
mod hash_files;
mod metadata;
mod phase;
mod stored_version;

use std::path::{Path, PathBuf};

use eyre::eyre;
use rand::distributions::{Alphanumeric, DistString};
use tokio::fs;

use super::UnicodeVersion;
pub use hash_files::HashFiles;
pub use metadata::Metadata;
pub use phase::HashPhase;
pub use stored_version::{StoredVersion, StoredVersionTag};

#[derive(Debug)]
pub struct Cache {
    path: PathBuf,
    pub metadata: Metadata,
}

impl Cache {
    pub fn new<P: Into<PathBuf>>(path: P) -> Self {
        Self {
            path: path.into(),
            metadata: Default::default(),
        }
    }

    pub async fn init<P: Into<PathBuf>>(path: P) -> eyre::Result<Self> {
        let mut cache = Self::new(path);

        if fs::try_exists(&cache.path).await? {
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

        if !fs::try_exists(&cache.path).await? {
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
        self.write_metadata().await?;

        Ok(())
    }

    pub async fn get_tmp_dir(&mut self) -> eyre::Result<PathBuf> {
        let path = self.tmp_filename(None);

        fs::create_dir_all(&path).await?;

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
            return Ok(());
        }

        fs::rename(local, cache_path).await?;
        let stored_entry = StoredVersion::new_current(version, hash, tag);

        for existing in self.metadata.stored_versions.iter_mut() {
            if existing.version == version {
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
