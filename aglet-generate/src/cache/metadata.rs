use std::path::Path;

use eyre::Context;
use serde::{Deserialize, Serialize};
use tokio::fs;
use tokio::io::AsyncWriteExt;
use tracing::info;

use crate::cache::cached_value::CachedValue;
use crate::cache::stored_version::StoredVersion;
use crate::ftp::RemoteVersion;
use crate::unicode::SelectVersion;

#[derive(Debug, Serialize, Deserialize)]
pub struct Metadata {
    #[serde(with = "toml_datetime_compat")]
    pub updated_at:      chrono::DateTime<chrono::Utc>,
    #[serde(default)]
    pub use_version:     Option<SelectVersion>,
    #[serde(default)]
    pub stored_versions: Vec<StoredVersion>,
    #[serde(default)]
    pub remote_listing:  CachedValue<Vec<RemoteVersion>>,
}

impl Metadata {
    pub async fn load<P: AsRef<Path>>(path: P) -> eyre::Result<Self> {
        let data = fs::read_to_string(path)
            .await
            .wrap_err("could not read metadata file")?;

        let mut metadata: Metadata = toml::from_str(&data).wrap_err("malformed metadata file")?;

        metadata.stored_versions.sort();

        Ok(metadata)
    }

    pub async fn write<P: AsRef<Path>>(&mut self, path: P) -> eyre::Result<()> {
        self.updated_at = chrono::Utc::now();

        let data = toml::to_string_pretty(&self).wrap_err("could not serialize metadata")?;

        let mut file = fs::File::create(&path).await.wrap_err(format!(
            "could not create metadata file {}",
            path.as_ref().display()
        ))?;

        file.write_all(data.as_bytes())
            .await
            .wrap_err("could not write data to metadata file")?;

        file.flush()
            .await
            .wrap_err("could not flush metadata contents to file")?;

        info!("write metadata file {}", path.as_ref().display());

        Ok(())
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Self {
            updated_at:      chrono::Utc::now(),
            remote_listing:  Default::default(),
            stored_versions: Default::default(),
            use_version:     Default::default(),
        }
    }
}
