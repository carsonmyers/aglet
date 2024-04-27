use std::collections::HashSet;

use chrono::{DateTime, Utc};
use eyre::eyre;
use serde::{Deserialize, Serialize};
use tracing::info;

use crate::cache::{DRAFT_VERSION_TTL, FORMER_VERSION_TTL, LATEST_VERSION_TTL};
use crate::unicode::{SelectVersion, UnicodeVersion};

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct StoredVersion {
    #[serde(with = "toml_datetime_compat")]
    pub fetched_at: DateTime<Utc>,
    #[serde(with = "toml_datetime_compat")]
    pub expires_at: DateTime<Utc>,
    pub version:    UnicodeVersion,
    pub hash:       String,
    pub tags:       HashSet<StoredVersionTag>,
}

impl StoredVersion {
    pub fn new_current(
        version: UnicodeVersion,
        hash: String,
        tag: Option<StoredVersionTag>,
    ) -> Self {
        let fetched_at = Utc::now();
        let expires_at = fetched_at
            + match tag {
                Some(StoredVersionTag::Latest) => LATEST_VERSION_TTL,
                Some(StoredVersionTag::Draft) => DRAFT_VERSION_TTL,
                _ => FORMER_VERSION_TTL,
            };

        info!(
            "new stored version for {} (fetched: {}, expires: {})",
            version, fetched_at, expires_at
        );

        let mut tags = HashSet::new();
        tags.insert(StoredVersionTag::Current);
        if let Some(tag) = tag {
            tags.insert(tag);
        }

        Self {
            fetched_at,
            expires_at,
            version,
            hash,
            tags,
        }
    }

    pub fn is_current(&self) -> bool {
        self.tags.contains(&StoredVersionTag::Current)
    }

    pub fn is_expired(&self) -> bool {
        self.expires_at <= Utc::now()
    }

    pub fn is_valid(&self) -> bool {
        self.expires_at > Utc::now()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum StoredVersionTag {
    Current,
    Latest,
    Draft,
}

impl TryFrom<SelectVersion> for StoredVersionTag {
    type Error = eyre::Report;

    fn try_from(value: SelectVersion) -> Result<Self, Self::Error> {
        match &value {
            SelectVersion::Draft => Ok(Self::Draft),
            SelectVersion::Latest => Ok(Self::Latest),
            _ => Err(eyre!("No tag for {}", &value)),
        }
    }
}
