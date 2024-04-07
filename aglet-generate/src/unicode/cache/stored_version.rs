use std::collections::HashSet;

use chrono::{DateTime, Utc};
use eyre::eyre;
use serde::{Deserialize, Serialize};

use super::UnicodeVersion;
use crate::unicode::SelectVersion;

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct StoredVersion {
    pub version: UnicodeVersion,
    pub hash: String,
    pub tags: HashSet<StoredVersionTag>,
    #[serde(with = "toml_datetime_compat")]
    pub fetched_at: DateTime<Utc>,
}

impl StoredVersion {
    pub fn new_current(
        version: UnicodeVersion,
        hash: String,
        tag: Option<StoredVersionTag>,
    ) -> Self {
        let mut tags = HashSet::new();
        tags.insert(StoredVersionTag::Current);
        if let Some(tag) = tag {
            tags.insert(tag);
        }

        Self {
            version,
            hash,
            tags,
            fetched_at: Utc::now(),
        }
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
