use std::collections::HashSet;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::UnicodeVersion;

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
