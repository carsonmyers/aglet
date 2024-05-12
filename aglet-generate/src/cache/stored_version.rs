use std::collections::HashSet;
use std::{cmp, fmt};

use chrono::{DateTime, Local, Utc};
use eyre::eyre;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use tracing::info;

use crate::cache::{DRAFT_VERSION_TTL, FORMER_VERSION_TTL, LATEST_VERSION_TTL};
use crate::unicode::{SelectVersion, UnicodeVersion};

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
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

    pub fn is_latest(&self) -> bool {
        self.tags.contains(&StoredVersionTag::Latest)
    }

    pub fn is_draft(&self) -> bool {
        self.tags.contains(&StoredVersionTag::Draft)
    }

    pub fn is_expired(&self) -> bool {
        self.expires_at <= Utc::now()
    }

    pub fn is_valid(&self) -> bool {
        self.expires_at > Utc::now()
    }

    pub fn selected_by(&self, select: &SelectVersion) -> bool {
        match select {
            SelectVersion::Latest => self.is_latest(),
            SelectVersion::Draft => self.is_draft(),
            SelectVersion::Version(v) => *v == self.version,
            SelectVersion::Hash(hash) => self.hash.starts_with(hash),
        }
    }
}

impl fmt::Display for StoredVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fetched_at = self.fetched_at.with_timezone(&Local);
        let tags = if self.tags.is_empty() {
            String::new()
        } else {
            format!(" ({})", self.tags.iter().sorted().join(", "))
        };

        let expires_at = self.expires_at.with_timezone(&Local);
        let expires_str = if self.is_valid() {
            let expires_in = expires_at - Local::now();
            let duration_str = match (
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

            format!(", valid for {}", duration_str)
        } else {
            format!(", expired {}", expires_at.format("%c"))
        };

        write!(
            f,
            "{} fetched {}{}{}",
            &self.hash[..7],
            fetched_at.format("%c"),
            expires_str,
            tags
        )
    }
}

impl PartialEq for StoredVersion {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Eq for StoredVersion {}

impl PartialOrd for StoredVersion {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StoredVersion {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match self.version.cmp(&other.version) {
            cmp::Ordering::Equal => match self.expires_at.cmp(&other.expires_at) {
                cmp::Ordering::Equal => self.fetched_at.cmp(&other.fetched_at),
                other => other,
            },
            other => other,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, Serialize, Deserialize, Ord, PartialOrd)]
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

impl fmt::Display for StoredVersionTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Current => f.write_str("Current"),
            Self::Latest => f.write_str("Latest"),
            Self::Draft => f.write_str("Draft"),
        }
    }
}
