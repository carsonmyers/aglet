use std::fmt;

use serde::{Deserialize, Serialize};

use crate::unicode::UnicodeVersion;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct RemoteVersion {
    pub version: UnicodeVersion,
    pub tag:     Option<RemoteVersionTag>,
}

impl RemoteVersion {
    pub fn new(
        version: UnicodeVersion,
        latest: &UnicodeVersion,
        draft: &Option<UnicodeVersion>,
    ) -> Self {
        let tag = match (latest, draft) {
            (latest, _) if *latest == version => Some(RemoteVersionTag::Latest),
            (_, Some(draft)) if *draft == version => Some(RemoteVersionTag::Draft),
            _ => None,
        };

        Self { version, tag }
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum RemoteVersionTag {
    Latest,
    Draft,
}

impl fmt::Display for RemoteVersionTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Latest => f.write_str("Latest"),
            Self::Draft => f.write_str("Draft"),
        }
    }
}
