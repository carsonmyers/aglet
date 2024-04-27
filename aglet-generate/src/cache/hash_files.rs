use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use itertools::Itertools;
use tokio::task::JoinSet;

use crate::progress::stats::ImmediateStats;

pub struct HashFiles {
    base_path: Arc<PathBuf>,
    filenames: Vec<PathBuf>,
    hashes: HashMap<PathBuf, String>,
    stats: Arc<ImmediateStats>,
}

impl HashFiles {
    pub fn new<P: Into<PathBuf>>(base_path: P, filenames: Vec<PathBuf>) -> eyre::Result<Self> {
        let stats = Arc::new(ImmediateStats::from_totals(filenames.len(), 0));
        let base_path = Arc::new(base_path.into());

        Ok(Self {
            base_path,
            filenames,
            hashes: HashMap::new(),
            stats,
        })
    }

    pub fn stats(&self) -> Arc<ImmediateStats> {
        self.stats.clone()
    }

    pub async fn hash_files(mut self) -> eyre::Result<String> {
        let mut join_set: JoinSet<eyre::Result<(PathBuf, String)>> = JoinSet::new();

        for filename in self.filenames {
            let stats = self.stats.clone();
            let base_path = self.base_path.clone();
            join_set.spawn(async move {
                let digest = sha256::try_async_digest(&filename).await?;
                let filename = filename.strip_prefix(base_path.as_ref())?;
                stats.add_file(0);

                Ok((filename.to_path_buf(), digest))
            });
        }

        while let Some(res) = join_set.join_next().await {
            let (filename, digest) = res??;
            self.hashes.insert(filename, digest);
        }

        let payload = self
            .hashes
            .iter()
            .sorted_by_key(|&(filename, _)| filename)
            .map(|(filename, digest)| format!("{}\t{}", filename.display(), digest))
            .collect::<Vec<_>>()
            .join("\n");

        Ok(sha256::digest(payload))
    }
}
