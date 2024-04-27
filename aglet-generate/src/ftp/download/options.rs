use std::path::PathBuf;
use std::sync::Arc;

use crate::ftp::Pool;

pub struct DownloadOptions {
    local: Option<PathBuf>,
    pool:  Option<Arc<Pool>>,
}

impl DownloadOptions {
    pub fn new() -> Self {
        Self {
            local: None,
            pool:  None,
        }
    }

    pub fn to(mut self, local: PathBuf) -> Self {
        self.local = Some(local);
        self
    }

    pub fn with_pool(mut self, pool: Arc<Pool>) -> Self {
        self.pool = Some(pool);
        self
    }

    pub fn take_local(mut self) -> Option<PathBuf> {
        self.local.take()
    }

    pub fn get_pool(&self) -> Option<Arc<Pool>> {
        match &self.pool {
            Some(pool) => Some(pool.clone()),
            None => None,
        }
    }
}
