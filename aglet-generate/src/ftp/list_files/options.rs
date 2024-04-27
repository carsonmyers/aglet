use std::collections::HashSet;
use std::sync::Arc;

use crate::ftp::Pool;

pub struct ListOptions {
    exclude_exts: HashSet<String>,
    pool:         Option<Arc<Pool>>,
}

impl ListOptions {
    pub fn new() -> Self {
        Self {
            exclude_exts: HashSet::new(),
            pool:         None,
        }
    }

    pub fn exclude_ext<S: Into<String>>(mut self, ext: S) -> Self {
        self.exclude_exts.insert(ext.into());
        self
    }

    pub fn with_pool(mut self, pool: Arc<Pool>) -> Self {
        self.pool = Some(pool);
        self
    }

    pub fn ext_excluded<S: AsRef<str>>(&self, ext: S) -> bool {
        self.exclude_exts.contains(ext.as_ref())
    }

    pub fn get_pool(&self) -> Option<Arc<Pool>> {
        match &self.pool {
            Some(pool) => Some(pool.clone()),
            None => None,
        }
    }
}
