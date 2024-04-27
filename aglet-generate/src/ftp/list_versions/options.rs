use std::sync::Arc;

use crate::ftp::Pool;

pub struct ListVersionOptions {
    pool: Option<Arc<Pool>>,
}

impl ListVersionOptions {
    pub fn new() -> Self {
        Self { pool: None }
    }

    pub fn with_pool(mut self, pool: Arc<Pool>) -> Self {
        self.pool = Some(pool);
        self
    }

    pub fn get_pool(&self) -> Option<Arc<Pool>> {
        match &self.pool {
            Some(pool) => Some(pool.clone()),
            None => None,
        }
    }
}
