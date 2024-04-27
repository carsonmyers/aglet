use std::sync::Arc;

use indicatif::ProgressBar;

use super::ListFiles;
use crate::progress::{stats, Phase};

pub struct ListProgress {
    stats: Arc<stats::ImmediateStats>,
}

impl ListProgress {
    pub fn new(list_files: &ListFiles) -> Self {
        Self {
            stats: list_files.stats(),
        }
    }

    pub fn new_phase(list_files: &ListFiles) -> Box<dyn Phase> {
        Box::new(Self::new(list_files))
    }
}

impl Phase for ListProgress {
    fn init(&self) -> Vec<ProgressBar> {
        vec![ProgressBar::new_spinner()]
    }

    fn update_msg(&self, _index: usize) -> Option<String> {
        Some(format!(
            "Listing files...\t{}\t{}",
            self.stats.total_files(),
            self.stats.total_bytes(),
        ))
    }

    fn finish(&self) -> Option<String> {
        Some(format!("Found {} file(s)", self.stats.total_files().0))
    }
}
