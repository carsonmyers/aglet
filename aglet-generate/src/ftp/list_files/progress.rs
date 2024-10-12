use super::ListFiles;
use crate::progress;
use crate::progress::{stats, Phase};
use color_eyre::owo_colors::OwoColorize;
use indicatif::{ProgressBar, ProgressStyle};
use std::sync::Arc;

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
