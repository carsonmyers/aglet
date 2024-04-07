use std::borrow::Cow;
use std::sync::Arc;

use indicatif::ProgressBar;

use crate::progress;
use crate::progress::data::PercentComplete;
use crate::progress::stats::ImmediateStats;
use crate::unicode::cache::HashFiles;

pub struct HashPhase {
    stats: Arc<ImmediateStats>,
}

impl HashPhase {
    pub fn phase(stage: &HashFiles) -> Box<dyn progress::Phase> {
        Box::new(Self {
            stats: stage.stats(),
        })
    }
}

impl progress::Phase for HashPhase {
    fn init(&self) -> Vec<ProgressBar> {
        vec![ProgressBar::new_spinner()]
    }

    fn update_msg(&self, _index: usize) -> Option<String> {
        Some(format!("Hashing files...\t{}", self.stats.files_progress()))
    }

    fn update_percent(&self, _index: usize) -> Option<PercentComplete> {
        Some(self.stats.percent_complete())
    }
}
