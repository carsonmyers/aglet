use std::sync::Arc;

use super::Download;
use crate::progress;
use crate::progress::data::PercentComplete;
use crate::progress::{stats, Phase};
use indicatif::{ProgressBar, ProgressStyle};

const PROGRESS_TEMPLATE: &str = "[{elapsed_precise}] {wide_bar:.cyan/blue} {percent_precise}%";
const FILE_TEMPLATE: &str = "{spinner} {wide_msg} {bytes}/{total_bytes} {percent}%";

pub struct DownloadProgress {
    stats: Arc<stats::ImmediateStats>,
    slots: Arc<stats::StatsSlots<stats::ImmediateFileStats>>,
    transient: stats::TransientStats,
}

impl DownloadProgress {
    pub fn new(download: &Download) -> Self {
        Self {
            stats: download.stats(),
            slots: download.slots(),
            transient: stats::TransientStats::new(),
        }
    }

    pub fn new_phase(download: &Download) -> Box<dyn Phase> {
        Box::new(Self::new(download))
    }

    fn status_msg(&self) -> String {
        format!(
            "{}\t{}\t{}",
            self.stats.files_progress(),
            self.stats.bytes_progress(),
            self.transient.bytes_speed(),
        )
    }

    fn file_msg(&self, slot: usize) -> Option<String> {
        self.slots
            .try_get(slot)
            .expect("valid slot")
            .map(|slot| slot.filename().to_string())
    }

    fn file_percent(&self, slot: usize) -> Option<PercentComplete> {
        self.slots
            .try_get(slot)
            .expect("valid slot")
            .map(|slot| slot.percent_complete())
    }
}

impl Phase for DownloadProgress {
    fn init(&self) -> Vec<ProgressBar> {
        let style = ProgressStyle::default_spinner().tick_chars(progress::SPINNER_SCAN_LONG);
        let status = ProgressBar::new_spinner().with_style(style);

        let style = ProgressStyle::with_template(PROGRESS_TEMPLATE)
            .expect("valid progress style")
            .tick_chars(progress::SPINNER_SCAN);
        let progress = ProgressBar::new(0).with_style(style);

        let mut bars = vec![status, progress];

        let style = ProgressStyle::with_template(FILE_TEMPLATE)
            .expect("valid file progress style")
            .tick_chars(progress::SPINNER_SCAN);
        for _ in 0..self.slots.len() {
            bars.push(ProgressBar::new_spinner().with_style(style.clone()))
        }

        bars
    }

    fn tick(&mut self) {
        self.transient.measure_stats(self.stats.clone());
    }

    fn update_msg(&self, index: usize) -> Option<String> {
        match index {
            0 => Some(self.status_msg()),
            1 => None,
            i => self.file_msg(i - 2),
        }
    }

    fn update_percent(&self, index: usize) -> Option<PercentComplete> {
        match index {
            0 => Some(self.stats.percent_complete()),
            1 => Some(self.stats.percent_complete()),
            i => self.file_percent(i - 2),
        }
    }

    fn finish(&self) -> Option<String> {
        Some(format!(
            "Downloaded {} {}",
            self.stats.total_files(),
            self.stats.total_bytes(),
        ))
    }
}
