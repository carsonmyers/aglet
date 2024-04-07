use std::sync::Arc;

use indicatif::{ProgressBar, ProgressStyle};
use tokio::sync::Mutex;

use crate::progress;
use crate::progress::data::PercentComplete;
use crate::progress::stats::{ImmediateFileStats, ImmediateStats, StatsSlots, TransientStats};
use crate::unicode::remote::{DownloadRemoteFiles, EnumerateRemoteFiles, ResolveSelectedVersion};
use crate::unicode::{SelectVersion, UnicodeVersion};

pub struct VersionPhase {
    msg: String,
    select_version: Arc<SelectVersion>,
    resolved_version: Arc<Mutex<Option<UnicodeVersion>>>,
}

impl VersionPhase {
    pub fn phase(dl: &ResolveSelectedVersion) -> Box<dyn progress::Phase> {
        let msg = match *dl.select_version() {
            SelectVersion::Draft => String::from("Looking for draft version..."),
            SelectVersion::Latest => String::from("Looking for latest version..."),
            SelectVersion::Version(version) => format!("Looking for {}...", version),
        };

        Box::new(Self {
            msg,
            select_version: dl.select_version(),
            resolved_version: dl.resolved_version(),
        })
    }
}

impl progress::Phase for VersionPhase {
    fn init(&self) -> Vec<ProgressBar> {
        vec![ProgressBar::new_spinner()]
    }

    fn update_msg(&self, _index: usize) -> Option<String> {
        Some(self.msg.clone())
    }

    fn finish(&self) -> Option<String> {
        let version = self.resolved_version.try_lock().ok()?;
        let Some(version) = *version else {
            return None;
        };

        if self.select_version.is_draft() {
            Some(format!("Download draft version {}", version))
        } else if self.select_version.is_latest() {
            Some(format!("Download latest version {}", version))
        } else {
            Some(format!("Download version {}", version))
        }
    }
}

pub struct EnumeratePhase {
    stats: Arc<ImmediateStats>,
}

impl EnumeratePhase {
    pub fn phase(dl: &EnumerateRemoteFiles) -> Box<dyn progress::Phase> {
        Box::new(Self { stats: dl.stats() })
    }
}

impl progress::Phase for EnumeratePhase {
    fn init(&self) -> Vec<ProgressBar> {
        vec![ProgressBar::new_spinner()]
    }

    fn update_msg(&self, _index: usize) -> Option<String> {
        Some(format!(
            "Gathering files...\t{}\t{}",
            self.stats.total_files(),
            self.stats.total_bytes()
        ))
    }

    fn finish(&self) -> Option<String> {
        Some(format!(
            "Found {} target file(s)",
            self.stats.total_files().0
        ))
    }
}

pub struct DownloadPhase {
    stats: Arc<ImmediateStats>,
    slots: Arc<StatsSlots<ImmediateFileStats>>,
    transient_stats: TransientStats,
}

impl DownloadPhase {
    pub fn phase(dl: &DownloadRemoteFiles) -> Box<dyn progress::Phase> {
        Box::new(Self {
            stats: dl.stats(),
            slots: dl.slots(),
            transient_stats: TransientStats::new(),
        })
    }

    fn update_status_msg(&self) -> Option<String> {
        Some(format!(
            "{}\t{}\t{}",
            self.stats.files_progress(),
            self.stats.bytes_progress(),
            self.transient_stats.bytes_speed()
        ))
    }

    fn update_status_percent(&self) -> PercentComplete {
        self.stats.percent_complete()
    }

    fn update_file_msg(&self, slot: usize) -> Option<String> {
        let slot = self.slots.try_get(slot).ok()?;
        Some(slot.filename().to_string())
    }

    fn update_file_percent(&self, slot: usize) -> Option<PercentComplete> {
        let slot = self.slots.try_get(slot).ok()?;
        Some(slot.percent_complete())
    }
}

impl progress::Phase for DownloadPhase {
    fn init(&self) -> Vec<ProgressBar> {
        let status = ProgressBar::new_spinner();
        let bar_style = ProgressStyle::with_template(
            "[{elapsed_precise}] {wide_bar:.cyan/blue} {percent_precise}%",
        )
        .expect("valid progress style");
        let progress = ProgressBar::new(0).with_style(bar_style);

        let file_style =
            ProgressStyle::with_template("{spinner} {wide_msg} {bytes}/{total_bytes} {percent}%")
                .expect("valid file progress style");

        let mut bars = vec![status, progress];
        bars.extend(
            (0..self.slots.len())
                .map(|_| ProgressBar::new_spinner().with_style(file_style.clone())),
        );

        bars
    }

    fn tick(&mut self) {
        self.transient_stats.measure_stats(self.stats.clone());
    }

    fn update_msg(&self, index: usize) -> Option<String> {
        match index {
            0 => self.update_status_msg(),
            1 => None,
            i => self.update_file_msg(i - 2),
        }
    }

    fn update_percent(&self, index: usize) -> Option<PercentComplete> {
        match index {
            0 => Some(self.update_status_percent()),
            1 => Some(self.update_status_percent()),
            i => self.update_file_percent(i - 2),
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
