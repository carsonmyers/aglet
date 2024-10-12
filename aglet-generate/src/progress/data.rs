use std::fmt::{Display, Formatter};

use indicatif::HumanBytes;

pub struct TotalFiles(pub u64);

impl Display for TotalFiles {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} files", self.0)
    }
}

pub struct FilesProgress(pub u64, pub u64);

impl Display for FilesProgress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0 > self.1 {
            panic!("invalid progress: completed {} > total {}", self.0, self.1)
        }

        write!(
            f,
            "{}/{} files ({} remaining)",
            self.0,
            self.1,
            self.1 - self.0
        )
    }
}

pub struct TotalBytes(pub u64);

impl Display for TotalBytes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", HumanBytes(self.0))
    }
}

pub struct BytesProgress(pub u64, pub u64);

impl Display for BytesProgress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let remaining = if self.0 <= self.1 { self.1 - self.0 } else { 0 };

        write!(
            f,
            "{}/{} ({} remaining)",
            HumanBytes(self.0),
            HumanBytes(self.1),
            HumanBytes(remaining),
        )
    }
}

pub struct BytesSpeed(pub u64);

impl Display for BytesSpeed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/s", HumanBytes(self.0))
    }
}

pub struct PercentComplete(pub u64, pub u64);

impl Display for PercentComplete {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.1}%", self.0 as f64 / self.1 as f64)
    }
}
