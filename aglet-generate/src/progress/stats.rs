use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use eyre::eyre;
use tokio::sync::RwLock;
use tokio::time;

use crate::progress::data;

const SAMPLE_COUNT: usize = 16;

#[derive(Default, Debug)]
pub struct ImmediateStats {
    files_tx:  AtomicUsize,
    files_tot: AtomicUsize,
    bytes_tx:  AtomicUsize,
    bytes_tot: AtomicUsize,
}

impl ImmediateStats {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_totals(files_tot: usize, bytes_tot: usize) -> Self {
        Self {
            files_tot: AtomicUsize::new(files_tot),
            bytes_tot: AtomicUsize::new(bytes_tot),
            ..Default::default()
        }
    }

    pub fn add_file(&self, size: usize) {
        self.bytes_tot.fetch_add(size, Ordering::SeqCst);
        self.files_tot.fetch_add(1, Ordering::SeqCst);
    }

    pub fn tx(&self, bytes: usize) {
        self.bytes_tx.fetch_add(bytes, Ordering::SeqCst);
    }

    pub fn file_complete(&self) {
        self.files_tx.fetch_add(1, Ordering::SeqCst);
    }

    pub fn total_files(&self) -> data::TotalFiles {
        single_stat(&self.files_tot, data::TotalFiles)
    }

    pub fn files_progress(&self) -> data::FilesProgress {
        stats_pair(&self.files_tx, &self.files_tot, data::FilesProgress)
    }

    pub fn total_bytes(&self) -> data::TotalBytes {
        single_stat(&self.bytes_tot, data::TotalBytes)
    }

    pub fn bytes_progress(&self) -> data::BytesProgress {
        stats_pair(&self.bytes_tx, &self.bytes_tot, data::BytesProgress)
    }

    pub fn percent_complete(&self) -> data::PercentComplete {
        stats_pair(&self.bytes_tx, &self.bytes_tot, data::PercentComplete)
    }
}

#[derive(Default)]
pub struct ImmediateFileStats {
    filename:  String,
    bytes_tx:  AtomicUsize,
    bytes_tot: AtomicUsize,
}

impl ImmediateFileStats {
    pub fn new<S: Into<String>>(filename: S, size: usize) -> Self {
        Self {
            filename: filename.into(),
            bytes_tot: AtomicUsize::new(size),
            ..Default::default()
        }
    }

    pub fn tx(&self, bytes: usize) {
        self.bytes_tx.fetch_add(bytes, Ordering::SeqCst);
    }

    pub fn filename(&self) -> &str {
        self.filename.as_str()
    }

    pub fn total_bytes(&self) -> data::TotalBytes {
        single_stat(&self.bytes_tot, data::TotalBytes)
    }

    pub fn bytes_progress(&self) -> data::BytesProgress {
        stats_pair(&self.bytes_tx, &self.bytes_tot, data::BytesProgress)
    }

    pub fn percent_complete(&self) -> data::PercentComplete {
        stats_pair(&self.bytes_tx, &self.bytes_tot, data::PercentComplete)
    }
}

pub struct TransientStats {
    last_measure:    time::Instant,
    samples:         [usize; SAMPLE_COUNT],
    next_sample_idx: usize,
    last_tx:         usize,
}

impl TransientStats {
    pub fn new() -> Self {
        Self {
            last_measure:    time::Instant::now(),
            samples:         [0usize; SAMPLE_COUNT],
            next_sample_idx: 0,
            last_tx:         0,
        }
    }

    pub fn measure_stats(&mut self, stats: Arc<ImmediateStats>) {
        self.measure(stats.bytes_tx.load(Ordering::SeqCst))
    }

    pub fn measure_file_stats(&mut self, stats: Arc<ImmediateFileStats>) {
        self.measure(stats.bytes_tx.load(Ordering::SeqCst))
    }

    pub fn bytes_speed(&self) -> data::BytesSpeed {
        let sample_sum = self.samples.iter().sum::<usize>() as f64;
        let sample_avg = sample_sum / SAMPLE_COUNT as f64;

        data::BytesSpeed(sample_avg as u64)
    }

    fn measure(&mut self, tx_now: usize) {
        let time_now = time::Instant::now();

        // calculate time and transmission deltas
        let delta_time = time_now - self.last_measure;
        let delta_tx = tx_now - self.last_tx;
        self.last_measure = time_now;
        self.last_tx = tx_now;

        // samples are normalized as a per-second amount
        let sample_norm_factor = delta_time.as_secs_f64().recip();
        let sample = delta_tx as f64 * sample_norm_factor;

        // add a sample to the list for averaging rates
        self.samples[self.next_sample_idx] = sample as usize;
        self.next_sample_idx = (self.next_sample_idx + 1) % SAMPLE_COUNT;
    }
}

pub struct StatsSlots<T> {
    slots: RwLock<Vec<Option<Arc<T>>>>,
    len:   usize,
}

impl<T> StatsSlots<T> {
    pub fn new(slots: usize) -> Self {
        Self {
            slots: RwLock::new(vec![None; slots]),
            len:   slots,
        }
    }

    pub async fn insert(&self, item: T) -> eyre::Result<(Arc<T>, usize)> {
        let mut slots = self.slots.write().await;

        let idx = slots
            .iter()
            .position(|slot| slot.is_none())
            .ok_or_else(|| eyre!("no free slots in stats collection"))?;

        let item = Arc::new(item);
        slots[idx].replace(item.clone());

        Ok((item, idx))
    }

    pub async fn release(&self, idx: usize) -> eyre::Result<()> {
        let mut slots = self.slots.write().await;

        slots
            .get_mut(idx)
            .ok_or_else(|| eyre!("invalid slot index {}", idx))?
            .take();

        Ok(())
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub async fn get(&self, idx: usize) -> eyre::Result<Arc<T>> {
        let slots = self.slots.read().await;
        Self::get_inner(&slots, idx)
    }

    pub fn try_get(&self, idx: usize) -> eyre::Result<Arc<T>> {
        let slots = self.slots.try_read()?;
        Self::get_inner(&slots, idx)
    }

    fn get_inner(slots: &Vec<Option<Arc<T>>>, idx: usize) -> eyre::Result<Arc<T>> {
        match slots.get(idx) {
            Some(Some(item)) => Ok(item.clone()),
            Some(None) => Err(eyre!("slot {} is not populated", idx)),
            None => Err(eyre!("invalid slot index {}", idx)),
        }
    }
}

fn single_stat<T, F>(stat: &AtomicUsize, constructor: F) -> T
where
    F: Fn(u64) -> T,
{
    constructor(stat.load(Ordering::SeqCst) as u64)
}

fn stats_pair<T, F>(a: &AtomicUsize, b: &AtomicUsize, constructor: F) -> T
where
    F: Fn(u64, u64) -> T,
{
    constructor(
        a.load(Ordering::SeqCst) as u64,
        b.load(Ordering::SeqCst) as u64,
    )
}
