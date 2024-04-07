use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_ftp::{DataStream, FtpError, FtpStream};
use color_eyre::owo_colors::OwoColorize;
use console::style;
use eyre::eyre;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::mpsc;
use tokio::task::JoinSet;

use crate::progress::stats::{ImmediateFileStats, ImmediateStats, StatsSlots};
use crate::task::TaskCounter;
use crate::unicode::remote::entry_meta::Entry;
use crate::unicode::remote::pool::{new_pool, Pool};
use crate::unicode::remote::{entry_meta, ftp};

#[derive(Debug)]
pub struct DownloadOptions {
    remote: String,
    local: PathBuf,
    exclude_exts: HashSet<String>,
    max_connections: usize,
}

impl DownloadOptions {
    pub fn new(remote: String, local: PathBuf) -> Self {
        Self {
            remote,
            local,
            exclude_exts: HashSet::new(),
            max_connections: 1,
        }
    }

    pub fn with_max_connections(mut self, max_connections: usize) -> Self {
        self.max_connections = max_connections;
        self
    }

    pub fn exclude_ext<S: Into<String>>(mut self, ext: S) -> Self {
        self.exclude_exts.insert(ext.into());
        self
    }

    pub fn build(self) -> EnumerateRemoteFiles {
        EnumerateRemoteFiles::new(self)
    }
}

pub struct EnumerateRemoteFiles {
    options: Arc<DownloadOptions>,
    pool: Arc<Pool>,
    files: Vec<TargetFile>,
    stats: Arc<ImmediateStats>,
}

impl EnumerateRemoteFiles {
    pub fn new(options: DownloadOptions) -> Self {
        let pool = new_pool(options.max_connections);

        Self {
            options: Arc::new(options),
            pool: Arc::new(pool),
            files: Default::default(),
            stats: Arc::new(Default::default()),
        }
    }

    pub fn stats(&self) -> Arc<ImmediateStats> {
        self.stats.clone()
    }

    pub async fn enumerate_remote_files(mut self) -> eyre::Result<DownloadRemoteFiles> {
        let mut join_set: JoinSet<eyre::Result<Vec<TargetFile>>> = JoinSet::new();
        let (tx_dirs, mut rx_dirs) = mpsc::channel::<Vec<String>>(1024);
        tx_dirs.send(vec![]).await?;

        let counter = TaskCounter::new();

        loop {
            // keep reading new dirs through the channel until the reader is closed or the counter
            // notifies that all tasks have finished (so no new dirs will be produced)
            let prefix = tokio::select! {
                Some(prefix) = rx_dirs.recv() => prefix,
                _ = counter.wait_finish() => if rx_dirs.is_empty() { break } else { continue },
                else => break,
            };

            let remote = if prefix.is_empty() {
                self.options.remote.to_string()
            } else {
                format!("{}/{}", &self.options.remote, prefix.join("/"))
            };

            let local = self.options.local.join(prefix.iter().collect::<PathBuf>());
            let options = self.options.clone();
            let pool = self.pool.clone();
            let stats = self.stats.clone();
            let tx_dirs = tx_dirs.clone();

            // move this into the spawned task so that the counter will decrease whenever the task
            // exits; this is done instead of relying on the join handles because each task can
            // spawn an unknown number of additional tasks by sending new dirs through tx_dirs
            let task_counter = counter.count_task();

            join_set.spawn(async move {
                let _dec_on_drop = task_counter;
                let mut ftp = pool.get().await?;
                let (new_files, new_dirs) = get_files_and_dirs(&mut ftp, remote.as_str()).await?;

                // send new dirs to the parent task so that it will spawn a new task for each
                for new_dir in new_dirs {
                    let mut new_prefix = prefix.clone();
                    new_prefix.push(new_dir.name);
                    tx_dirs.send(new_prefix).await.expect("can send dir");
                }

                // filter files with excluded extensions and map them into target files
                let new_files = new_files
                    .into_iter()
                    .filter(|file| match file.name.rsplit_once('.') {
                        Some((_, ext)) => !options.exclude_exts.contains(ext),
                        None => true,
                    })
                    .map(|file| TargetFile {
                        remote: format!("{}/{}", remote, &file.name),
                        local: local.join(&file.name),
                        size: file.size,
                    })
                    .inspect(|file| stats.add_file(file.size))
                    .collect::<Vec<_>>();

                // resolve to the new files found at this prefix; the prefix is also included
                // so that the local and remote paths can be constructed correctly
                Ok(new_files)
            });

            // when there all tasks are finished, close the reader so the loop can break
            if rx_dirs.is_empty() && counter.finished() {
                rx_dirs.close();
            }
        }

        // collect all the found target files by joining all tasks and return the next stage
        while let Some(res) = join_set.join_next().await {
            self.files.append(&mut res??);
        }

        Ok(DownloadRemoteFiles::new(self))
    }
}

pub struct DownloadRemoteFiles {
    options: Arc<DownloadOptions>,
    pool: Arc<Pool>,
    files: Vec<TargetFile>,
    stats: Arc<ImmediateStats>,
    slots: Arc<StatsSlots<ImmediateFileStats>>,
}

impl DownloadRemoteFiles {
    fn new(stage: EnumerateRemoteFiles) -> Self {
        let max_connections = stage.options.max_connections;

        Self {
            options: stage.options,
            pool: stage.pool,
            files: stage.files,
            stats: stage.stats,
            slots: Arc::new(StatsSlots::new(max_connections)),
        }
    }

    pub fn stats(&self) -> Arc<ImmediateStats> {
        self.stats.clone()
    }

    pub fn slots(&self) -> Arc<StatsSlots<ImmediateFileStats>> {
        self.slots.clone()
    }

    pub async fn download_remote_files(self) -> eyre::Result<Vec<PathBuf>> {
        create_local_dirs(self.files.as_slice()).await?;

        let mut join_set: JoinSet<eyre::Result<PathBuf>> = JoinSet::new();

        for file in self.files {
            let pool = self.pool.clone();
            let stats = self.stats.clone();
            let file_stats_slots = self.slots.clone();

            join_set.spawn(async move {
                let mut ftp = pool.get().await?;

                // use a free slot for a stats object to keep track of the transfer
                let (file_stats, slot) = file_stats_slots
                    .insert(ImmediateFileStats::new(&file.remote, file.size))
                    .await?;

                download_file(stats, file_stats, &mut ftp, &file).await?;

                // free up the stats slot for the next download
                file_stats_slots.release(slot).await?;

                Ok(file.local)
            });
        }

        // collect the downloaded filenames from the subtasks for the next stage
        let mut files = Vec::new();
        while let Some(res) = join_set.join_next().await {
            files.push(res??);
        }

        Ok(files)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct TargetFile {
    pub remote: String,
    pub local: PathBuf,
    pub size: usize,
}

pub async fn get_files_and_dirs(
    ftp: &mut FtpStream,
    remote: &str,
) -> eyre::Result<(Vec<entry_meta::File>, Vec<entry_meta::Directory>)> {
    let (files, rest) = ftp::list(ftp, remote)
        .await?
        .partition::<Vec<Entry>, _>(Entry::is_file);

    let files = files
        .into_iter()
        .filter_map(Entry::file)
        .collect::<Vec<_>>();

    let directories = rest
        .into_iter()
        .filter_map(Entry::directory)
        .collect::<Vec<_>>();

    Ok((files, directories))
}

pub async fn create_local_dirs(files: &[TargetFile]) -> eyre::Result<()> {
    // use a hash set so that each parent is only created once; this will still result in some
    // duplicated work if a nested directory is created before its parent, because create_dir_all
    // will also create all parent directories
    let mut dirs = HashSet::new();
    for file in files {
        let parent = file.local.parent().ok_or_else(|| {
            eyre!(
                "invalid destination {:?} for {:?}: no parent directory",
                file.local,
                file.remote
            )
        })?;

        // paths will be moved to a task, so they can't be borrowed
        dirs.insert(parent.to_path_buf());
    }

    // create the parent directories for each parent
    let mut join_set = JoinSet::new();
    for dir in dirs {
        join_set.spawn(tokio::fs::create_dir_all(dir));
    }

    // handle any fs errors
    while let Some(res) = join_set.join_next().await {
        res??;
    }

    Ok(())
}

pub async fn download_file(
    stats: Arc<ImmediateStats>,
    file_stats: Arc<ImmediateFileStats>,
    ftp: &mut FtpStream,
    file: &TargetFile,
) -> eyre::Result<()> {
    ftp.retr(&file.remote, move |reader| {
        let stats = stats.clone();
        let file_stats = file_stats.clone();

        async {
            let file = tokio::fs::File::create(&file.local)
                .await
                .map_err(FtpError::ConnectionError)?;

            copy(stats, file_stats, reader, file)
                .await
                .map_err(FtpError::ConnectionError)?;

            Ok::<_, FtpError>(())
        }
    })
    .await?;

    Ok(())
}

async fn copy(
    stats: Arc<ImmediateStats>,
    file_stats: Arc<ImmediateFileStats>,
    mut reader: BufReader<DataStream>,
    mut file: tokio::fs::File,
) -> Result<(), tokio::io::Error> {
    loop {
        // read a buffer of data from the remote file
        let buf = reader.fill_buf().await?;
        let bytes = buf.len();

        // the file has been read in its entirety
        if bytes == 0 {
            break;
        }

        // write the buffered data to the local file, in multiple chunks if necessary
        let mut pos = 0;
        while pos < bytes {
            let written = file.write(&buf[pos..]).await?;

            // since the buffer can't be empty here, the write must make progress
            if written == 0 {
                return Err(tokio::io::Error::new(
                    tokio::io::ErrorKind::WriteZero,
                    format!(
                        "attempted to write zero bytes to destination file {}",
                        file_stats.filename()
                    ),
                ));
            }

            stats.tx(written);
            file_stats.tx(written);

            pos += written;
        }

        reader.consume(bytes);
    }

    stats.file_complete();

    Ok(())
}
