use std::str::FromStr;
use std::sync::Arc;

use async_ftp::FtpStream;
use tokio::sync::mpsc;
use tokio::task::JoinSet;
use tracing::info;

use crate::ftp::list_files::{entry, ListOptions};
use crate::ftp::{new_pool, Pool, RecursiveFiles, RemoteFile};
use crate::progress::stats::ImmediateStats;
use crate::task::TaskCounter;

pub struct ListFiles {
    options: Arc<ListOptions>,
    remote:  Arc<String>,
    pool:    Arc<Pool>,
    stats:   Arc<ImmediateStats>,
}

impl ListFiles {
    pub fn new<S: AsRef<str>>(remote: S, options: ListOptions) -> Self {
        let pool = options.get_pool().unwrap_or_else(|| Arc::new(new_pool(1)));
        let remote = remote.as_ref().trim_end_matches('/').to_string();

        Self {
            options: Arc::new(options),
            remote: Arc::new(remote.into()),
            pool,
            stats: Arc::new(ImmediateStats::new()),
        }
    }

    pub fn stats(&self) -> Arc<ImmediateStats> {
        self.stats.clone()
    }

    pub async fn list_files(self) -> eyre::Result<RecursiveFiles> {
        let (tx_dirs, mut rx_dirs) = mpsc::channel::<String>(1024);
        tx_dirs.send(self.remote.to_string()).await?;

        let mut join_set: JoinSet<eyre::Result<Vec<RemoteFile>>> = JoinSet::new();

        let counter = TaskCounter::new();
        let mut directory_count = 0usize;

        loop {
            // keep reading new dirs through the channel until the reader is closed or the counter
            // notifies that all tasks have finished (so no new dirs will be produced)
            let prefix = tokio::select! { biased;
                _ = counter.wait_finish() => if rx_dirs.is_empty() {
                    info!("finish enumeration: no more threads or prefixes");
                    break
                } else {
                    info!("no threads running, but receiver is not empty: continue");
                    continue
                },
                Some(prefix) = rx_dirs.recv() => {
                    info!("rx prefix {:?} [{} msgs]", &prefix, rx_dirs.len());
                    directory_count += 1;
                    prefix
                },
                else => {
                    info!("finish enumeration: receiver is closed");
                    break
                },
            };

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
                let (new_files, new_dirs) = get_files_and_dirs(&mut ftp, prefix.as_str()).await?;

                // send new dirs to the parent task so that it will spawn a new task for each
                for new_dir in new_dirs {
                    tx_dirs
                        .send(format!("{}/{}", prefix, &new_dir.name))
                        .await
                        .expect("can send dir");
                }

                // filter files with excluded extensions and map them into target files
                let new_files = new_files
                    .into_iter()
                    .filter(|file| match file.name.rsplit_once('.') {
                        Some((_, ext)) => !options.ext_excluded(ext),
                        None => true,
                    })
                    .map(|file| RemoteFile {
                        path: format!("{}/{}", prefix, &file.name),
                        size: file.size,
                    })
                    .inspect(|file| {
                        info!("add file {}", &file.path);
                        stats.add_file(file.size)
                    })
                    .collect::<Vec<_>>();

                // resolve to the new files found at this prefix; the prefix is also included
                // so that the local and remote paths can be constructed correctly
                Ok(new_files)
            });

            // when there all tasks are finished, close the reader so the loop can break
            if rx_dirs.is_empty() && counter.finished() {
                info!("close receiver: no tasks running and receiver is empty");
                rx_dirs.close();
            }
        }

        // collect all the found target files by joining all tasks and return the next stage
        let mut files = Vec::new();
        while let Some(res) = join_set.join_next().await {
            files.append(&mut res??);
        }

        info!(
            "gathered {} files from {} directories",
            files.len(),
            directory_count
        );

        RecursiveFiles::new(self.remote.as_ref(), files)
    }
}

async fn get_files_and_dirs(
    ftp: &mut FtpStream,
    remote: &str,
) -> eyre::Result<(Vec<entry::File>, Vec<entry::Directory>)> {
    let entries = ftp
        .list(Some(remote))
        .await?
        .into_iter()
        .map(|line| entry::Entry::from_str(line.as_str()))
        .collect::<Result<Vec<_>, _>>()?;

    let mut files = Vec::new();
    let mut directories = Vec::new();
    for entry in entries {
        match entry {
            entry::Entry::File(file) => files.push(file),
            entry::Entry::Directory(directory) => directories.push(directory),
            _ => (),
        }
    }

    Ok((files, directories))
}
