use std::env::current_dir;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_ftp::{DataStream, FtpError, FtpStream};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::task::JoinSet;

use crate::ftp::download::DownloadOptions;
use crate::ftp::{new_pool, Pool, RecursiveFiles};
use crate::progress::stats::{ImmediateFileStats, ImmediateStats, StatsSlots};

pub struct Download {
    files: RecursiveFiles,
    local: Option<PathBuf>,
    pool:  Arc<Pool>,
    stats: Arc<ImmediateStats>,
    slots: Arc<StatsSlots<ImmediateFileStats>>,
}

impl Download {
    pub fn new(files: RecursiveFiles, options: DownloadOptions) -> Self {
        let pool = options.get_pool().unwrap_or_else(|| Arc::new(new_pool(1)));
        let local = options.take_local();
        let num_slots = pool.status().max_size;

        Self {
            files,
            local,
            pool,
            stats: Arc::new(ImmediateStats::new()),
            slots: Arc::new(StatsSlots::new(num_slots)),
        }
    }

    pub fn stats(&self) -> Arc<ImmediateStats> {
        self.stats.clone()
    }

    pub fn slots(&self) -> Arc<StatsSlots<ImmediateFileStats>> {
        self.slots.clone()
    }

    pub async fn download(self) -> eyre::Result<Vec<PathBuf>> {
        let mut join_set: JoinSet<eyre::Result<PathBuf>> = JoinSet::new();

        let local = match self.local {
            Some(local) => local,
            None => current_dir()?,
        };

        for file in self.files {
            let parent = local.join(file.relative_parent());
            let local = local.join(file.relative_path());
            let file = file.take();

            let pool = self.pool.clone();
            let stats = self.stats.clone();
            let slots = self.slots.clone();

            join_set.spawn(async move {
                tokio::fs::create_dir_all(parent).await?;

                let mut ftp = pool.get().await?;

                let (file_stats, slot) = slots
                    .insert(ImmediateFileStats::new(&file.path, file.size))
                    .await?;

                // download the file and release the slot regardless of whether the download succeeded
                let dl_res = download_file(stats, file_stats, &mut ftp, &file.path, &local).await;
                let rl_res = slots.release(slot).await;

                // preferentially return the download error
                dl_res.and(rl_res)?;

                Ok(local)
            });
        }
        Ok(vec![])
    }
}

pub async fn download_file(
    stats: Arc<ImmediateStats>,
    file_stats: Arc<ImmediateFileStats>,
    ftp: &mut FtpStream,
    remote: &str,
    local: &Path,
) -> eyre::Result<()> {
    ftp.retr(remote.as_ref(), move |reader| {
        let stats = stats.clone();
        let file_stats = file_stats.clone();
        let local = local.to_path_buf();

        let res = async {
            let file = tokio::fs::File::create(local)
                .await
                .map_err(FtpError::ConnectionError)?;

            copy(stats, file_stats, reader, file)
                .await
                .map_err(FtpError::ConnectionError)?;

            Ok::<_, FtpError>(())
        };

        res
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
