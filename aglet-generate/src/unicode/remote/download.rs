use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use async_ftp::{DataStream, FtpError, FtpStream};
use eyre::{eyre, Report};
use itertools::Itertools;
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::sync::{mpsc, Mutex};
use tokio::task::{JoinHandle, JoinSet};

use crate::unicode::remote::entry_meta::Entry;
use crate::unicode::remote::ftp;

pub struct Download {
    remote: String,
    local: PathBuf,
    error_recv: Option<mpsc::Receiver<Report>>,
    state: Arc<DownloadState>,
}

pub struct DownloadState {
    error_send: Mutex<mpsc::Sender<Report>>,
    pub files: Mutex<Box<[Option<Arc<FileState>>]>>,
    pub errors_occurred: AtomicBool,
    pub total_files: AtomicUsize,
    pub files_transferred: AtomicUsize,
    pub total_bytes: AtomicUsize,
    pub bytes_transferred: AtomicUsize,
}

impl DownloadState {
    fn new(error_send: mpsc::Sender<Report>) -> Self {
        Self {
            error_send: Mutex::new(error_send),
            files: Mutex::new(Box::new([])),
            errors_occurred: Default::default(),
            total_files: Default::default(),
            files_transferred: Default::default(),
            total_bytes: Default::default(),
            bytes_transferred: Default::default(),
        }
    }
    async fn send(&self, report: Report) {
        self.errors_occurred.store(true, Ordering::SeqCst);
        if let Err(err) = self.error_send.lock().await.send(report).await {
            eprintln!("Download error: {}", err.0);
        }
    }

    pub async fn get_file_count(&self) -> usize {
        self.files.lock().await.len()
    }

    fn set_file_count(&self, count: usize) {
        let items = (0..count)
            .map(|_| None)
            .collect::<Vec<Option<Arc<FileState>>>>();

        let mut files = self
            .files
            .try_lock()
            .expect("DownloadState filenames is already locked");

        *files = items.into_boxed_slice();
    }

    pub async fn get_file(&self, index: usize) -> Option<Arc<FileState>> {
        let files = self.files.lock().await;
        let entry = files.get(index);
        let Some(entry) = entry else {
            panic!("get_file failed: index {} into {:?}", index, &self.files);
        };

        match entry {
            Some(entry) => Some(entry.clone()),
            None => None,
        }
    }

    async fn set_file(&self, index: usize, file: Option<Arc<FileState>>) {
        let mut files = self.files.lock().await;
        let entry = files.get_mut(index);
        let Some(entry) = entry else {
            panic!("set_file failed: index {} into {:?}", index, &files);
        };

        *entry = file
    }
}

#[derive(Debug)]
pub struct FileState {
    pub filename: String,
    pub total_bytes: AtomicUsize,
    pub bytes_transferred: AtomicUsize,
}

impl FileState {
    fn new(file: &TargetFile) -> Self {
        Self {
            filename: file.remote.clone(),
            total_bytes: AtomicUsize::new(file.size),
            bytes_transferred: AtomicUsize::new(0),
        }
    }
}

#[derive(Clone, Debug)]
struct TargetFile {
    remote: String,
    local: PathBuf,
    size: usize,
}

impl Download {
    pub fn new<P: Into<PathBuf>, S: Into<String>>(remote: S, local: P) -> Self {
        let (tx, rx) = mpsc::channel(32);

        Self {
            remote: remote.into(),
            local: local.into(),
            error_recv: Some(rx),
            state: Arc::new(DownloadState::new(tx)),
        }
    }

    pub fn stats(&self) -> Arc<DownloadState> {
        self.state.clone()
    }

    pub fn start(
        mut self,
        max_connections: usize,
    ) -> (JoinHandle<eyre::Result<()>>, mpsc::Receiver<Report>) {
        self.state.set_file_count(max_connections);

        let remote = self.remote.clone();
        let local = self.local.clone();
        let state = self.state.clone();

        let handle = tokio::spawn(async move {
            let mut ftp = ftp::login().await?;

            // ignore errors from this stage; they won't be downloaded and the presence of an error
            // in the next stage is more important
            let files = gather_files(state.clone(), &mut ftp, &remote, &local).await;
            state.errors_occurred.store(false, Ordering::SeqCst);

            create_local_dirs(state.clone(), &files).await;
            if state.errors_occurred.load(Ordering::SeqCst) {
                return Err(eyre!(
                    "Aborted download due to errors creating the destination folders"
                ));
            }

            download_files_in_groups(state.clone(), files, max_connections).await;
            if state.errors_occurred.load(Ordering::SeqCst) {
                return Err(eyre!(
                    "Errors occurred during download: UCD may be incomplete"
                ));
            }

            Ok(())
        });

        let recv = self.error_recv.take().unwrap();

        (handle, recv)
    }
}

macro_rules! collect_errs {
    ($state:ident, $set:ident) => {
        while let Some(res) = $set.join_next().await {
            let res: eyre::Result<()> = match res {
                Ok(Err(err)) => Err(err.into()),
                Err(err) => Err(err.into()),
                _ => Ok(()),
            };

            if let Err(err) = res {
                $state.send(err).await;
            }
        }
    };
}

macro_rules! bail_err {
    ($state:ident, $res:ident, $bail:block) => {
        match $res {
            Err(err) => {
                $state.send(err).await;
                $bail
            },
            Ok(val) => val,
        }
    };
    ($state:ident, $res:ident) => {
        bail_err!($state, $res, { return })
    };
}

async fn gather_files(
    state: Arc<DownloadState>,
    ftp: &mut FtpStream,
    remote: &str,
    local: &Path,
) -> Vec<TargetFile> {
    let mut directories = VecDeque::from([vec![]]);
    let mut files = Vec::new();

    while let Some(prefix) = directories.pop_front() {
        let state = state.clone();
        let remote = format!("{}/{}", remote, prefix.join("/"));
        let local = local.join(prefix.iter().collect::<PathBuf>());

        let files_and_dirs = get_files_and_dirs(state.clone(), ftp, &remote, &local).await;
        let (mut new_files, new_directories) = bail_err!(state, files_and_dirs, {
            continue;
        });

        files.append(&mut new_files);
        directories.extend(new_directories.into_iter().map(|dir| {
            let mut new_prefix = prefix.clone();
            new_prefix.push(dir);
            new_prefix
        }));
    }

    files
}

async fn get_files_and_dirs(
    state: Arc<DownloadState>,
    ftp: &mut FtpStream,
    remote: &str,
    local: &Path,
) -> eyre::Result<(Vec<TargetFile>, Vec<String>)> {
    let (files, rest) = ftp::list(ftp, remote)
        .await?
        .partition::<Vec<Entry>, _>(Entry::is_file);

    let files = files
        .into_iter()
        .filter_map(Entry::file)
        .filter(|file| !matches!(file.name.rsplit_once('.'), Some((_, "zip" | "pdf"))))
        .map(|file| {
            state.total_bytes.fetch_add(file.size, Ordering::Relaxed);

            TargetFile {
                remote: format!("{}/{}", remote, &file.name),
                local: local.join(&file.name),
                size: file.size,
            }
        })
        .collect::<Vec<_>>();

    state.total_files.fetch_add(files.len(), Ordering::Relaxed);

    let directories = rest
        .into_iter()
        .filter_map(Entry::directory)
        .map(|dir| dir.name)
        .collect::<Vec<_>>();

    Ok((files, directories))
}

async fn create_local_dirs(state: Arc<DownloadState>, files: &[TargetFile]) {
    let mut dirs = HashSet::new();
    for file in files {
        let parent = file.local.parent().ok_or_else(|| {
            eyre!(
                "invalid destination {:?} for {:?}: no parent directory",
                file.local,
                file.remote
            )
        });

        let parent = bail_err!(state, parent);

        dirs.insert(parent.to_path_buf());
    }

    let mut set = JoinSet::new();

    for dir in dirs {
        set.spawn(tokio::fs::create_dir_all(dir));
    }

    collect_errs!(state, set);
}

async fn download_files_in_groups(
    state: Arc<DownloadState>,
    files: Vec<TargetFile>,
    max_connections: usize,
) {
    if files.is_empty() || max_connections == 0 {
        return;
    }

    let mut chunk_sizes = vec![0usize; max_connections];
    let mut chunks = vec![vec![]; max_connections];
    for file in files {
        let smallest = chunk_sizes.iter().position_min().unwrap();
        chunk_sizes[smallest] += file.size;
        chunks[smallest].push(file);
    }

    let mut set = JoinSet::new();

    for (conn_idx, chunk) in chunks.into_iter().enumerate() {
        let chunk = chunk.to_vec();
        set.spawn(download_files(state.clone(), chunk, conn_idx));
    }

    collect_errs!(state, set);
}

async fn download_files(
    state: Arc<DownloadState>,
    files: Vec<TargetFile>,
    conn_idx: usize,
) -> eyre::Result<()> {
    let mut ftp = ftp::login().await?;

    for file in files {
        let file_state = Arc::new(FileState::new(&file));
        state.set_file(conn_idx, Some(file_state.clone())).await;

        download_file(state.clone(), file_state, &mut ftp, &file).await;

        state.set_file(conn_idx, None).await;
    }

    Ok(())
}

async fn download_file(
    state: Arc<DownloadState>,
    file_state: Arc<FileState>,
    ftp: &mut FtpStream,
    paths: &TargetFile,
) {
    let res = {
        let state = state.as_ref();
        let file_state = file_state.as_ref();
        ftp.retr(&paths.remote, move |reader| async {
            let file = tokio::fs::File::create(&paths.local)
                .await
                .map_err(FtpError::ConnectionError)?;

            copy(state, file_state, reader, file)
                .await
                .map_err(FtpError::ConnectionError)?;

            Ok::<_, FtpError>(())
        })
        .await
        .map_err(|err| err.into())
    };

    bail_err!(state, res);
}

async fn copy(
    state: &DownloadState,
    file_state: &FileState,
    mut reader: BufReader<DataStream>,
    mut file: tokio::fs::File,
) -> Result<(), tokio::io::Error> {
    let mut buf = Vec::new();

    loop {
        let amt = reader.read_buf(&mut buf).await?;
        if amt == 0 {
            break;
        }

        let mut pos = 0;
        while pos < amt {
            let written = file.write(&buf[pos..amt]).await?;
            if written == 0 {
                return Err(tokio::io::Error::new(
                    tokio::io::ErrorKind::WriteZero,
                    "write zero bytes into writer",
                ));
            }

            pos += written;
            state.bytes_transferred.fetch_add(written, Ordering::SeqCst);
            file_state
                .bytes_transferred
                .fetch_add(written, Ordering::SeqCst);
        }
    }

    state.files_transferred.fetch_add(1, Ordering::SeqCst);
    Ok(())
}
