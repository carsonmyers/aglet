use std::str::FromStr;
use std::sync::atomic::Ordering;

use chrono::Utc;
use clap::Args;
use console::style;
use eyre::eyre;
use indicatif::{HumanBytes, MultiProgress, ProgressBar, ProgressStyle};
use tokio::task::AbortHandle;
use tokio::{fs, time};

use crate::unicode::{cache, remote, CommonArgs, UnicodeVersion};

#[derive(Args, Debug)]
pub struct FetchArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(value_parser = parse_select_version)]
    version: SelectVersion,
}

#[derive(Debug, Clone)]
pub enum SelectVersion {
    Latest,
    Draft,
    Version(UnicodeVersion),
}

fn parse_select_version(arg: &str) -> eyre::Result<SelectVersion> {
    match arg.to_lowercase().as_str() {
        "latest" => Ok(SelectVersion::Latest),
        "draft" => Ok(SelectVersion::Draft),
        value => Ok(SelectVersion::Version(UnicodeVersion::from_str(value)?)),
    }
}

pub async fn run(args: FetchArgs, cache: &mut cache::Cache) -> eyre::Result<()> {
    let started_at = Utc::now();

    let spin_login = spin("Login...");
    let mut ftp = remote::ftp::login().await?;
    spin_login.abort();

    let spin_versions = spin("Check version...");

    let remote_versions = remote::list_versions(&mut ftp).await?;
    let latest_version = remote::get_latest_version(&mut ftp).await?;
    let draft_version = remote::get_draft_version(&remote_versions, &latest_version)?;

    let version = match args.version {
        SelectVersion::Latest => latest_version,
        SelectVersion::Draft => match draft_version {
            Some(draft_version) => draft_version,
            None => return Err(eyre!("no draft version found")),
        },
        SelectVersion::Version(version) => version,
    };

    if draft_version.is_some_and(|draft_version| version == draft_version) {
        return Err(eyre!(
            "downloading a UCD draft version is not yet supported"
        ));
    }

    let version_exists = remote_versions
        .iter()
        .any(|remote_version| remote_version == &version);
    if !version_exists {
        return Err(eyre!("Version {:?} does not exist", version));
    }

    spin_versions.abort();

    let spin_temp_dir = spin("Create temp dir...");
    let tmp_dir = cache.get_tmp_dir().await?;
    spin_temp_dir.abort();

    let download = remote::Download::new(version.remote_dir(), &tmp_dir);
    let stats = download.stats();

    let (download, mut error_recv) = download.start(5);

    let stats_handle = tokio::spawn(async move {
        eprintln!(
            "{}",
            style(format!(
                "\u{2713} fetching UCD version {}",
                style(version.to_string()).bold()
            ))
            .green()
        );

        let fetch_spin = ProgressBar::new_spinner();
        loop {
            time::sleep(time::Duration::from_millis(100)).await;
            fetch_spin.set_message(format!(
                "gather files... {} files\t{}",
                stats.total_files.load(Ordering::Relaxed),
                HumanBytes(stats.total_bytes.load(Ordering::Relaxed) as u64),
            ));
            fetch_spin.tick();

            if stats.bytes_transferred.load(Ordering::SeqCst) > 0 {
                break;
            }
        }
        fetch_spin.finish_and_clear();

        let group = MultiProgress::new();
        let spin = group.add(ProgressBar::new_spinner());
        let pb = group.add(
            ProgressBar::new(stats.total_bytes.load(Ordering::Relaxed) as u64).with_style(
                ProgressStyle::with_template(
                    "[{elapsed_precise}] {wide_bar:.cyan/blue} {percent_precise}%",
                )
                .unwrap(),
            ),
        );

        let file_spins = (0..stats.get_file_count().await)
            .map(|i| {
                group.add(
                    ProgressBar::new_spinner()
                        .with_style(
                            ProgressStyle::with_template(
                                "{spinner} [{prefix}] {wide_msg} {bytes}/{total_bytes} {percent}%",
                            )
                            .unwrap(),
                        )
                        .with_prefix((i + 1).to_string()),
                )
            })
            .collect::<Vec<_>>();

        let total_bytes = stats.total_bytes.load(Ordering::Relaxed);
        let total_files = stats.total_files.load(Ordering::Relaxed);
        let mut last_tx = 0;
        let mut samples = [0usize; 16];
        let mut sample_idx = 0;
        loop {
            time::sleep(time::Duration::from_millis(100)).await;
            let files = stats.files_transferred.load(Ordering::SeqCst);
            let tx = stats.bytes_transferred.load(Ordering::SeqCst);
            let files_remaining = total_files - files;
            let remaining = total_bytes.checked_sub(tx).unwrap_or_default();
            let delta = tx - last_tx;
            last_tx = tx;

            samples[sample_idx] = delta;
            sample_idx = (sample_idx + 1) % samples.len();
            let avg_delta = samples.iter().sum::<usize>() as f64 / samples.len() as f64;
            let per_second = (avg_delta * 10f64) as u64;

            spin.set_message(format!(
                "{}/{} files ({} remaining)\t{}/{} ({} remaining)\t{}/s",
                files,
                total_files,
                files_remaining,
                HumanBytes(tx as u64),
                HumanBytes(total_bytes as u64),
                HumanBytes(remaining as u64),
                HumanBytes(per_second),
            ));
            spin.tick();
            pb.set_position(stats.bytes_transferred.load(Ordering::Relaxed) as u64);

            for (idx, spin) in file_spins.iter().enumerate() {
                let file = stats.get_file(idx).await;
                if let Some(file) = file {
                    spin.set_message(file.filename.clone());
                    spin.set_length(file.total_bytes.load(Ordering::SeqCst) as u64);
                    spin.set_position(file.bytes_transferred.load(Ordering::SeqCst) as u64);
                } else {
                    spin.set_message("".to_string());
                    spin.set_length(0);
                    spin.set_position(0);
                }
            }
        }
    });
    let errors_handle = tokio::spawn(async move {
        while let Some(err) = error_recv.recv().await {
            eprintln!("\t ! {}", err);
        }
    });

    let encountered_err = if let Err(err) = download.await? {
        eprintln!("{}", err);
        true
    } else {
        false
    };

    stats_handle.abort();
    errors_handle.abort();

    if !encountered_err {
        let duration = Utc::now() - started_at;
        let minutes = duration.num_minutes();
        let seconds = duration.num_seconds() - minutes;
        eprintln!(
            "{}",
            style(format!("\u{2713} downloaded in {}:{}", minutes, seconds)).green()
        );

        let spin_intern = spin("Adding version to database...");
        cache.intern_version(version, None, &tmp_dir).await?;
        spin_intern.abort();
        eprintln!("{}", style("\u{2713} done").green());
    } else {
        fs::remove_dir(tmp_dir).await?;
    }

    Ok(())
}

fn spin<S: AsRef<str>>(message: S) -> AbortHandle {
    let message = message.as_ref().to_string();

    let handle = tokio::spawn(async move {
        let pb = ProgressBar::new_spinner();
        pb.set_message(message);
        loop {
            time::sleep(time::Duration::from_millis(100)).await;
            pb.tick();
        }
    });

    handle.abort_handle()
}
