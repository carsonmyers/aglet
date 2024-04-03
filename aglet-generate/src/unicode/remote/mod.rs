mod download;
mod entry_meta;
pub mod ftp;

use std::ffi::OsStr;
use std::path::PathBuf;
use std::str::FromStr;

use async_ftp::FtpStream;
use eyre::{eyre, OptionExt};

use crate::unicode::remote::entry_meta::Entry;
use crate::unicode::UnicodeVersion;
pub use download::{Download, DownloadState};

pub async fn list_versions(ftp: &mut FtpStream) -> eyre::Result<Vec<UnicodeVersion>> {
    let mut versions = ftp::list(ftp, "/Public")
        .await?
        .filter_map(Entry::directory)
        .filter_map(|dir| UnicodeVersion::from_str(&dir.name).ok())
        .collect::<Vec<_>>();

    versions.sort();

    Ok(versions)
}

pub async fn get_latest_version(ftp: &mut FtpStream) -> eyre::Result<UnicodeVersion> {
    let latest_link = ftp::list(ftp, "/Public/UCD")
        .await?
        .find(|entry| entry.name() == "latest")
        .ok_or_eyre("could not find latest version: /Public/UCD/latest not found")
        .map(Entry::link)?
        .ok_or_eyre("could not find latest version: /Public/UCD/latest is not a link")?;

    let latest = PathBuf::from(latest_link.target)
        .file_name()
        .and_then(OsStr::to_str)
        .ok_or_eyre("could not find latest version: /Public/UCD/latest doesn't link to a version")
        .and_then(UnicodeVersion::from_str)?;

    Ok(latest)
}

pub fn get_draft_version(
    versions: &[UnicodeVersion],
    latest: &UnicodeVersion,
) -> eyre::Result<Option<UnicodeVersion>> {
    let mut after_latest = versions
        .into_iter()
        .filter(|&ver| ver > latest)
        .collect::<Vec<_>>();

    if after_latest.len() > 1 {
        return Err(eyre!("multiple draft versions detected"));
    }

    Ok(after_latest.pop().cloned())
}
