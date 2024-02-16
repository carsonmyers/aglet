use std::str::FromStr;

use eyre::{eyre, Result, WrapErr};
use suppaftp::list::File;
use suppaftp::AsyncFtpStream;
use tokio::task::JoinSet;

use super::UnicodeVersion;

pub async fn list_versions() -> Result<Vec<UnicodeVersion>> {
    let mut ftp = login().await?;
    let mut versions = get_present_versions(&mut ftp).await?;

    if versions.is_empty() {
        return Err(eyre!("no unicode versions found"));
    }

    let mut latest_version: Option<usize> = None;
    for (i, version) in versions.iter().enumerate().rev() {
        if has_ucd(version, &mut ftp).await? {
            latest_version = Some(i);
            break;
        }
    }

    match latest_version {
        Some(i) => {
            versions.resize(i + 1, Default::default());
            Ok(versions)
        },
        None => Err(eyre!("no unicode version has a unicode database")),
    }
}

async fn login() -> Result<AsyncFtpStream> {
    let mut ftp = AsyncFtpStream::connect("www.unicode.org:21")
        .await
        .wrap_err("error connecting to unicode.org")?;

    ftp.login("anonymous", "anonymous@example.com").await?;

    Ok(ftp)
}

async fn get_present_versions(ftp: &mut AsyncFtpStream) -> Result<Vec<UnicodeVersion>> {
    let mut versions = ftp
        .list(Some("/Public"))
        .await?
        .into_iter()
        .map(|line| File::try_from(line.as_str()))
        .collect::<Result<Vec<File>, _>>()?
        .into_iter()
        .filter_map(|file| {
            if !file.is_directory() {
                None
            } else {
                UnicodeVersion::from_str(file.name()).ok()
            }
        })
        .collect::<Vec<_>>();

    versions.sort();

    Ok(versions)
}
async fn has_ucd(version: &UnicodeVersion, ftp: &mut AsyncFtpStream) -> Result<bool> {
    let path = format!("{}/ucd", version.remote_dir());
    let res = ftp
        .list(Some(&path))
        .await?
        .into_iter()
        .map(|line| {
            File::try_from(line.as_str())
                .map(|file| file.name() == &version.unicode_data_filename())
        })
        .any(|maybe_file| matches!(maybe_file, Ok(true)));

    Ok(res)
}
