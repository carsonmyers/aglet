use std::str::FromStr;

use async_ftp::FtpStream;
use eyre::WrapErr;

use super::entry_meta::Entry;

pub async fn login() -> eyre::Result<FtpStream> {
    let mut ftp = FtpStream::connect("www.unicode.org:21")
        .await
        .wrap_err("error connecting to unicode.org")?;

    ftp.login("anonymous", "anonymous@example.com").await?;

    Ok(ftp)
}

pub struct FtpList {
    lines: Vec<String>,
}

impl Iterator for FtpList {
    type Item = Entry;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(line) = self.lines.pop() {
            if let Ok(entry) = Entry::from_str(&line) {
                return Some(entry);
            }

            eprintln!("unsupported listing format for line: {:?}", line);
        }

        None
    }
}

pub async fn list(ftp: &mut FtpStream, dir: &str) -> eyre::Result<FtpList> {
    let lines = ftp.list(Some(dir)).await?;

    Ok(FtpList { lines })
}
