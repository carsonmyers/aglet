use std::path::{Path, PathBuf};

use eyre::WrapErr;
use tokio::fs;

use crate::parse;
use crate::unicode::UnicodeVersion;

pub trait ParseFromFile: Sized {
    fn filename(version: UnicodeVersion) -> eyre::Result<(UnicodeVersion, PathBuf)>;
    fn parse(input: &str, version: UnicodeVersion) -> parse::Result<Self>;
}

pub trait LoadFromFile: ParseFromFile {
    async fn load(filename: PathBuf, version: UnicodeVersion) -> eyre::Result<Self> {
        let data = fs::read_to_string(&filename).await?;

        parse::finish(Self::parse(&data, version))
            .wrap_err_with(|| format!("failed to parse ucd file {}", filename.display()))
    }
}
