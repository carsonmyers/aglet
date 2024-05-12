use std::path::{Path, PathBuf};

use eyre::WrapErr;
use tokio::fs;

use crate::parse;
use crate::unicode::UnicodeVersion;

pub trait ParseFromFile: Sized {
    fn filename(base_path: &Path, version: &UnicodeVersion) -> eyre::Result<PathBuf>;
    fn parse<'a, 'b>(input: &'a str, version: &'b UnicodeVersion) -> parse::Result<'a, Self>;
}

pub trait LoadFromFile: ParseFromFile {
    async fn load(base_path: &Path, version: &UnicodeVersion) -> eyre::Result<Self> {
        let filename = Self::filename(base_path, version)?;
        let data = fs::read_to_string(&filename).await?;

        parse::finish(Self::parse(&data, version)).wrap_err("failed to parse ucd file")
    }
}
