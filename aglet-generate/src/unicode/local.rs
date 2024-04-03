use std::fs::read_dir;
use std::io::ErrorKind;
use std::path::Path;
use std::str::FromStr;

use eyre::{Result, WrapErr};
use itertools::Itertools;

use super::UnicodeVersion;

pub fn list_versions<P: AsRef<Path>>(data_dir: P) -> Result<Vec<UnicodeVersion>> {
    let entries = match read_dir(data_dir) {
        Err(err) => {
            return match err.kind() {
                ErrorKind::NotFound => Ok(vec![]),
                _ => Err(err).wrap_err("error reading unicode data directory"),
            };
        },
        Ok(entries) => entries,
    };

    let mut versions = entries
        .filter_map_ok(|entry| {
            if !entry.metadata().ok()?.is_dir() {
                None
            } else {
                UnicodeVersion::from_str(entry.file_name().to_str()?).ok()
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    versions.sort();
    Ok(versions)
}

fn has_ucd<P: AsRef<Path>>(data_dir: P, version: &UnicodeVersion) -> Result<bool> {
    let mut entries = read_dir(data_dir)?;

    let ucd = entries.find(|entry_res| {
        let Ok(entry) = entry_res else {
            return false;
        };

        let Ok(metadata) = entry.metadata() else {
            return false;
        };

        if metadata.is_dir() {
            return false;
        };

        let Some(file_name) = entry.file_name().to_str() else {
            return false;
        };

        todo!();
    });

    todo!();
}
