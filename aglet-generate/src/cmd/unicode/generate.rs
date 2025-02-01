use std::path::{Path, PathBuf};

use clap::Args;
use eyre::eyre;

use crate::cache::{Cache, StoredVersion};
use crate::cmd::unicode::fetch;
use crate::cmd::unicode::CommonArgs;
use crate::unicode::ucd::LoadFromFile;
use crate::unicode::UnicodeVersion;
use crate::unicode::{ucd, SelectVersion};

#[derive(Args, Debug)]
pub struct GenerateArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(long = "crate", default_value = "aglet-unicode")]
    pub target_crate: String,

    #[arg(long = "download-missing", short)]
    pub download_missing: bool,
}

pub async fn run(args: GenerateArgs, cache: &mut Cache) -> eyre::Result<()> {
    let version_or_latest = args.common.version.clone().unwrap_or(SelectVersion::Latest);
    if !cache.has_version_or_default(args.common.version.as_ref()) {
        let select_version = args.common.version.clone().unwrap_or(SelectVersion::Latest);
        download_missing(&args, select_version, None, cache).await?;
    }

    let version = cache
        .version_or_default(&args.common.version)
        .expect("version should exist in cache")
        .version;

    // Generate data:
    // - Catalog Properties:
    //   - Block (Blocks.txt)
    //   - Script (Scripts.txt)
    // - Enumerated Properties:
    //   - General Category (UnicodeData.txt)
    // - Binary Properties:
    //   - All in DerivedCoreProperties.txt
    //   - All in PropList.txt
    //   - All in emoji-data.txt
    // - String Properties:
    //   - Uppercase_Mapping (SpecialCasing.txt)
    //   - Lowercase_Mapping (SpecialCasing.txt)
    //   - Titlecase_Mapping (SpecialCasing.txt)
    //   - Simple_Uppercase_Mapping (UnicodeData.txt)
    //   - Simple_Lowercase_Mapping (UnicodeData.txt)
    //   - Simple_Titlecase_Mapping (UnicodeData.txt)
    //   - Simple_Case_Folding (CaseFolding.txt)
    //   - Case_Folding (CaseFolding.txt)
    //   - NFKC_Casefold (DerivedNormalizationProps.txt)
    //   - NFKC_Simple_Casefold (DerivedNormalizationProps.txt)
    // - Miscellaneous Properties:
    //   - Script Extension (ScriptExtensions.txt)
    // - Property Metadata:
    //   - Property Names (PropertyAliases.txt)
    //   - Property Name Aliases (PropertyAliases.txt)
    //   - Property Values (PropertyValueAliases.txt)
    //   - Property Value Aliases (PropertyValueAliases.txt)

    // Property names, values, and aliases:
    let property_names = load_data::<ucd::PropertyNames>(&args, version, cache).await?;
    let property_values = load_data::<ucd::PropertyValues>(&args, version, cache).await?;

    // UnicodeData.txt:
    // - General Category (enumerated)
    // - Simple_Uppercase_Mapping (string)
    // - Simple_Lowercase_Mapping (string)
    // - Simple_Titlecase_Mapping (string)
    let unicode_data = load_data::<ucd::UnicodeData>(&args, version, cache).await?;

    // CaseFolding.txt:
    // - Simple_Case_Folding (string)
    // - Case_Folding (string)
    let case_folding = load_data::<ucd::CaseFolding>(&args, version, cache).await?;

    // SpecialCasing.txt:
    // - Uppercase_Mapping (string)
    // - Lowercase_Mapping (string)
    // - Titlecase_Mapping (string)
    let special_casing = load_data::<ucd::SpecialCasing>(&args, version, cache).await?;

    Ok(())
}

async fn load_data<T: LoadFromFile>(
    args: &GenerateArgs,
    version: UnicodeVersion,
    cache: &mut Cache,
) -> eyre::Result<T> {
    let (source_version, filepath) = T::filename(version)?;
    if !cache.has_version(&source_version.into()) {
        download_missing(args, source_version.into(), Some(&filepath), cache).await?;
    }

    let stored_version = cache
        .version(&source_version.into())
        .expect("downloaded version should exist");

    let base_path = cache.version_path(stored_version)?;
    let filename = base_path.join(filepath);

    T::load(filename, source_version).await
}

async fn download_missing<'c, 'v>(
    args: &GenerateArgs,
    version: SelectVersion,
    filepath: Option<&Path>,
    cache: &mut Cache,
) -> eyre::Result<()> {
    if !args.download_missing {
        return if let Some(filepath) = filepath {
            Err(eyre!("data file {} requires unicode version {}, but it is not downloaded. Use --download-missing to automatically fetch required data", filepath.display(), version))
        } else {
            Err(eyre!("unicode version {} is not downloaded. Use --download-missing to automatically fetch missing database versions", version))
        };
    }

    let fetch_args = fetch::FetchArgs {
        common: CommonArgs {
            version: Some(version.clone()),
            ..args.common
        },
        dry_run: false,
    };

    fetch::run(fetch_args, cache).await?;
    Ok(())
}
