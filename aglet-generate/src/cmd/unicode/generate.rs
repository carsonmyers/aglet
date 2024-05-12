use clap::Args;
use eyre::eyre;
use tracing::{warn, warn_span};

use crate::cache::Cache;
use crate::cmd::unicode::CommonArgs;
use crate::unicode::ucd;
use crate::unicode::ucd::LoadFromFile;

#[derive(Args, Debug)]
pub struct GenerateArgs {
    #[command(flatten)]
    pub common: CommonArgs,

    #[arg(long = "crate", default_value = "aglet-unicode")]
    target_crate: String,
}

pub async fn run(args: GenerateArgs, cache: &mut Cache) -> eyre::Result<()> {
    let version = cache.version_or_default(&args.common.version)?;

    let Some(version) = version else {
        if cache.metadata.stored_versions.is_empty() {
            return Err(eyre!("no versions of the unicode database have been downloaded; use `ag-gen unicode fetch` to get the latest version"));
        }

        return Err(eyre!(
            "no valid versions are cached; use `ag-gen unicode fetch` to get an up-to-date version"
        ));
    };

    let base_path = cache.version_path(version)?;

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
    let property_names = ucd::PropertyNames::load(&base_path, &version.version).await?;
    let property_values = ucd::PropertyValues::load(&base_path, &version.version).await?;

    // UnicodeData.txt:
    // - General Category (enumerated)
    // - Simple_Uppercase_Mapping (string)
    // - Simple_Lowercase_Mapping (string)
    // - Simple_Titlecase_Mapping (string)
    let unicode_data = ucd::UnicodeData::load(&base_path, &version.version).await?;

    Ok(())
}
