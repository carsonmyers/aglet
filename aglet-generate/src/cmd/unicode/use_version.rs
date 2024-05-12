use console::style;

use crate::cache::Cache;
use crate::cmd::unicode::CommonArgs;

pub async fn run(args: CommonArgs, cache: &mut Cache) -> eyre::Result<()> {
    let mut candidate_versions = cache
        .metadata
        .stored_versions
        .iter()
        .filter(|stored_version| stored_version.selected_by(&args.version))
        .collect::<Vec<_>>();

    let target_version = candidate_versions.pop();
    let Some(target_version) = target_version else {
        let text = format!(
            "No stored versions match {0}\nuse `ag-gen unicode fetch {0}` to download it",
            args.version,
        );

        eprintln!("{}", style(text).bright().red());

        return Ok(());
    };

    let select = if args.version.is_version() {
        format!("{}", args.version)
    } else {
        format!("{} ({})", args.version, target_version.version)
    };

    let version_line = style(format!("   {}", target_version));
    let version_line = if target_version.is_expired() && target_version.is_current() {
        format!("{}", version_line.bright().red())
    } else if target_version.is_current() {
        format!("{}", version_line.bright().green())
    } else {
        format!("{}", version_line.bright().black())
    };

    println!("Use {}:\n{}", select, version_line);

    cache.metadata.use_version = Some(args.version);

    Ok(())
}
