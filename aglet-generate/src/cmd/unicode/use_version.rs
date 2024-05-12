use console::style;

use crate::cache::Cache;
use crate::cmd::unicode::CommonArgs;

pub async fn run(args: CommonArgs, cache: &mut Cache) -> eyre::Result<()> {
    let Some(select) = args.version else {
        let text = "Version is required: use `ag-gen unicode use --help` for more information";
        eprintln!("{}", style(text).bright().red());
        return Ok(());
    };

    let Ok(target_version) = cache.version(&select) else {
        let text = format!(
            "No stored versions match {0}\nuse `ag-gen unicode fetch {0}` to download it",
            select
        );
        eprintln!("{}", style(text).bright().red());
        return Ok(());
    };

    let select_str = if select.is_version() {
        format!("{}", select)
    } else {
        format!("{} ({})", select, target_version.version)
    };

    let version_line = style(format!("   {}", target_version));
    let version_line = if target_version.is_expired() && target_version.is_current() {
        format!("{}", version_line.bright().red())
    } else if target_version.is_current() {
        format!("{}", version_line.bright().green())
    } else {
        format!("{}", version_line.bright().black())
    };

    println!("Use {}:\n{}", select_str, version_line);

    cache.metadata.use_version = Some(select);

    Ok(())
}
