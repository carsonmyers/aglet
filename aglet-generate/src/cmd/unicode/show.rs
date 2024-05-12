use console::style;

use crate::cache::Cache;
use crate::cmd::unicode::CommonArgs;
use crate::unicode::SelectVersion;

pub async fn run(_args: CommonArgs, cache: &mut Cache) -> eyre::Result<()> {
    let Ok(Some(current_version)) = cache.default_version() else {
        return print_no_current_version(cache);
    };

    let use_line = if let Some(version) = &cache.metadata.use_version {
        let select = match version {
            SelectVersion::Version(version) => version.to_string(),
            v => format!("{} ({})", v, current_version.version),
        };

        format!("Use {}:", select)
    } else {
        let select = current_version.version.to_string();
        format!("Use most recent valid version: {}", select)
    };

    let version_line = style(format!("   {}", current_version));
    let version_line = if current_version.is_expired() && current_version.is_current() {
        format!("{}", version_line.bright().red())
    } else if current_version.is_current() {
        format!("{}", version_line.bright().green())
    } else {
        format!("{}", version_line.bright().black())
    };

    println!("{}\n{}", use_line, version_line);

    Ok(())
}

fn print_no_current_version(cache: &Cache) -> eyre::Result<()> {
    let candidate_versions = if let Some(version) = &cache.metadata.use_version {
        cache
            .metadata
            .stored_versions
            .iter()
            .filter(|stored| stored.selected_by(&version))
            .collect::<Vec<_>>()
    } else {
        cache.metadata.stored_versions.iter().collect::<Vec<_>>()
    };

    let matching_msg = if let Some(version) = &cache.metadata.use_version {
        format!(" matching {}", version)
    } else {
        String::new()
    };

    if candidate_versions.len() == 0 {
        let text = format!("no versions{} found in cache", matching_msg);
        eprintln!("{}", style(text).bright().red());
        return Ok(());
    }

    let text = format!(
        "{} stored versions{} found in cache, but none are valid and current:",
        candidate_versions.len(),
        matching_msg
    );
    eprintln!("{}", style(text).bright().red());

    for v in candidate_versions {
        let version_line = style(format!("   {} - {}", v.version, v));

        if v.is_expired() {
            eprintln!("{}", version_line.bright().red());
        } else {
            eprintln!("{}", version_line.bright().black());
        }
    }

    Ok(())
}
