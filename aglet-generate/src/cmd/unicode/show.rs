use crate::cache::Cache;
use crate::cmd::unicode::CommonArgs;

pub async fn run(_args: CommonArgs, cache: &mut Cache) -> eyre::Result<()> {
    // let mut ftp = remote::ftp::login().await?;
    //
    // let versions = remote::list_versions(&mut ftp).await?;
    // dbg!(&versions);
    //
    // let latest = remote::get_latest_version(&mut ftp).await?;
    // let draft = remote::get_draft_version(&versions, &latest)?;
    //
    // dbg!(latest);
    // dbg!(draft);

    dbg!(&cache);

    Ok(())
}
