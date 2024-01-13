use color_eyre::eyre::{Report, WrapErr};
use thiserror::Error;

fn main() -> Result<(), Report> {
    color_eyre::install()?;

    let stuff = do_thing()?;
    println!("stuff: {}", stuff);
    Ok(())
}

fn do_thing() -> Result<String, Report> {
    other_thing()?;
    let stuff = std::fs::read_to_string("fake_file").wrap_err("Failed to open file")?;

    Ok(stuff.to_uppercase())
}

fn other_thing() -> Result<String, Report> {
    Err(CustomError::AnError(String::from("this is an error")).into())
}

#[derive(Error, Debug)]
pub enum CustomError {
    #[error("this is an error: {0}")]
    AnError(String),
}
