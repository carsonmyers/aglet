mod common;
mod error;
pub mod ftp;
pub mod ucd;

use std::fmt::Display;

pub use common::*;
pub use error::*;
use eyre::eyre;
use nom::{AsChar, Finish, IResult, Input};

pub fn finish<I, O, E>(res: IResult<I, O, E>) -> eyre::Result<O>
where
    I: Input,
    <I as Input>::Item: AsChar,
    E: Display,
{
    match res.finish() {
        Ok((_, res)) => Ok(res),
        Err(err) => Err(eyre!("{}", err)),
    }
}
