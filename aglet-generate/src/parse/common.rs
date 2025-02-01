use nom::{AsChar, Input, Parser};

use crate::parse::{Error, Result};

pub fn non_digits(input: &str) -> Result<&str> {
    input.split_at_position_complete(|c| c.is_dec_digit())
}

pub fn spaces(input: &str) -> Result<&str> {
    input.split_at_position_complete(|c| !c.is_space() || c.is_newline())
}

pub fn spaces1(input: &str) -> Result<&str> {
    input.split_at_position1_complete(
        |c| !c.is_space() || c.is_newline(),
        nom::error::ErrorKind::Space,
    )
}

pub fn non_spaces(input: &str) -> Result<&str> {
    input.split_at_position_complete(|c| c.is_space())
}

pub fn hex_digits(input: &str) -> Result<&str> {
    input.split_at_position_complete(|c| !c.is_hex_digit())
}

pub fn rest_of_line(input: &str) -> Result<&str> {
    input.split_at_position_complete(|c| c.is_newline())
}

pub fn context<'a, F, T>(
    ctx: &'static str,
    mut parser: F,
) -> impl Parser<&'a str, Output = T, Error = Error<'a>>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    move |i: &'a str| match parser.parse(i) {
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(e.with_context(i, ctx))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(e.with_context(i, ctx))),
        other => other,
    }
}

pub fn context_res<'a, T>(
    ctx: &'static str,
    res: nom::IResult<&'a str, T, Error<'a>>,
) -> nom::IResult<&'a str, T, Error<'a>> {
    match res {
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(e.append_context(ctx))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(e.append_context(ctx))),
        other => other,
    }
}
