use nom::{AsChar, Input};

use crate::parse::Result;

pub fn non_digits<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position_complete(|c| c.is_dec_digit())
}

pub fn spaces<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position_complete(|c| !c.is_space() || c.is_newline())
}

pub fn spaces1<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position1_complete(
        |c| !c.is_space() || c.is_newline(),
        nom::error::ErrorKind::Space,
    )
}

pub fn non_spaces<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position_complete(|c| c.is_space())
}

pub fn hex_digits<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position_complete(|c| !c.is_hex_digit())
}

pub fn rest_of_line<'a>(input: &'a str) -> Result<&'a str> {
    input.split_at_position_complete(|c| c.is_newline())
}
