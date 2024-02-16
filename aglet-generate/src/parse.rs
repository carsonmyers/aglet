use nom::IResult;

pub type Result<'a, T> = IResult<&'a str, T>;
