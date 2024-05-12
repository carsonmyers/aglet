use nom::Parser;

use crate::parse::Result;

pub fn month<'a>(input: &'a str) -> Result<&'a str> {
    use nom::branch::alt;
    use nom::bytes::complete::tag_no_case;

    alt((
        tag_no_case("Jan"),
        tag_no_case("Feb"),
        tag_no_case("Mar"),
        tag_no_case("Apr"),
        tag_no_case("May"),
        tag_no_case("Jun"),
        tag_no_case("Jul"),
        tag_no_case("Aug"),
        tag_no_case("Sep"),
        tag_no_case("Oct"),
        tag_no_case("Nov"),
        tag_no_case("Dec"),
    ))
    .parse(input)
}

pub fn time<'a>(input: &'a str) -> Result<&'a str> {
    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::combinator::{consumed, map};
    use nom::sequence::separated_pair;

    map(
        consumed(separated_pair(digit1, tag(":"), digit1)),
        |(out, _)| out,
    )
    .parse(input)
}
