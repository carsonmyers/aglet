use nom::IResult;

pub type Result<'a, T> = IResult<&'a str, T>;

pub fn find_digit(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_till;

    take_till(|c: char| c.is_ascii_digit())(input)
}

pub fn skip_space(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;

    take_while1(|c: char| c.is_whitespace())(input)
}

pub fn non_space(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;

    take_while1(|c: char| !c.is_whitespace())(input)
}

pub fn identifier(input: &str) -> Result<&str> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1};
    use nom::combinator::recognize;
    use nom::multi::many0_count;
    use nom::sequence::pair;

    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub fn comment(input: &str) -> Result<&str> {
    use nom::bytes::complete::tag;
    use nom::combinator::rest;
    use nom::sequence::preceded;

    preceded(skip_space, preceded(tag("#"), rest))(input)
}

pub fn month(input: &str) -> Result<&str> {
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
    ))(input)
}

pub fn time(input: &str) -> Result<&str> {
    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::combinator::{consumed, map};
    use nom::sequence::separated_pair;

    map(
        consumed(separated_pair(digit1, tag(":"), digit1)),
        |(out, _)| out,
    )(input)
}
