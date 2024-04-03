use nom::IResult;

pub type Result<'a, T> = IResult<&'a str, T>;

pub fn find_digit(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_till;

    take_till(|c: char| c.is_digit(10))(input)
}

pub fn skip_space(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;

    take_while1(|c: char| c.is_whitespace())(input)
}

pub fn non_space(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;

    take_while1(|c: char| !c.is_whitespace())(input)
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
