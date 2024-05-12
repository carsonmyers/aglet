use crate::parse::{rest_of_line, spaces, spaces1, Error, Result};
use aglet_text::CharRange;
use nom::Parser;
use paste::paste;
use seq_macro::seq;

pub fn comment(input: &str) -> Result<()> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::line_ending;
    use nom::combinator::{eof, value};
    use nom::sequence::{delimited, preceded, terminated};

    alt((
        terminated(value((), spaces1), eof),
        terminated(value((), spaces), line_ending),
        delimited(
            spaces,
            value((), preceded(tag("#"), rest_of_line)),
            alt((line_ending, eof)),
        ),
    ))
    .parse(input)
}

pub fn name(input: &str) -> Result<&str> {
    use nom::bytes::complete::{take_while, take_while1};
    use nom::combinator::recognize;
    use nom::sequence::pair;

    recognize(pair(
        take_while1(|c: char| c == '_' || c.is_alphabetic()),
        take_while(|c: char| c == '_' || c.is_alphanumeric()),
    ))
    .parse(input)
}

pub fn value(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;
    use nom::AsChar;

    take_while1(|c: char| !c.is_space() && !c.is_newline() && c != ';').parse(input)
}

pub fn char_range(input: &str) -> Result<CharRange> {
    use nom::bytes::complete::tag;
    use nom::combinator::opt;
    use nom::sequence::preceded;

    let (next_input, codepoints) = (codepoint, opt(preceded(tag(".."), codepoint))).parse(input)?;

    match codepoints.try_into() {
        Ok(range) => Ok((next_input, range)),
        Err(_) => Err(nom::Err::Error(Error::range(input))),
    }
}

pub fn codepoint<'a>(input: &'a str) -> Result<u32> {
    use nom::character::complete::hex_digit1;
    use nom::combinator::map_res;

    map_res(hex_digit1, |hex: &'a str| u32::from_str_radix(hex, 16)).parse(input)
}

pub fn many0_values<'a, F, T>(
    mut parser: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    use nom::bytes::complete::tag;
    use nom::sequence::delimited;

    move |i: &'a str| {
        let mut acc = Vec::with_capacity(4);
        let mut next_input = i;
        loop {
            match parser.parse(next_input) {
                Ok((i, o)) => {
                    next_input = i;
                    acc.push(o);
                },
                Err(_) => break,
            }

            match delimited(spaces, tag(";"), spaces).parse(next_input) {
                Ok((i, _)) => {
                    next_input = i;
                },
                Err(_) => break,
            }
        }

        Ok((next_input, acc))
    }
}

pub fn many1_values<'a, F, T>(parser: F) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    use nom::combinator::verify;
    verify(many0_values(parser), |values: &Vec<T>| !values.is_empty())
}

pub fn next_value<'a, F, T>(mut parser: F) -> impl FnMut(&'a str) -> Result<'a, T>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    use nom::bytes::complete::tag;
    use nom::sequence::delimited;

    move |i: &'a str| {
        let mut next_input = i;

        let (i, _) = delimited(spaces, tag(";"), spaces).parse(next_input)?;
        next_input = i;

        let (i, o) = parser.parse(next_input)?;
        next_input = i;

        Ok((next_input, o))
    }
}

pub trait UcdTupleParser<'a, T> {
    fn parse_ucd_tuple(&mut self, input: &'a str) -> Result<'a, T>;
}

impl<'a> UcdTupleParser<'a, ()> for () {
    fn parse_ucd_tuple(&mut self, input: &'a str) -> Result<'a, ()> {
        Ok((input, ()))
    }
}

impl<'a, T, F> UcdTupleParser<'a, (T,)> for (F,)
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    fn parse_ucd_tuple(&mut self, input: &'a str) -> Result<'a, (T,)> {
        let (i, o) = self.0.parse(input)?;
        Ok((i, (o,)))
    }
}

macro_rules! ucd_trait(
    () => (
        seq!(N in 0..=21 {
            ucd_trait!(#(N)*);
        });
    );
    ($first:literal $second:literal $($rest:literal)+) => (
        ucd_trait!(inner $first $second; $($rest)+);
    );
    (inner $($current:literal)*; $head:literal $($rest:literal)+) => (
        ucd_trait_impl!($($current)*);
        ucd_trait!(inner $($current)* $head; $($rest)+);
    );
    (inner $($current:literal)*; $head:literal) => (
        ucd_trait_impl!($($current)*);
        ucd_trait_impl!($($current)* $head);
    )
);

macro_rules! ucd_trait_impl(
    ($($id:literal)*) => (
        paste! {
            ucd_trait_impl!(inner $($id [<Output $id>] [<Parser $id>]),*);
        }
    );
    (inner 0 $out1:ident $fun1:ident, $($id:tt $out:ident $fun:ident),*) => (
        impl<'a, $out1, $($out),+ , $fun1, $($fun),+>
            UcdTupleParser<'a, ( $out1, $($out),+ )>
            for ( $fun1, $($fun),+ )
        where
            $fun1: nom::Parser<&'a str, Output = $out1, Error = Error<'a>>,
            $($fun: nom::Parser<&'a str, Output = $out, Error = Error<'a>>),+
        {
            fn parse_ucd_tuple(&mut self, input: &'a str) -> Result<'a, ( $out1, $($out),+ )> {
                use nom::sequence::delimited;
                use nom::bytes::complete::tag;
                use crate::parse::spaces;

                let mut next_input = input;

                let (i, o) = self.0.parse(next_input)?;
                next_input = i;

                let res = (o, $({
                    let (i, _) = delimited(spaces, tag(";"), spaces).parse(next_input)?;
                    next_input = i;

                    let (i, o) = self.$id.parse(next_input)?;
                    next_input = i;

                    o
                }),+);

                Ok((next_input, res))
            }
        }
    )
);

ucd_trait!();

pub fn ucd_line<'a, T, List>(
    mut parser: List,
) -> impl Parser<&'a str, Output = T, Error = Error<'a>>
where
    List: UcdTupleParser<'a, T>,
{
    move |input: &'a str| parser.parse_ucd_tuple(input)
}

pub fn ucd_lines<'a, T, List>(
    parser: List,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    List: UcdTupleParser<'a, T>,
{
    use nom::multi::{many0_count, many1};
    use nom::sequence::delimited;

    many1(delimited(
        many0_count(comment),
        ucd_line(parser),
        many0_count(comment),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {
        assert_eq!(name("abc123"), Ok(("", "abc123")));
        assert_eq!(name("f_123"), Ok(("", "f_123")));
        assert_eq!(name("_123"), Ok(("", "_123")));
        assert!(name("1abc").is_err());
    }

    #[test]
    fn test_codepoint() {
        assert_eq!(codepoint("AB"), Ok(("", 0xABu32)));
        assert_eq!(codepoint("123"), Ok(("", 0x123u32)));
    }

    #[test]
    fn test_char_range() {
        let (_, range) = char_range("AB").unwrap();
        assert_eq!(range.start(), &'\u{AB}');
        assert_eq!(range.end(), &'\u{AB}');

        let (_, range) = char_range("123..F012").unwrap();
        assert_eq!(range.start(), &'\u{123}');
        assert_eq!(range.end(), &'\u{F012}');
    }

    #[test]
    fn test_ucd_line() {
        use nom::combinator::opt;

        let (_, (range, val1, val2)) = ucd_line((char_range, name, name))
            .parse("AB..C0 ; hello;\tworld")
            .unwrap();

        assert_eq!(range.start(), &'\u{AB}');
        assert_eq!(range.end(), &'\u{C0}');
        assert_eq!(val1, "hello");
        assert_eq!(val2, "world");

        let (_, (val, rest)) = ucd_line((name, many1_values(name)))
            .parse("hello;my;darling")
            .unwrap();

        assert_eq!(val, "hello");
        assert_eq!(rest.len(), 2);
        assert_eq!(rest[0], "my");
        assert_eq!(rest[1], "darling");

        let mut parser = ucd_line((name, many0_values(opt(name))));

        let (_, (val, rest)) = parser.parse("a;b").unwrap();
        assert_eq!(val, "a");
        assert_eq!(rest.len(), 1);
        assert_eq!(rest[0], Some("b"));

        let (_, (val, rest)) = parser.parse("a;b;c").unwrap();
        assert_eq!(val, "a");
        assert_eq!(rest.len(), 2);
        assert_eq!(rest[0], Some("b"));
        assert_eq!(rest[1], Some("c"));

        let (_, (val, rest)) = parser.parse("a;;;").unwrap();
        assert_eq!(val, "a");
        assert_eq!(rest.len(), 3);
        assert!(rest.iter().all(|val| val.is_none()));

        let (_, (val, rest)) = parser.parse("a;hello;;world;;").unwrap();
        assert_eq!(val, "a");
        assert_eq!(rest.len(), 5);
        assert_eq!(rest[0], Some("hello"));
        assert!(rest[1].is_none());
        assert_eq!(rest[2], Some("world"));
        assert!(rest[3].is_none());
        assert!(rest[4].is_none());
    }

    #[test]
    fn test_ucd_lines() {
        use nom::combinator::opt;

        let input = [
            "# hello world",
            "# I am comments",
            "",
            "\t#some data:",
            "1C;one_c;C_1C;jim",
            "",
            "### MORE DATA ###",
            "#################",
            "",
            "A0..AC;some_as;;;",
            "B1..10B2\t;lots\t;\tand_lots   ;  AND_LOTS ;     # this one is big # so big",
            "00..1A;just_some ; # hello!",
            "",
            " # and that's it",
        ]
        .join("\n");

        let res = ucd_lines((char_range, name, many0_values(opt(name)))).parse(&input);
        assert!(res.is_ok());

        let (i, lines) = res.unwrap();
        assert_eq!(i, "");
        assert_eq!(lines.len(), 4);

        let (range, name, aliases) = &lines[0];
        assert_eq!(range.start(), &'\u{1c}');
        assert_eq!(range.end(), &'\u{1c}');
        assert_eq!(name, &"one_c");
        assert_eq!(aliases.len(), 2);
        assert_eq!(aliases[0], Some("C_1C"));
        assert_eq!(aliases[1], Some("jim"));

        let (range, name, aliases) = &lines[1];
        assert_eq!(range.start(), &'\u{a0}');
        assert_eq!(range.end(), &'\u{ac}');
        assert_eq!(name, &"some_as");
        assert_eq!(aliases.len(), 3);
        assert!(aliases.iter().all(|alias| alias.is_none()));

        let (range, name, aliases) = &lines[2];
        assert_eq!(range.start(), &'\u{b1}');
        assert_eq!(range.end(), &'\u{10B2}');
        assert_eq!(name, &"lots");
        assert_eq!(aliases.len(), 3);
        assert_eq!(aliases[0], Some("and_lots"));
        assert_eq!(aliases[1], Some("AND_LOTS"));
        assert!(aliases[2].is_none());

        let (range, name, aliases) = &lines[3];
        assert_eq!(range.start(), &'\u{0}');
        assert_eq!(range.end(), &'\u{1a}');
        assert_eq!(name, &"just_some");
        assert_eq!(aliases.len(), 1);
        assert!(aliases[0].is_none());
    }
}
