use std::str::FromStr;

use aglet_text::{CharRange, UnicodeContext, UnicodeContextKind};
use nom::Parser;
use paste::paste;
use seq_macro::seq;

use crate::parse::{Error, Result};

pub fn comment(input: &str) -> Result<()> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::line_ending;
    use nom::combinator::{eof, value};
    use nom::sequence::{delimited, preceded, terminated};

    use super::{rest_of_line, spaces, spaces1};

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
    use nom::sequence::{delimited, pair};

    use super::spaces;

    delimited(
        spaces,
        recognize(pair(
            take_while1(|c: char| c == '_' || c == '-' || c.is_alphabetic()),
            take_while(|c: char| c == '_' || c == '-' || c.is_alphanumeric()),
        )),
        spaces,
    )
    .parse(input)
}

pub fn value(input: &str) -> Result<&str> {
    use nom::bytes::complete::take_while1;
    use nom::combinator::map;
    use nom::AsChar;

    map(
        take_while1(|c: char| !c.is_newline() && c != ';' && c != '#'),
        |val: &str| val.trim(),
    )
    .parse(input)
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

pub fn codepoints<'a>(input: &'a str) -> Result<Vec<u32>> {
    use nom::multi::separated_list1;

    use super::spaces1;

    separated_list1(spaces1, codepoint).parse(input)
}

pub fn codepoint<'a>(input: &'a str) -> Result<u32> {
    use nom::character::complete::hex_digit1;
    use nom::combinator::map_res;

    map_res(hex_digit1, |hex: &'a str| u32::from_str_radix(hex, 16)).parse(input)
}

pub fn language_tag<'a>(input: &'a str) -> Result<&'a str> {
    use nom::bytes::complete::take_while1;
    use nom::combinator::opt;
    use nom::sequence::terminated;

    terminated(
        take_while1(|c: char| c.is_alphabetic()),
        opt(take_while1(|c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })),
    )
    .parse(input)
}

pub fn unicode_context(input: &str) -> Result<UnicodeContext> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::{map, map_res, opt};

    map(
        (
            opt((
                alt((tag("Not"), tag("not"), tag("NOT"))),
                alt((tag("_"), tag("-"))),
            )),
            map_res(name, UnicodeContextKind::from_str),
        ),
        |(negation, kind)| UnicodeContext::new(kind, negation.is_some()),
    )
    .parse(input)
}

pub fn condition_list(input: &str) -> Result<(Option<&str>, Vec<UnicodeContext>)> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    use nom::multi::{separated_list0, separated_list1};
    use nom::sequence::{preceded, separated_pair, terminated};

    use super::{context, spaces};

    let ctx_parser = map(separated_list1(spaces, unicode_context), |ctx| (None, ctx));

    let lang_ctx_parser = map(
        separated_pair(
            language_tag,
            spaces,
            separated_list0(spaces, unicode_context),
        ),
        |(lang, ctx)| (Some(lang), ctx),
    );

    alt((
        context("contexts only", ctx_parser),
        context("language and contexts", lang_ctx_parser),
    ))
    .parse(input)
}

pub fn many0_values<'a, F, T>(
    mut parser: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    use nom::bytes::complete::tag;
    use nom::sequence::delimited;

    use super::spaces;

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

    use super::context;

    context(
        "many1_values",
        verify(many0_values(parser), |values: &Vec<T>| !values.is_empty()),
    )
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
        use super::context_res;

        let (i, o) = context_res("ucd value 0", self.0.parse(input))?;
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
                use crate::parse::{context, context_res, spaces};

                let mut next_input = input;

                let (i, o) = context_res("ucd value 0", self.0.parse(next_input))?;
                next_input = i;

                let res = (o, $({
                    let (i, _) = context(stringify!(ucd delimiter $id), delimited(spaces, tag(";"), spaces)).parse(next_input)?;
                    next_input = i;

                    let (i, o) = context_res(stringify!(ucd value $id), self.$id.parse(next_input))?;
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

    use super::{context, spaces};

    many1(delimited(
        context(
            "skip pre-comments and whitespace",
            (many0_count(comment), spaces),
        ),
        context("ucd line", ucd_line(parser)),
        context("skip post-comments", many0_count(comment)),
    ))
}

struct UcdHelperOutput<T> {
    value: T,
    next_must_match_empty: bool,
}

impl<T> UcdHelperOutput<T> {
    fn empty(value: T) -> Self {
        Self {
            value,
            next_must_match_empty: true,
        }
    }
    
    fn value(value: T) -> Self {
        Self {
            value,
            next_must_match_empty: false,
        }
    }
}

fn next_ucd_value_helper<'a, F, T>(
    mut parser: F,
    match_separator: bool,
    must_match_empty: bool,
) -> impl Parser<&'a str, Output = UcdHelperOutput<T>, Error = Error<'a>>
where
    F: Parser<&'a str, Output = T, Error = Error<'a>>,
{
    use nom::bytes::complete::tag;
    use nom::sequence::delimited;

    use super::spaces;

    move |i: &'a str| {
        let mut next_input = i;
        
        if !must_match_empty && match_separator {
            let separator_res = delimited(spaces, tag(";"), spaces).parse(next_input);
        }
        
        if must_match_empty {
            let (_, o) = parser.parse("")?;
            return Ok((i, UcdHelperOutput::empty(o)));
        }


        if match_separator {
            
            let (i, _) = delimited(spaces, tag(";"), spaces).parse(next_input)?;
            next_input = i;
        }

        let (i, o) = parser.parse(next_input)?;
        next_input = i;

        Ok((next_input, UcdHelperOutput::value(o)))
    }
}

fn ucd_separator<'a>() -> impl Parser<&'a str, Output = bool, Error = Error<'a>> {
    use nom::bytes::complete::tag;
    use nom::sequence::delimited;
    use nom::combinator::{map, opt};
    
    use super::spaces;
    
    map(opt(delimited(spaces, tag(";"), spaces)), |tag| tag.is_some())
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::combinator::all_consuming;

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
    fn test_language_tag() {
        let (i, lang) = language_tag("en$").unwrap();
        assert_eq!(lang, "en");
        assert_eq!(i, "$");

        let (i, lang) = language_tag("lt$").unwrap();
        assert_eq!(lang, "lt");
        assert_eq!(i, "$");

        let (i, lang) = language_tag("gsw-u-sd-chzh$").unwrap();
        assert_eq!(lang, "gsw");
        assert_eq!(i, "$");
    }

    #[test]
    fn test_unicode_context() {
        let (i, ctx) = unicode_context("After_I$").unwrap();
        assert!(!ctx.negated);
        assert_eq!(ctx.kind, UnicodeContextKind::AfterI);
        assert_eq!(i, "$");

        let (i, ctx) = unicode_context("not-before-dot$").unwrap();
        assert!(ctx.negated);
        assert_eq!(ctx.kind, UnicodeContextKind::BeforeDot);
        assert_eq!(i, "$");

        let res = unicode_context("Not_Something_Else");
        assert!(res.is_err());
    }

    #[test]
    fn test_condition_list() {
        let (i, (lang, contexts)) = condition_list("en After_I Not_Final$").unwrap();
        assert_eq!(lang, Some("en"));
        assert_eq!(contexts.len(), 2);
        assert!(!contexts[0].negated);
        assert_eq!(contexts[0].kind, UnicodeContextKind::AfterI);
        assert!(contexts[1].negated);
        assert_eq!(contexts[1].kind, UnicodeContextKind::Final);
        assert_eq!(i, "$");

        let (i, (lang, contexts)) = condition_list("not-final-sigma more_Above$").unwrap();
        assert!(lang.is_none());
        assert_eq!(contexts.len(), 2);
        assert!(contexts[0].negated);
        assert_eq!(contexts[0].kind, UnicodeContextKind::FinalSigma);
        assert!(!contexts[1].negated);
        assert_eq!(contexts[1].kind, UnicodeContextKind::MoreAbove);
        assert_eq!(i, "$");
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
    fn test_ucd_line_partial() {
        let mut parser = ucd_line((name, name));
        
        let (i, (val1, val2)) = parser.parse("a;b").unwrap();
        assert_eq!(val1, "a");
        assert_eq!(val2, "b");
        assert_eq!(i, "");
        
        let (i, (val1, val2)) = parser.parse("a;b;").unwrap();
        assert_eq!(val1, "a");
        assert_eq!(val2, "b");
        assert_eq!(i, "");
        
        let (i, (val1, val2)) = parser.parse("a;b;c").unwrap();
        assert_eq!(val1, "a");
        assert_eq!(val2, "b");
        assert_eq!(i, "");
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

        let unicode_data_samples = [
            "0000;<control>;Cc;0;BN;;;;;N;NULL;;;;",
            "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0061;",
            "D7FB;HANGUL JONGSEONG PHIEUPH-THIEUTH;Lo;0;L;;;;;N;;;;;",
            "D800;<Non Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;",
        ]
        .join("\n");

        let mut parser = all_consuming(ucd_lines((
            codepoint,
            value,
            crate::parse::ucd::name,
            many0_values(opt(value)),
        )));

        let res = parser.parse(&unicode_data_samples);
        assert!(res.is_ok());

        let (i, lines) = res.unwrap();
        assert_eq!(lines.len(), 4);
        assert_eq!(i, "");

        let (cp, val, gc, _) = &lines[0];
        assert_eq!(cp, &0x0);
        assert_eq!(val, &"<control>");
        assert_eq!(gc, &"Cc");

        let (cp, val, gc, _) = &lines[1];
        assert_eq!(cp, &0x41);
        assert_eq!(val, &"LATIN CAPITAL LETTER A");
        assert_eq!(gc, &"Lu");

        let (cp, val, gc, _) = &lines[2];
        assert_eq!(cp, &0xD7FB);
        assert_eq!(val, &"HANGUL JONGSEONG PHIEUPH-THIEUTH");
        assert_eq!(gc, &"Lo");

        let (cp, val, gc, _) = &lines[3];
        assert_eq!(cp, &0xD800);
        assert_eq!(val, &"<Non Private Use High Surrogate, First>");
        assert_eq!(gc, &"Cs");
    }
}
