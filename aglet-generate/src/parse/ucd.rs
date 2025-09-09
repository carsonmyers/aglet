use std::fmt::Debug;
use std::str::FromStr;

use aglet_text::{CharRange, UnicodeContext, UnicodeContextKind};
use nom::Parser;
use paste::paste;
use seq_macro::seq;
use tracing::warn;
use crate::parse::{Error, Result};

fn ucd_delimiter(input: &str) -> Result<()> {
    use nom::bytes::complete::tag;
    use nom::combinator::value;
    use nom::sequence::delimited;

    use super::spaces;

    value((), delimited(spaces, tag(";"), spaces)).parse(input)
}

pub fn comment(input: &str) -> Result<()> {
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_while};
    use nom::character::complete::line_ending;
    use nom::combinator::{consumed, eof, map, value};
    use nom::sequence::{delimited, preceded, terminated};

    use super::{context, spaces, spaces1};

    let mut parser = alt((
        context("spaces eof", terminated(value((), spaces1), eof)),
        context("spaces nl", terminated(value((), spaces), line_ending)),
        context("spaces comment", delimited(
            spaces,
            value((), preceded(tag("#"), take_while(|c| c != '\n'))),
            alt((line_ending, eof)),
        )),
    ));
    
    parser.parse(input)
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
    use nom::bytes::complete::take_while;
    use nom::combinator::map;
    use nom::AsChar;

    map(
        take_while(|c: char| !c.is_newline() && c != ';' && c != '#'),
        |val: &str| val.trim(),
    )
    .parse(input)
}

pub fn codepoint_range(input: &str) -> Result<(u32, Option<u32>)> {
    use nom::bytes::complete::tag;
    use nom::combinator::opt;

    let (next_input, (start, range, end)) = (
        codepoint,
        opt(tag("..")),
        opt(codepoint)
    ).parse(input)?;

    // ensure that an open range (e.g. `AB..`) is not accepted
    if range.is_some() && end.is_none() {
        return Err(nom::Err::Error(Error::range(input)))
    }
    
    Ok((next_input, (start, end)))
}

pub fn char_range(input: &str) -> Result<CharRange> {
    let (next_input, (start, end)) = codepoint_range(input)?;
    
    match (start, end).try_into() {
        Ok(range) => Ok((next_input, range)),
        Err(_) => Err(nom::Err::Error(Error::range(input))),
    }
}

pub fn codepoints(input: &str) -> Result<Vec<u32>> {
    use nom::multi::separated_list1;

    use super::spaces1;

    separated_list1(spaces1, codepoint).parse(input)
}

pub fn chars(input: &str) -> Result<Vec<char>> {
    use nom::multi::separated_list1;
    
    use super::spaces1;
    
    separated_list1(spaces1, codepoint_char).parse(input)
}

pub fn codepoint(input: &str) -> Result<u32> {
    use nom::character::complete::hex_digit1;
    use nom::combinator::map_res;

    map_res(hex_digit1, |hex: &str| u32::from_str_radix(hex, 16)).parse(input)
}

pub fn codepoint_char(input: &str) -> Result<char> {
    use nom::combinator::map_res;

    map_res(codepoint, |codepoint| char::from_u32(codepoint).ok_or_else(|| {
        aglet_text::Error::InvalidCodepoint(codepoint)
    })).parse(input)
}

pub fn language_tag(input: &str) -> Result<&str> {
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
    use nom::combinator::map;
    use nom::multi::{separated_list0, separated_list1};
    use nom::sequence::separated_pair;

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

macro_rules! ucd_value(
    ($ctx:expr, $parser:expr, $input:expr) => {{
        use nom::{Input, AsChar, Err};

        use super::context_res;
        
        let (final_input, value_input) = $input.split_at_position_complete::<_, Error>(|c| {
            c == ';' || c == '#' || c.is_newline()
        })?;
        
        match context_res($ctx, $parser.parse(value_input.trim())) {
            Ok((remaining, output)) if remaining.is_empty() => Ok((final_input, output)),
            Ok((remaining, _)) => {
                println!("incomplete value: {} / {}", $input, remaining);
                Err(Err::Error::<Error>(Error::incomplete_value($input).append_context($ctx)))
            }
            err @ Err(_) => err,
        }
    }};
    ($parser:expr, $input:expr) => (
        ucd_value!("ucd value", $parser, $input)
    );
);

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
        let (next_input, output) = ucd_value!("ucd value 0", self.0, input)?;
        Ok((next_input, (output,)))
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
                use nom::combinator::opt;

                use super::context;

                let mut next_input = input;

                let (i, o) = ucd_value!("ucd value 0", self.0, next_input)?;
                next_input = i;

                let res = (o, $({
                    let (i, _) = context(stringify!(ucd delimiter $id), opt(ucd_delimiter)).parse(next_input)?;
                    next_input = i;

                    let ctx = stringify!(ucd value $id);
                    let (i, o) = ucd_value!(ctx, self.$id, next_input)?;
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
    parser: List,
) -> impl Parser<&'a str, Output = T, Error = Error<'a>>
where
    List: UcdTupleParser<'a, T>,
{
    use nom::combinator::map;
    
    use super::empty;
    
    map(line(parser, empty), |(output, _)| output)
}

pub fn ucd_line_rest<'a, T, U, List, F>(
    prefix_parser: List,
    rest_parser: F,
) -> impl Parser<&'a str, Output = (T, Vec<U>), Error = Error<'a>>
where
    List: UcdTupleParser<'a, T>,
    F: Parser<&'a str, Output = U, Error = Error<'a>>,
{
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::line_ending;
    use nom::combinator::{consumed, eof, map, value, verify, peek};
    use nom::multi::many0;
    use nom::sequence::delimited;
    
    use super::{context, spaces};
    
    let terminator = context(
        "ucd value terminator", alt((
            value(true, tag(";")),
            value(false, eof),
            value(false, peek(tag("#"))),
            value(false, peek(line_ending)),
        ))
    );
    
    let value = context("ucd spaced value", delimited(spaces, consumed(rest_parser), spaces));
    let segment = context("ucd segments", verify((
        value,
        terminator,
    ), |((input, _), is_delimiter)| {
        *is_delimiter || !input.is_empty()
    }));
    
    let segments = context("ucd segments", many0(segment));
    let filtered = map(segments, |segments| {
        segments
            .into_iter()
            .filter_map(|((input, value), is_delimiter)| {
                if !is_delimiter && input.is_empty() {
                    None
                } else {
                    Some(value)
                }
            })
            .collect()
        });

    line(prefix_parser, filtered)
}

fn line<'a, T, U, List, F>(
    mut prefix_parser: List,
    mut rest_parser: F,
) -> impl Parser<&'a str, Output = (T, U), Error = Error<'a>>
where
    List: UcdTupleParser<'a, T>,
    F: Parser<&'a str, Output = U, Error = Error<'a>>,
{
    use nom::{AsChar, Err};
    use nom::combinator::{consumed, opt};
    
    use super::{context, context_res};

    move |input: &'a str| {
        // parse the ucd line prefix tuple
        let (next_input, prefix) = context_res("line prefix tuple", prefix_parser.parse_ucd_tuple(input))?;

        // skip the next delimiter, if it exists
        let (next_input, _) = context("line prefix delimiter", opt(ucd_delimiter)).parse(next_input)?;
        
        // collect the remaining fields
        let (next_input, rest) = context_res("line suffix", rest_parser.parse(next_input))?;
        
        // skip whitespace and comments at the end of the line
        let (next_input, (remainder, _)) = consumed(opt(comment)).parse(next_input)?;
        
        // ensure the whole line was consumed. The comment parser must match a section of text which
        // ends in a newline, unless it appeared at the end of the file. If it didn't match a
        // newline, then the next input text must be empty
        if !remainder.ends_with("\n") && !next_input.is_empty() {
            return Err(Err::Error::<Error>(Error::incomplete_line(next_input)));
        }

        Ok((next_input, (prefix, rest)))
    }
}

pub fn ucd_lines<'a, P, L, F, T>(
    parser: P,
    f: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    P: UcdTupleParser<'a, L>,
    F: FnMut(L) -> T,
{
    lines(ucd_line(parser), |v| Ok(f(v)))
}

pub fn ucd_lines_err<'a, P, L, F, T>(
    parser: P,
    f: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    P: UcdTupleParser<'a, L>,
    F: FnMut(L) -> std::result::Result<T, Error<'a>>,
{
    lines(ucd_line(parser), f)
}

pub fn ucd_lines_rest<'a, P, L, R, A, F, T>(
    prefix_parser: P,
    rest_parser: R,
    f: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    P: UcdTupleParser<'a, L>,
    R: Parser<&'a str, Output = A, Error = Error<'a>>,
    F: FnMut((L, Vec<A>)) -> T,
{
    lines(ucd_line_rest(prefix_parser, rest_parser), |v| Ok(f(v)))
}

pub fn ucd_lines_rest_err<'a, P, L, R, A, F, T>(
    prefix_parser: P,
    rest_parser: R,
    f: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    P: UcdTupleParser<'a, L>,
    R: Parser<&'a str, Output = A, Error = Error<'a>>,
    F: FnMut((L, Vec<A>)) -> std::result::Result<T, Error<'a>>,
{
    lines(ucd_line_rest(prefix_parser, rest_parser), f)
}

fn lines<'a, P, L, F, T, E>(
    mut parser: P,
    f: F,
) -> impl Parser<&'a str, Output = Vec<T>, Error = Error<'a>>
where
    P: Parser<&'a str, Output = L, Error = Error<'a>>,
    F: FnMut(T) -> std::result::Result<T, Error<'a>>,
{
    use nom::multi::many0_count;
    
    use super::{context, context_res};
    
    move |input: &'a str| {
        let mut next_input = input;
        let mut lines = Vec::with_capacity(1024);
        loop {
            let (i, _) = context("skip comments", many0_count(comment)).parse(next_input)?;
            next_input = i;
            
            if next_input.is_empty() {
                break;
            }
            
            let (i, line) = context_res("line parser", parser.parse(next_input))?;
            next_input = i;
            
            let mapped = f(line)?;
            lines.push(mapped);
        }
        
        Ok((next_input, lines))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::combinator::all_consuming;
    use tracing::warn;

    #[test]
    fn test_comment() {
        use nom::combinator::consumed;
        
        let input = [
            "# hello world",
            " # this is a comment\t ",
            " ",
            "",
            " #end",
        ].join("\n");

        let (i, (text, _)) = consumed(comment).parse(&input).unwrap();
        assert_eq!(text, "# hello world\n");
        
        let (i, (text, _)) = consumed(comment).parse(i).unwrap();
        assert_eq!(text, " # this is a comment\t \n");

        let (i, (text, _)) = consumed(comment).parse(i).unwrap();
        assert_eq!(text, " \n");

        let (i, (text, _)) = consumed(comment).parse(i).unwrap();
        assert_eq!(text, "\n");
        assert_eq!(i, " #end");

        let (i, (text, _)) = consumed(comment).parse(i).unwrap();
        assert_eq!(text, " #end");
        
        assert_eq!(i, "");
    }

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
        let (_, (range, val1, val2)) = ucd_line((char_range, name, name))
            .parse("AB..C0 ; hello;\tworld")
            .unwrap();

        assert_eq!(range.start(), &'\u{AB}');
        assert_eq!(range.end(), &'\u{C0}');
        assert_eq!(val1, "hello");
        assert_eq!(val2, "world");

        let mut parser = ucd_line((name, name));

        let (_, (name1, name2)) = parser.parse("a;b").unwrap();
        assert_eq!(name1, "a");
        assert_eq!(name2, "b");
        
        let res = parser.parse("a");
        assert!(res.is_err());
        
        let res = parser.parse("a;b;c");
        assert!(res.is_err());
    }
    
    #[test]
    fn test_ucd_line_prefix() {
        let mut parser = ucd_line_rest((char_range, name), value);
        
        let (_, ((range, val), rest)) = parser
            .parse("AB..C0 ; hello ; world;; # comment")
            .unwrap();
        assert_eq!(range.start(), &'\u{AB}');
        assert_eq!(range.end(), &'\u{C0}');
        assert_eq!(val, "hello");
        assert_eq!(rest, &["world", ""]);
        
        let res = parser.parse("1;");
        assert!(res.is_err());
        
        let (_, ((range, val), rest)) = parser
            .parse("1;hello # comment")
            .unwrap();
        assert_eq!(range.start(), &'\u{1}');
        assert_eq!(range.end(), &'\u{1}');
        assert_eq!(val, "hello");
        assert!(rest.is_empty());
        
        let mut parser = ucd_line_rest((name,), value);
        
        let (_, ((val,), rest)) = parser
            .parse("abc")
            .unwrap();
        assert_eq!(val, "abc");
        assert!(rest.is_empty());
        
        let (_, ((val,), rest)) = parser
            .parse("abc;hello")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello"]);

        let (_, ((val,), rest)) = parser
            .parse("abc;hello;")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello"]);

        let (_, ((val,), rest)) = parser
            .parse("abc;hello#")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello"]);

        let (_, ((val,), rest)) = parser
            .parse("abc;hello;;#")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello", ""]);

        let (_, ((val,), rest)) = parser
            .parse("abc;hello;;;#")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello", "", ""]);

        let (_, ((val,), rest)) = parser
            .parse("abc;hello;;;")
            .unwrap();
        assert_eq!(val, "abc");
        assert_eq!(rest, &["hello", "", ""]);
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
            "B1..10B2\t;lots\t;\tand_lots   ;  AND_LOTS ;;many;     # this one is big # so big",
            "00..1A;just_some;# hello!",
            "",
            " # and that's it",
        ]
        .join("\n");

        let mut parser = ucd_lines_rest((char_range, name), opt(name));

        let (i, lines) = parser.parse(&input).unwrap();
        assert_eq!(i, "");
        assert_eq!(lines.len(), 4);

        let ((range, val), aliases) = &lines[0];
        assert_eq!(range.start(), &'\u{1c}');
        assert_eq!(range.end(), &'\u{1c}');
        assert_eq!(val, &"one_c");
        assert_eq!(aliases, &[Some("C_1C"), Some("jim")]);

        let ((range, val), aliases) = &lines[1];
        assert_eq!(range.start(), &'\u{a0}');
        assert_eq!(range.end(), &'\u{ac}');
        assert_eq!(val, &"some_as");
        assert_eq!(aliases, &[None, None]);

        let ((range, val), aliases) = &lines[2];
        assert_eq!(range.start(), &'\u{b1}');
        assert_eq!(range.end(), &'\u{10B2}');
        assert_eq!(val, &"lots");
        assert_eq!(aliases, &[Some("and_lots"), Some("AND_LOTS"), None, Some("many")]);

        let ((range, val), aliases) = &lines[3];
        assert_eq!(range.start(), &'\u{0}');
        assert_eq!(range.end(), &'\u{1a}');
        assert_eq!(val, &"just_some");
        assert!(aliases.is_empty());

        let unicode_data_samples = [
            "0000;<control>;Cc;0;BN;;;;;N;NULL;;;;",
            "0041;LATIN CAPITAL LETTER A;Lu;0;L;;;;;N;;;;0061;",
            "D7FB;HANGUL JONGSEONG PHIEUPH-THIEUTH;Lo;0;L;;;;;N;;;;;",
            "D800;<Non Private Use High Surrogate, First>;Cs;0;L;;;;;N;;;;;",
        ]
        .join("\n");

        let mut parser = ucd_lines_rest((
            codepoint,
            value,
            name,
        ), opt(value));

        let (i, lines) = parser.parse(&unicode_data_samples).unwrap();
        assert_eq!(lines.len(), 4);
        assert_eq!(i, "");

        let ((cp, val, gc), _) = &lines[0];
        assert_eq!(cp, &0x0);
        assert_eq!(val, &"<control>");
        assert_eq!(gc, &"Cc");

        let ((cp, val, gc), _) = &lines[1];
        assert_eq!(cp, &0x41);
        assert_eq!(val, &"LATIN CAPITAL LETTER A");
        assert_eq!(gc, &"Lu");

        let ((cp, val, gc), _) = &lines[2];
        assert_eq!(cp, &0xD7FB);
        assert_eq!(val, &"HANGUL JONGSEONG PHIEUPH-THIEUTH");
        assert_eq!(gc, &"Lo");

        let ((cp, val, gc), _) = &lines[3];
        assert_eq!(cp, &0xD800);
        assert_eq!(val, &"<Non Private Use High Surrogate, First>");
        assert_eq!(gc, &"Cs");
    }
}
