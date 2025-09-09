use std::collections::HashMap;

use aglet_text::CharRange;
use nom::Parser;

use crate::parse;

pub type Range = (u32, Option<u32>);
pub type RangeMap = HashMap<String, Vec<(u32, Option<u32>)>>;

pub fn char_range_single_properties<'a, P, T>(
    parser: P,
) -> impl Parser<&'a str, Output = (Option<CharRange>, T), Error = parse::Error<'a>>
where
    P: Parser<&'a str, Output = T, Error = parse::Error<'a>>,
{
    use parse::ucd::{codepoint_range, ucd_lines};
    
    let line_parser = |((start, end), prop)| {
        let range = match (start, end).try_into() {
            Ok(range) => Some(range),
            Err(_) => None,
        };

        (range, prop)
    };
    
    ucd_lines((codepoint_range, parser), line_parser)
}

pub fn property_ranges(input: &str) -> parse::Result<RangeMap> {
    use parse::ucd::name;
    
    let (i, lines) = char_range_single_properties(name).parse(input)?;
    let props = lines
        .into_iter()
        .filter_map(|(range, prop)| {
            range.map(|range| (range, prop))
        })
        .collect();
    
    Ok((i, props))
}