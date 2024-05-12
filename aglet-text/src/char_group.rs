//! Character groups for creating and matching character classes
//!
//! This module exists to facilitate the creation and modification of groups of ranges of `char`s,
//! through adding and removing ranges from the group. A CharGroup is a sorted and reduced collection
//! of ranges; sorting allows the collection to be modified more easily, and reducing ensures that there
//! are not overlapping or directly adjacent ranges (combining them into simpler ranges) to both simplify
//! modification and matching.
//!
//! [`CharGroupSlice`]: a reference to a [`CharGroup`] which can be created as a `const`
//! [`CharGroup`]: a modifiable (through addition and subtraction of ranges) vector of `char` ranges

use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, AddAssign, Deref, Sub, SubAssign};
use std::slice::Iter;

use crate::error::Error;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct CharRange(char, char);

impl CharRange {
    #[inline]
    pub fn start(&self) -> &char {
        &self.0
    }

    #[inline]
    pub fn end(&self) -> &char {
        &self.1
    }

    #[inline]
    pub fn contains(&self, c: &char) -> bool {
        &self.0 <= c && c <= &self.1
    }
}

impl Display for CharRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} .. {}", self.start(), self.end())
    }
}

impl From<(char, Option<char>)> for CharRange {
    fn from(value: (char, Option<char>)) -> Self {
        match value {
            (start, Some(end)) => (start, end).into(),
            (start, None) => start.into(),
        }
    }
}

impl TryFrom<(u32, Option<u32>)> for CharRange {
    type Error = Error;

    fn try_from(value: (u32, Option<u32>)) -> Result<Self, Self::Error> {
        match value {
            (start, Some(end)) => (start, end).try_into(),
            (start, None) => start.try_into(),
        }
    }
}

impl From<(char, char)> for CharRange {
    fn from(value: (char, char)) -> Self {
        Self(value.0, value.1)
    }
}

impl TryFrom<(u32, u32)> for CharRange {
    type Error = Error;

    fn try_from(value: (u32, u32)) -> Result<Self, Self::Error> {
        let start = char::from_u32(value.0).ok_or(Error::InvalidCodepoint(value.0))?;
        let end = char::from_u32(value.1).ok_or(Error::InvalidCodepoint(value.1))?;

        Ok(Self(start, end))
    }
}

impl From<char> for CharRange {
    fn from(value: char) -> Self {
        Self(value, value)
    }
}

impl TryFrom<u32> for CharRange {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let Some(c) = char::from_u32(value) else {
            return Err(Error::InvalidCodepoint(value));
        };

        Ok(Self(c, c))
    }
}

/// Slice version of a [`CharGroup`]
///
/// Useful for creating `const` character classes, for example as generated unicode data sets
pub type CharGroupSlice = [CharRange];

/// A collection of inclusive character ranges
///
/// The main purpose of a character group is to test for membership with `contains`. However, these groups can be
/// built in a variety of ways - the most complicated of which are regex specified character classes (which
/// are built from user input) and generated unicode data tables (e.g. all codepoints in the Letter general
/// category).
///
/// Because character groups will often consist of multiple disjoint character ranges, the `CharGroup` type's
/// purpose is to facilitate the creation of this construction by implementing addition and subtraction onto
/// a vector of character ranges such that the result contains a minimal set of ranges which don't overlap
/// and aren't immediately adjacent.
#[derive(Debug)]
pub struct CharGroup {
    ranges: Vec<CharRange>,
}

impl CharGroup {
    /// Create an empty `CharGroup` which contains no characters.
    ///
    /// An empty group is a good starting point for constructing a positive class of matching characters,
    /// for example a range like `[a-zA-Z]`; the desired ranges can be added to the group.
    pub const fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    /// Create a full `CharGroup` which contains all possible characters.
    ///
    /// A full group is a good starting point for creating a negative class of matching characters,
    /// for example a range like `[^a-zA-Z]`; the non-matching ranges can be subtracted from the group.
    pub fn all() -> Self {
        Self {
            ranges: vec![CharRange('\0', char::MAX)],
        }
    }

    /// Create a `CharGroup` from a slice
    ///
    /// The slice form of `CharGroup` doesn't support the addition and subtraction of ranges, so there's
    /// no need for it to be sorted or reduced to a minimal set; therefore, when a `CharGroup` is constructed
    /// from a slice, the data will be sorted and reduced at that point.
    pub fn from_slice(slice: &CharGroupSlice) -> Self {
        let mut ranges = slice.to_vec();
        ranges.sort_by(|a, b| a.start().cmp(b.start()));

        CharGroup { ranges }
    }

    /// Find the beginning of the group.
    ///
    /// The start character is the lowest matching character in the group, irrespective of any gaps
    /// in the group. For the group `[a-z0-9]`, the start is the character `0` (because it has the lowest
    /// codepoint 0x30). For the group `[^a-z0-9]` the start is the character `\0` (because it is the lowest
    /// codepoint, and the group matches all codepoints except those specified in the negative class).
    pub fn start(&self) -> Option<&char> {
        self.ranges.first().map(|range| range.start())
    }

    /// Find the end of the group
    ///
    /// The end character is the highest matching character in the group, irrespective of any gaps
    /// in the group. For the group `[a-z0-9]`, the end is the character `z` (because it has the highest
    /// codepoints 0x7A). For the group `[^a-z0-9]` the end is the character `\u{10FFFF}` because it is the
    /// highest codepoint, and the group matches all codepoints except those specified in the negative class.
    pub fn end(&self) -> Option<&char> {
        self.ranges.last().map(|range| range.end())
    }

    /// Add a range of characters to the group.
    ///
    /// If the range overlaps or is adjacent to a range or ranges already in the group, they will be combined
    /// such that the group is represented by a minimal set of ranges with no overlaps or adjacencies.
    pub fn add_range(&mut self, range: CharRange) {
        // get a list of existing ranges which overlap or are adjacent to the range to be added
        let overlap = self.overlap(&range, true);

        if overlap.is_empty() {
            // if there is no overlap, the new range just needs to be added to the correct place in the vec
            let idx = self
                .ranges
                .iter()
                // find the first existing range where the specified range is after the existing range's end;
                // since there is no overlap, this is guaranteed to be correct
                .position(|existing| range.start() > existing.end())
                // the new range should be added _after_ the found range
                .map(|pos| pos + 1)
                // but if no existing range precedes the new one, it should be added at the beginning
                .unwrap_or(0);

            self.ranges.insert(idx, range);
        } else {
            // if there is an overlap, all overlapping ranges will be combined into one

            // get the beginning position and lowest character of the overlap
            let (start_i, start) = overlap.first().map(|(i, r)| (i, r.start())).unwrap();

            // get the end position and highest character of the overlap
            let (end_i, end) = overlap.last().map(|(i, r)| (i, r.end())).unwrap();

            // the start of the new range should be either the start of the overlapped existing ranges, or the
            // start of the newly added range, whichever is lower
            let start = char::min(*start, *range.start());

            // the end of the new range should be either the end of the overlapped existing ranges, or the
            // end of the newly added range, whichever is higher
            let end = char::max(*end, *range.end());

            // replace all overlapped ranges with a new combined range
            self.ranges
                .splice(*start_i..=*end_i, [CharRange(start, end)]);
        }
    }

    /// Remove a range of characters from the group.
    ///
    /// If the removed range bisects a range already in the group, it will be split into two new ranges without
    /// the specified characters. If it overlaps several existing ranges, they will be removed or truncated.
    pub fn remove_range(&mut self, range: CharRange) {
        // get a list of overlapping ranges that need to be removed, truncated, or split
        let overlap = self.overlap(&range, false);

        // if there is no overlap, then there's nothing to remove
        if overlap.is_empty() {
            return;
        }

        // save the start and end indexes of the overlapped ranges
        let start_i = *overlap.first().map(|(i, _)| i).unwrap();
        let end_i = *overlap.last().map(|(i, _)| i).unwrap();

        // build a list of replacement ranges
        let mut replace_with = Vec::new();

        // an overlapped range which contains either the start or end of the removed range might contain
        // characters that should remain in the group.
        for (_, existing) in &overlap {
            // characters before the start of the removed range should be preserved
            if existing.contains(range.start()) && existing.start() < range.start() {
                // the new end of the remaining range is one character before the start of the removed range
                let end = char_sub(*range.start(), 1);
                replace_with.push(CharRange(*existing.start(), end));
            }

            // characters after the end of the removed range should be preserved
            if existing.contains(range.end()) && existing.end() > range.end() {
                // the new start of the remaining range is one character after the end of the removed range
                let start = char_add(*range.end(), 1);
                replace_with.push(CharRange(start, *existing.end()));
            }
        }

        self.ranges.splice(start_i..=end_i, replace_with);
    }

    /// Test whether a character is contained in the character group
    pub fn contains(&self, c: char) -> bool {
        self.ranges.iter().any(|range| range.contains(&c))
    }

    pub fn iter(&self) -> impl Iterator<Item = &CharRange> {
        self.ranges.iter()
    }

    /// Find all ranges in the group which overlap a given range.
    ///
    /// This function can optionally include adjacent ranges in its definition of overlap; this is useful when
    /// adding ranges (an adjacent range should be combined), and incorrect when removing them.
    fn overlap(&self, range: &CharRange, include_adjacent: bool) -> Vec<(usize, &CharRange)> {
        self.ranges
            .iter()
            .enumerate()
            .filter(|(_, existing)| {
                let (existing_start, existing_end) = if include_adjacent {
                    // check whether the characters surrounding the existing range overlap the specified range
                    // if adjacencies are included
                    (char_sub(*existing.start(), 1), char_add(*existing.end(), 1))
                } else {
                    // if not checking adjacency, only ranges which actually overlap are considered
                    (*existing.start(), *existing.end())
                };

                range.start() <= &existing_end && range.end() >= &existing_start
            })
            .collect()
    }

    /// Sort the list of ranges first by their starts, then by their ends
    fn sort(&mut self) {
        self.ranges.sort_by(|a, b| match a.start().cmp(b.start()) {
            Ordering::Equal => a.end().cmp(b.end()),
            other => other,
        })
    }

    /// Reduce the ranges to a minimal set of non-overlapping, non-adjacent ranges.
    ///
    /// The list must already be sorted
    ///
    /// If two ranges overlap or are immediately adjacent with each other, they will be combined.
    /// Groups of overlapping/adjacent ranges are found and consumed by checking if each subsequent range in
    /// the list overlaps, keeping track of the space to be covered by a new replacement range along the way.
    /// Once a range is found that doesn't overlap with the current group, the ranges are replaced with a new one
    /// and the process is repeated until there are no more ranges to process.
    fn reduce(&mut self) {
        // the ranges aren't reduced in place, so create a vector to hold the new set
        let mut new_ranges = Vec::new();

        loop {
            // keep track of the current size of the range; overlapping ranges will be added together here
            let mut current_range: Option<CharRange> = None;

            // keep track of the number of ranges consumed
            let mut overlapping_count: usize = 0;

            // manually iterate the ranges; I think this is cleaner than tracking the current range and
            // consumed count with an enumerator and state in `.reduce()`
            let mut iter = self.ranges.iter();
            loop {
                let Some(range) = iter.next() else {
                    // there are no more ranges
                    break;
                };

                let Some(CharRange(start, end)) = current_range else {
                    // this is the first range checked in this iteration; initialize the current range and include
                    // it in the count
                    current_range = Some(CharRange(*range.start(), *range.end()));
                    overlapping_count += 1;
                    continue;
                };

                // if this range doesn't overlap, then the current group has been reduced
                if range.start() > &char_add(end, 1) {
                    break;
                }

                // include the range in the accumulator and include it in the count
                current_range = Some(CharRange(start, char::max(end, *range.end())));
                overlapping_count += 1;
            }

            let Some(CharRange(start, end)) = current_range else {
                // there are no more ranges
                break;
            };

            // add the accumulated range to the new set
            new_ranges.push(CharRange(start, end));

            // remove the processed ranges; the next iteration will pick up from what's left
            self.ranges = self.ranges.split_off(overlapping_count);
        }

        self.ranges = new_ranges;
    }
}

/// AsRef implementation so that Add, AddAssign, Sub, and SubAssign can be made to work on [`CharGroup`] and
/// [`CharGroupSlice`] the same way
impl AsRef<CharGroupSlice> for CharGroup {
    fn as_ref(&self) -> &CharGroupSlice {
        self.ranges.as_slice()
    }
}

impl Deref for CharGroup {
    type Target = CharGroupSlice;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl Iterator for CharGroup {
    type Item = CharRange;

    fn next(&mut self) -> Option<Self::Item> {
        self.ranges.pop()
    }
}

impl<'a> IntoIterator for &'a CharGroup {
    type Item = &'a CharRange;
    type IntoIter = Iter<'a, CharRange>;

    fn into_iter(self) -> Self::IntoIter {
        self.ranges.iter()
    }
}

/// Implement addition on [`CharGroup`] in terms of `AddAssign`
impl<T> Add<T> for CharGroup
where
    CharGroup: AddAssign<T>,
{
    type Output = CharGroup;

    fn add(mut self, rhs: T) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

/// Allow [`CharGroup`]s to be add-assigned to each other
impl<T> AddAssign<T> for CharGroup
where
    T: AsRef<CharGroupSlice> + Sized,
{
    fn add_assign(&mut self, rhs: T) {
        for range in rhs.as_ref() {
            self.add_range(range.clone());
        }
    }
}

/// Implement subtraction on [`CharGroup`] in terms of `SubAssign`
impl<T> Sub<T> for CharGroup
where
    CharGroup: SubAssign<T>,
{
    type Output = CharGroup;

    fn sub(mut self, rhs: T) -> Self::Output {
        self.sub_assign(rhs);
        self
    }
}

/// Allow [`CharGroup`]s to be sub-assigned from each other
impl<T> SubAssign<T> for CharGroup
where
    T: AsRef<CharGroupSlice> + Sized,
{
    fn sub_assign(&mut self, rhs: T) {
        for range in rhs.as_ref() {
            self.remove_range(range.clone());
        }
    }
}

const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

/// add `amount` to a character, skipping over the surrogate groups, so that the result is still a valid `char`
const fn char_add(c: char, amount: u32) -> char {
    let next_val = c as u32 + amount;

    let next_val = if SURROGATE_START <= next_val && next_val <= SURROGATE_END {
        // the result of `c + amount` is within the surrogate range, and so is an invalid `char`. Skip over
        // the surrogates as if they aren't there
        SURROGATE_END + 1 + (next_val - SURROGATE_START)
    } else if next_val > char::MAX as u32 {
        // don't allow the result to go beyond the max codepoint
        char::MAX as u32
    } else {
        next_val
    };

    // char::from_u32 isn't const in the stable channel, but the above is enough to ensure
    // that this isn't an invalid operation
    unsafe { std::mem::transmute(next_val) }
}

const fn char_sub(c: char, amount: u32) -> char {
    let num_val = c as u32;

    // don't overflow by subtraction
    if amount > num_val {
        return '\0';
    }

    let next_val = num_val - amount;

    let next_val = if SURROGATE_START <= next_val && next_val <= SURROGATE_END {
        // the result of `c - amount` is within the surrogate range, and so is an invalid `char`. Skip over
        // the surrogates as if they aren't there
        SURROGATE_START - 1 - (SURROGATE_END - next_val)
    } else {
        next_val
    };

    // char::from_u32 isn't const in the stable channel, but the above is enough to ensure
    // that this isn't an invalid operation
    unsafe { std::mem::transmute(next_val) }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ranges {
        ($($start:literal .. $end:expr),* $(,)?) => {
            vec![$(CharRange($start, $end)),*]
        }
    }

    macro_rules! char_group {
        ($($start:literal .. $end:expr),* $(,)?) => {
            CharGroup {
                ranges: ranges![$($start .. $end),*],
            }
        }
    }

    #[test]
    fn sort() {
        let mut group = CharGroup {
            ranges: ranges!['c'..'d', 'b'..'d', 'c'..'h', 'b'..'g', 'a'..'z'],
        };

        group.sort();

        // the ranges should be sorted by start first, then by end
        assert_eq!(group.ranges[0], CharRange('a', 'z'));
        assert_eq!(group.ranges[1], CharRange('b', 'd'));
        assert_eq!(group.ranges[2], CharRange('b', 'g'));
        assert_eq!(group.ranges[3], CharRange('c', 'd'));
        assert_eq!(group.ranges[4], CharRange('c', 'h'));
    }

    #[test]
    fn reduce() {
        let mut group = CharGroup {
            ranges: ranges![
                // a-j overlapping ranges
                'a'..'g',
                'b'..'c',
                'f'..'j',
                'h'..'j',
                // l-p overlapping ranges with adjacent range p..=p
                'l'..'m',
                'm'..'n',
                'n'..'o',
                'p'..'p',
                // overlapping ranges adjacent over surrogates
                '\u{D7FE}'..'\u{D7FF}',
                '\u{E000}'..'\u{E001}',
            ],
        };

        group.sort();
        group.reduce();

        assert_eq!(group.ranges.len(), 3);
        assert_eq!(group.ranges[0], CharRange('a', 'j'));
        assert_eq!(group.ranges[1], CharRange('l', 'p'));
        assert_eq!(group.ranges[2], CharRange('\u{D7FE}', '\u{E001}'));
    }

    #[test]
    fn overlap() {
        let group = char_group!['a'..'d', 'f'..'h', 'j'..'m', 't'..'w'];

        struct TestCase {
            test: CharRange,
            include_adjacent: bool,
            expect: Vec<(usize, CharRange)>,
        }

        // test the result of `overlap()` for a given `test` range on the set of ranges above
        let tests: Vec<TestCase> = vec![
            TestCase {
                test: CharRange('g', 'h'),
                include_adjacent: true,
                expect: vec![(1, CharRange('f', 'h'))],
            },
            TestCase {
                test: CharRange('f', 'h'),
                include_adjacent: true,
                expect: vec![(1, CharRange('f', 'h'))],
            },
            TestCase {
                test: CharRange('d', 'f'),
                include_adjacent: true,
                expect: vec![(0, CharRange('a', 'd')), (1, CharRange('f', 'h'))],
            },
            TestCase {
                test: CharRange('c', 'e'),
                include_adjacent: true,
                expect: vec![(0, CharRange('a', 'd')), (1, CharRange('f', 'h'))],
            },
            TestCase {
                test: CharRange('c', 'e'),
                include_adjacent: false,
                expect: vec![(0, CharRange('a', 'd'))],
            },
            TestCase {
                test: CharRange('g', 'l'),
                include_adjacent: true,
                expect: vec![(1, CharRange('f', 'h')), (2, CharRange('j', 'm'))],
            },
            TestCase {
                test: CharRange('o', 'r'),
                include_adjacent: true,
                expect: vec![],
            },
            TestCase {
                test: CharRange('i', 's'),
                include_adjacent: true,
                expect: vec![
                    (1, CharRange('f', 'h')),
                    (2, CharRange('j', 'm')),
                    (3, CharRange('t', 'w')),
                ],
            },
            TestCase {
                test: CharRange('i', 's'),
                include_adjacent: false,
                expect: vec![(2, CharRange('j', 'm'))],
            },
            TestCase {
                test: CharRange('0', 'z'),
                include_adjacent: true,
                expect: vec![
                    (0, CharRange('a', 'd')),
                    (1, CharRange('f', 'h')),
                    (2, CharRange('j', 'm')),
                    (3, CharRange('t', 'w')),
                ],
            },
            TestCase {
                test: CharRange('0', '9'),
                include_adjacent: true,
                expect: vec![],
            },
            TestCase {
                test: CharRange('y', 'z'),
                include_adjacent: true,
                expect: vec![],
            },
            TestCase {
                test: CharRange('x', 'z'),
                include_adjacent: true,
                expect: vec![(3, CharRange('t', 'w'))],
            },
            TestCase {
                test: CharRange('x', 'z'),
                include_adjacent: false,
                expect: vec![],
            },
            TestCase {
                test: CharRange(char_sub('a', 4), char_sub('a', 1)),
                include_adjacent: true,
                expect: vec![(0, CharRange('a', 'd'))],
            },
            TestCase {
                test: CharRange(char_sub('a', 4), char_sub('a', 1)),
                include_adjacent: false,
                expect: vec![],
            },
        ];

        for test in tests {
            let actual = group.overlap(&test.test, test.include_adjacent);
            println!(
                "test: {:?} (adj. {}); expected: {:?}; actual: {:?}",
                &test.test, &test.include_adjacent, &test.expect, &actual
            );
            assert_eq!(actual.len(), test.expect.len());
            for (actual, expected) in actual.into_iter().zip(test.expect.into_iter()) {
                assert_eq!(actual.0, expected.0);
                assert_eq!(actual.1, &expected.1);
            }
        }
    }

    #[test]
    fn add_range() {
        struct TestCase {
            add: CharRange,
            expect: Vec<CharRange>,
        }

        // each test is run in order on a mutable `CharGroup`, so the tests are cumulative
        // (the first test operates on an empty `CharGroup`, and the second on the result of the first, etc.)
        let tests = vec![
            TestCase {
                add: CharRange('j', 'l'),
                expect: ranges!['j'..'l'],
            },
            TestCase {
                add: CharRange('h', 'j'),
                expect: ranges!['h'..'l'],
            },
            TestCase {
                add: CharRange('l', 'n'),
                expect: ranges!['h'..'n'],
            },
            TestCase {
                add: CharRange('p', 'r'),
                expect: ranges!['h'..'n', 'p'..'r'],
            },
            TestCase {
                add: CharRange('s', 'u'),
                expect: ranges!['h'..'n', 'p'..'u'],
            },
            TestCase {
                add: CharRange('o', 'w'),
                expect: ranges!['h'..'w'],
            },
            TestCase {
                add: CharRange('a', 'c'),
                expect: ranges!['a'..'c', 'h'..'w'],
            },
            TestCase {
                add: CharRange('e', 'f'),
                expect: ranges!['a'..'c', 'e'..'f', 'h'..'w'],
            },
            TestCase {
                add: CharRange('a', 'c'),
                expect: ranges!['a'..'c', 'e'..'f', 'h'..'w'],
            },
            TestCase {
                add: CharRange('d', 'd'),
                expect: ranges!['a'..'f', 'h'..'w'],
            },
            TestCase {
                add: CharRange('f', 'h'),
                expect: ranges!['a'..'w'],
            },
            TestCase {
                add: CharRange('x', 'z'),
                expect: ranges!['a'..'z'],
            },
            TestCase {
                add: CharRange('\u{D7FE}', '\u{D7FF}'),
                expect: ranges!['a'..'z', '\u{D7FE}'..'\u{D7FF}'],
            },
            TestCase {
                add: CharRange('\u{E000}', '\u{E001}'),
                expect: ranges!['a'..'z', '\u{D7FE}'..'\u{E001}'],
            },
        ];

        let mut group = CharGroup::new();
        for test in tests {
            group.add_range(test.add.clone());
            println!(
                "test: {:?}; expected: {:?}; actual: {:?}",
                test.add, &test.expect, &group.ranges
            );
            assert_eq!(group.ranges, test.expect);
        }
    }

    #[test]
    fn remove_range() {
        struct TestCase {
            remove: CharRange,
            expect: Vec<CharRange>,
        }

        // each test is run in order on a full `CharGroup`, so the tests are cumulative
        // (the first test is run against a full `CharGroup`, the second on the result from the first, etc.)
        let tests = vec![
            TestCase {
                remove: CharRange('h', 'j'),
                expect: ranges!['\0'..'g', 'k'..char::MAX],
            },
            TestCase {
                remove: CharRange('l', 'n'),
                expect: ranges!['\0'..'g', 'k'..'k', 'o'..char::MAX],
            },
            TestCase {
                remove: CharRange('l', 'n'),
                expect: ranges!['\0'..'g', 'k'..'k', 'o'..char::MAX],
            },
            TestCase {
                remove: CharRange('m', 'p'),
                expect: ranges!['\0'..'g', 'k'..'k', 'q'..char::MAX],
            },
            TestCase {
                remove: CharRange('e', 'g'),
                expect: ranges!['\0'..'d', 'k'..'k', 'q'..char::MAX],
            },
            TestCase {
                remove: CharRange('c', 'e'),
                expect: ranges!['\0'..'b', 'k'..'k', 'q'..char::MAX],
            },
            TestCase {
                remove: CharRange('r', 'r'),
                expect: ranges!['\0'..'b', 'k'..'k', 'q'..'q', 's'..char::MAX],
            },
            TestCase {
                remove: CharRange('s', 's'),
                expect: ranges!['\0'..'b', 'k'..'k', 'q'..'q', 't'..char::MAX],
            },
            TestCase {
                remove: CharRange('u', 'v'),
                expect: ranges!['\0'..'b', 'k'..'k', 'q'..'q', 't'..'t', 'w'..char::MAX],
            },
            TestCase {
                remove: CharRange('k', 'k'),
                expect: ranges!['\0'..'b', 'q'..'q', 't'..'t', 'w'..char::MAX],
            },
            TestCase {
                remove: CharRange('p', 's'),
                expect: ranges!['\0'..'b', 't'..'t', 'w'..char::MAX],
            },
            TestCase {
                remove: CharRange('y', 'y'),
                expect: ranges!['\0'..'b', 't'..'t', 'w'..'x', 'z'..char::MAX],
            },
            TestCase {
                remove: CharRange('b', 'y'),
                expect: ranges!['\0'..'a', 'z'..char::MAX],
            },
            TestCase {
                remove: CharRange('\u{E000}', '\u{E001}'),
                expect: ranges!['\0'..'a', 'z'..'\u{D7FF}', '\u{E002}'..char::MAX,],
            },
        ];

        let mut group = CharGroup::all();
        for test in tests {
            group.remove_range(test.remove.clone());
            println!(
                "test: {:?}; expected: {:?}; actual: {:?}",
                test.remove, &test.expect, &group.ranges
            );
            assert_eq!(test.expect, group.ranges);
        }
    }
}
