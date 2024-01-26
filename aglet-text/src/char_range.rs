use std::ops::{Range, RangeBounds, RangeInclusive};

pub trait CharGroup {
    fn start(&self) -> Option<&char>;
    fn end(&self) -> Option<&char>;
    fn contains(&self, c: &char) -> bool;
}

pub struct CharRange {
    ranges: Vec<RangeInclusive<char>>,
}

impl CharRange {
    pub const fn empty() -> Self {
        Self { ranges: Vec::new() }
    }

    pub fn all() -> Self {
        Self {
            ranges: vec!['\0'..=char::MAX],
        }
    }

    pub fn start(&self) -> Option<&char> {
        self.ranges.first().map(|range| range.start())
    }

    pub fn end(&self) -> Option<&char> {
        self.ranges.last().map(|range| range.end())
    }

    pub fn add(&mut self, range: RangeInclusive<char>) {
        let overlap = self.overlap(&range);

        let (splice_range, replace_with) = if !overlap.is_empty() {
            let (start_i, start) = overlap.first().map(|(i, r)| (i, r.start())).unwrap();
            let (end_i, end) = overlap.last().map(|(i, r)| (i, r.end())).unwrap();

            (*start_i..=*end_i, *start..=*end)
        } else {
            let idx = self
                .ranges
                .iter()
                .position(|existing| range.start() > existing.end())
                .unwrap_or(self.ranges.len());
            (idx..=idx, range)
        };

        self.ranges.splice(splice_range, [replace_with]);
    }

    pub fn remove(&mut self, range: RangeInclusive<char>) {
        let overlap = self.overlap(&range);

        if overlap.is_empty() {
            return;
        }

        let start_i = *overlap.first().map(|(i, _)| i).unwrap();
        let end_i = *overlap.last().map(|(i, _)| i).unwrap();
        let mut replace_with = Vec::new();
        for (_, existing) in &overlap {
            if existing.contains(range.start()) {
                let end = char_sub(*range.start(), 1);
                replace_with.push(*existing.start()..=end);
            }

            if existing.contains(range.end()) {
                let start = char_add(*range.end(), 1);
                replace_with.push(start..=*existing.end());
            }
        }

        self.ranges.splice(start_i..=end_i, replace_with);
    }

    pub fn contains(&self, c: char) -> bool {
        self.ranges.iter().any(|range| range.contains(&c))
    }

    fn overlap(&self, range: &RangeInclusive<char>) -> Vec<(usize, &RangeInclusive<char>)> {
        self.ranges
            .iter()
            .enumerate()
            .filter(|(_, existing)| {
                existing.start() <= range.end() && existing.end() >= range.start()
            })
            .collect()
    }
}

const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

const fn char_add(c: char, amount: u32) -> char {
    let next_val = c as u32 + amount;
    let next_val = if SURROGATE_START <= next_val && next_val <= SURROGATE_END {
        SURROGATE_END + 1 + (next_val - SURROGATE_START)
    } else if next_val > char::MAX as u32 {
        char::MAX as u32
    } else {
        next_val
    };

    unsafe { std::mem::transmute(next_val) }
}

const fn char_sub(c: char, amount: u32) -> char {
    let num_val = c as u32;
    if amount > num_val {
        return '\0';
    }

    let next_val = num_val - amount;
    let next_val = if SURROGATE_START <= next_val && next_val <= SURROGATE_END {
        SURROGATE_START - 1 - (SURROGATE_END - next_val)
    } else {
        next_val
    };

    unsafe { std::mem::transmute(next_val) }
}
