use std::collections::HashSet;
use std::ops::Range;

use aglet_derive::DefaultWithSpan;
use aglet_text::{Span, StringSpan};

use crate::lowering::error;

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Expr {
    pub span:  Span,
    pub flags: HashSet<Flag>,
    pub kind:  ExprKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Flag {
    CaseInsensitive,
    MultiLine,
    DotMatchesNewline,
    CRLFMode,
    SwapGreed,
    Unicode,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub enum ExprKind {
    Alternation(Alternation),
    Concatenation(Concatenation),
    Repetition(Repetition),
    Group(Group),
    Class(Class),
    #[default]
    Empty,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Alternation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Concatenation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Repetition {
    pub span:   Span,
    pub lower:  usize,
    pub upper:  Option<usize>,
    pub greedy: bool,
    pub item:   Box<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Group {
    pub span:          Span,
    pub capture_index: Option<usize>,
    pub capture_name:  Option<StringSpan>,
    pub item:          Box<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Class {
    pub span:   Span,
    pub ranges: Vec<ClassRange>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct ClassRange {
    pub span:  Span,
    pub range: Range<char>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct LowerResult {
    pub ir:     Expr,
    pub errors: Vec<error::Error>,
}
