use std::convert::TryFrom;
use std::fmt;

use aglet_text::Span;

use crate::parse::error::TokenConvertError;
use crate::tokenize::TokenKind;

#[derive(Default)]
pub struct Ast {
    pub head: Expr,
}

#[derive(Default)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Default)]
pub enum ExprKind {
    Alternation(Alternation),
    Concatenation(Concatenation),
    Repetition(Repetition),
    Any,
    Literal(char),
    Digit(bool),
    Whitespace(bool),
    WordChar(bool),
    Boundary(Boundary),
    Group(Group),
    Class(Class),
    #[default]
    Empty,
}

pub struct Alternation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

pub struct Concatenation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

pub struct Repetition {
    pub span: Span,
    pub kind: RepetitionKind,
    pub item: Box<Expr>,
}

pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Range(Range),
}

impl TryFrom<TokenKind> for RepetitionKind {
    type Error = TokenConvertError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Question => Ok(Self::ZeroOrOne),
            TokenKind::Star => Ok(Self::ZeroOrMore),
            TokenKind::Plus => Ok(Self::OneOrMore),
            _ => Err(TokenConvertError::InvalidTokenForRepetition(value)),
        }
    }
}

pub struct Range {
    pub span:  Span,
    pub start: Option<usize>,
    pub end:   Option<usize>,
}

pub struct Boundary {
    pub span: Span,
    pub kind: BoundaryKind,
}

#[derive(Debug)]
pub enum BoundaryKind {
    StartOfLine,
    EndOfLine,
    StartOfText,
    EndOfText,
    WordBoundary,
    NonWordBoundary,
}

impl TryFrom<TokenKind> for BoundaryKind {
    type Error = TokenConvertError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::StartOfLine => Ok(Self::StartOfLine),
            TokenKind::EndOfLine => Ok(Self::EndOfLine),
            TokenKind::StartOfText => Ok(Self::StartOfText),
            TokenKind::EndOfText => Ok(Self::EndOfText),
            TokenKind::WordBoundary => Ok(Self::WordBoundary),
            TokenKind::NonWordBoundary => Ok(Self::NonWordBoundary),
            _ => Err(TokenConvertError::InvalidTokenForBoundary(value)),
        }
    }
}

pub struct Group {
    pub span: Span,
    pub kind: GroupKind,
}

pub enum GroupKind {
    Capturing(CapturingGroup),
    Named(NamedGroup),
    NonCapturing(NonCapturingGroup),
    Flags(FlagGroup),
}

pub struct CapturingGroup {
    pub span:  Span,
    pub index: usize,
    pub expr:  Box<Expr>,
}

pub struct NamedGroup {
    pub span: Span,
    pub name: StringSpan,
    pub expr: Box<Expr>,
}

pub struct NonCapturingGroup {
    pub span:  Span,
    pub flags: Option<Flags>,
    pub expr:  Box<Expr>,
}

pub struct FlagGroup {
    pub flags: Flags,
}

pub struct Flags {
    pub span:        Span,
    pub set_flags:   Vec<FlagKind>,
    pub clear_flags: Vec<FlagKind>,
}

#[derive(Debug, PartialEq)]
pub enum FlagKind {
    CaseInsensitive,
    MultiLine,
    DotMatchesNewline,
    SwapGreed,
    Unicode,
    IgnoreWhitespace,
}

impl TryFrom<char> for FlagKind {
    type Error = TokenConvertError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'i' => Ok(Self::CaseInsensitive),
            'm' => Ok(Self::MultiLine),
            's' => Ok(Self::DotMatchesNewline),
            'U' => Ok(Self::SwapGreed),
            'u' => Ok(Self::Unicode),
            'x' => Ok(Self::IgnoreWhitespace),
            _ => Err(TokenConvertError::InvalidFlag(value)),
        }
    }
}

pub struct Class {
    pub span:    Span,
    pub negated: bool,
    pub kind:    ClassKind,
}

pub enum ClassKind {
    Unicode(UnicodeClass),
    Specified(SpecifiedClass),
}

pub struct UnicodeClass {
    pub span:  Span,
    pub name:  Option<StringSpan>,
    pub value: StringSpan,
}

pub struct SpecifiedClass {
    pub span:  Span,
    pub items: Vec<ClassSpec>,
}

pub struct ClassSpec {
    pub span: Span,
    pub kind: ClassSpecKind,
}

pub enum ClassSpecKind {
    Intersection(Intersection),
    Difference(Difference),
    Symmetrical(Symmetrical),
    Literal(char),
    Digit(bool),
    Whitespace(bool),
    WordChar(bool),
    Range(char, char),
    Posix(PosixClass),
    Class(Class),
}

pub struct Intersection {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

pub struct Difference {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

pub struct Symmetrical {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

pub struct PosixClass {
    pub span: Span,
    pub kind: PosixKind,
}

#[derive(Debug)]
pub enum PosixKind {
    AlNum,
    Alpha,
    Ascii,
    Blank,
    Cntrl,
    Digit,
    Graph,
    Lower,
    Print,
    Punct,
    Space,
    Upper,
    Word,
    XDigit,
}

impl TryFrom<&str> for PosixKind {
    type Error = TokenConvertError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "alnum" => Ok(Self::AlNum),
            "alpha" => Ok(Self::Alpha),
            "ascii" => Ok(Self::Ascii),
            "blank" => Ok(Self::Blank),
            "cntrl" => Ok(Self::Cntrl),
            "digit" => Ok(Self::Digit),
            "graph" => Ok(Self::Graph),
            "lower" => Ok(Self::Lower),
            "print" => Ok(Self::Print),
            "punct" => Ok(Self::Punct),
            "space" => Ok(Self::Space),
            "upper" => Ok(Self::Upper),
            "word" => Ok(Self::Word),
            "xdigit" => Ok(Self::XDigit),
            _ => Err(TokenConvertError::InvalidPosixClass(value.to_string())),
        }
    }
}

pub struct StringSpan {
    pub span:  Span,
    pub value: String,
}
