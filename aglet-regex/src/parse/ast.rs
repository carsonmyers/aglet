use std::convert::TryFrom;

use aglet_text::Span;

use crate::parse::error::TokenConvertError;
use crate::tokenize::{self, TokenKind};

#[derive(Default, Debug)]
pub struct Ast {
    pub head: Expr,
}

#[derive(Default, Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Default, Debug)]
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

#[derive(Debug)]
pub struct Alternation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

#[derive(Debug)]
pub struct Concatenation {
    pub span:  Span,
    pub items: Vec<Expr>,
}

#[derive(Debug)]
pub struct Repetition {
    pub span: Span,
    pub kind: RepetitionKind,
    pub item: Box<Expr>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Range {
    pub span:  Span,
    pub start: Option<usize>,
    pub end:   Option<usize>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Group {
    pub span: Span,
    pub kind: GroupKind,
}

#[derive(Debug)]
pub enum GroupKind {
    Capturing(CapturingGroup),
    Named(NamedGroup),
    NonCapturing(NonCapturingGroup),
    Flags(FlagGroup),
}

#[derive(Debug)]
pub struct CapturingGroup {
    pub span:  Span,
    pub index: usize,
    pub expr:  Box<Expr>,
}

#[derive(Debug)]
pub struct NamedGroup {
    pub span: Span,
    pub name: StringSpan,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct NonCapturingGroup {
    pub span:  Span,
    pub flags: Option<Flags>,
    pub expr:  Box<Expr>,
}

#[derive(Debug)]
pub struct FlagGroup {
    pub flags: Flags,
}

#[derive(Debug)]
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
    CRLFMode,
    SwapGreed,
    Unicode,
    IgnoreWhitespace,
}

impl From<tokenize::Flag> for FlagKind {

    fn from(value: tokenize::Flag) -> Self {
        match value {
            tokenize::Flag::CaseInsensitive => Self::CaseInsensitive,
            tokenize::Flag::MultiLine => Self::MultiLine,
            tokenize::Flag::DotMatchesNewline => Self::DotMatchesNewline,
            tokenize::Flag::CRLFMode => Self::CRLFMode,
            tokenize::Flag::SwapGreed => Self::SwapGreed,
            tokenize::Flag::Unicode => Self::Unicode,
            tokenize::Flag::IgnoreWhitespace => Self::IgnoreWhitespace,
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub span:    Span,
    pub negated: bool,
    pub kind:    ClassKind,
}

#[derive(Debug)]
pub enum ClassKind {
    Unicode(UnicodeClass),
    Specified(SpecifiedClass),
}

#[derive(Debug)]
pub struct UnicodeClass {
    pub span:  Span,
    pub name:  Option<StringSpan>,
    pub value: StringSpan,
}

#[derive(Debug)]
pub struct SpecifiedClass {
    pub span:  Span,
    pub items: Vec<ClassSpec>,
}

#[derive(Debug)]
pub struct ClassSpec {
    pub span: Span,
    pub kind: ClassSpecKind,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Intersection {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Debug)]
pub struct Difference {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Debug)]
pub struct Symmetrical {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Debug)]
pub struct PosixClass {
    pub span: Span,
    pub kind: PosixKind,
    pub negated: bool,
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

#[derive(Debug)]
pub struct StringSpan {
    pub span:  Span,
    pub value: String,
}
