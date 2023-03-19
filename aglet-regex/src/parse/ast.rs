use std::convert::TryFrom;

use aglet_text::Span;

use crate::parse::error::TokenConvertError;
use crate::tokenize::TokenKind;

#[derive(Default)]
pub struct Ast {
    pub head: Option<Expr>,
}

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Alternation(Alternation),
    Concatenation(Vec<Expr>),
    Repetition(Repetition),
    Any,
    Literal(char),
    Boundary(Boundary),
    Group(Group),
    Class(Class),
}

pub struct Alternation {
    pub span:  Span,
    pub exprs: Vec<Expr>,
}

pub struct Concatenation {
    pub span:  Span,
    pub exprs: Vec<Expr>,
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
    Range(Option<usize>, Option<usize>),
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

pub struct Boundary {
    pub span: Span,
    pub kind: BoundaryKind,
}

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
    pub expr: Box<Expr>,
}

pub enum GroupKind {
    Capturing(usize),
    Named(StringSpan),
    NonCapturing,
}

pub struct Class {
    pub span:    Span,
    pub negated: bool,
    pub kind:    ClassKind,
}

pub enum ClassKind {
    Unicode(UnicodeClass),
    Specified(Vec<ClassSpec>),
}

pub struct PosixClass {
    pub span: Span,
    pub kind: PosixKind,
}

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

pub struct UnicodeClass {
    pub span:  Span,
    pub name:  Option<StringSpan>,
    pub value: StringSpan,
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

pub struct StringSpan {
    pub span:  Span,
    pub value: String,
}
