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
    Posix(PosixClass),
    Unicode(UnicodeClass),
    Specified(ClassSpec),
}

pub struct PosixClass {
    pub span: Span,
    pub kind: PosixKind,
}

pub enum PosixKind {
    Upper,
    Lower,
    Alpha,
    Digit,
    XDigit,
    AlNum,
    Punct,
    Blank,
    Space,
    Cntrl,
    Graph,
    Print,
    Word,
}

impl TryFrom<&str> for PosixKind {
    type Error = TokenConvertError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "upper" => Ok(Self::Upper),
            "lower" => Ok(Self::Lower),
            "alpha" => Ok(Self::Alpha),
            "digit" => Ok(Self::Digit),
            "xdigit" => Ok(Self::XDigit),
            "alnum" => Ok(Self::AlNum),
            "punct" => Ok(Self::Punct),
            "blank" => Ok(Self::Blank),
            "space" => Ok(Self::Space),
            "cntrl" => Ok(Self::Cntrl),
            "graph" => Ok(Self::Graph),
            "print" => Ok(Self::Print),
            "word" => Ok(Self::Word),
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
    pub span:  Span,
    pub items: Vec<ClassSpecKind>,
}

pub enum ClassSpecKind {
    Intersection(ClassSpec, ClassSpec),
    Difference(ClassSpec, ClassSpec),
    Symmetric(ClassSpec, ClassSpec),
    Literal(char),
    Range(char, char),
    Class(ClassKind),
    Union(ClassSpec),
}

pub struct StringSpan {
    pub span:  Span,
    pub value: String,
}
