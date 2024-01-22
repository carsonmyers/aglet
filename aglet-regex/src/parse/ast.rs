use std::convert::TryFrom;
use std::default::Default;

use aglet_derive::DefaultWithSpan;
use aglet_text::{Span, StringSpan};

use crate::parse::error;
use crate::tokenize::{self, TokenKind};

macro_rules! unwrap_kind_fn {
    ( $name:ident , $variant:path , $result:ty ) => {
        pub fn $name(&self) -> Option<&$result> {
            if let $variant($name) = &self.kind {
                Some($name)
            } else {
                None
            }
        }
    };
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct ParseResult {
    pub ast:    Expr,
    pub errors: Vec<error::Error>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    unwrap_kind_fn!(alternation, ExprKind::Alternation, Alternation);
    unwrap_kind_fn!(concatenation, ExprKind::Concatenation, Concatenation);
    unwrap_kind_fn!(repetition, ExprKind::Repetition, Repetition);
    unwrap_kind_fn!(literal, ExprKind::Literal, char);
    unwrap_kind_fn!(digit, ExprKind::Digit, bool);
    unwrap_kind_fn!(whitespace, ExprKind::Whitespace, bool);
    unwrap_kind_fn!(word_char, ExprKind::WordChar, bool);
    unwrap_kind_fn!(boundary, ExprKind::Boundary, Boundary);
    unwrap_kind_fn!(group, ExprKind::Group, Group);
    unwrap_kind_fn!(class, ExprKind::Class, Class);

    pub fn capturing_group(&self) -> Option<&CapturingGroup> {
        let Some(group) = self.group() else {
            return None;
        };

        group.capturing_group()
    }

    pub fn named_group(&self) -> Option<&NamedGroup> {
        let Some(group) = self.group() else {
            return None;
        };

        group.named_group()
    }

    pub fn non_capturing_group(&self) -> Option<&NonCapturingGroup> {
        let Some(group) = self.group() else {
            return None;
        };

        group.non_capturing_group()
    }

    pub fn flag_group(&self) -> Option<&FlagGroup> {
        let Some(group) = self.group() else {
            return None;
        };

        group.flag_group()
    }
}

#[derive(Default, DefaultWithSpan, Debug)]
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

#[derive(Debug)]
pub struct Repetition {
    pub span:   Span,
    pub kind:   RepetitionKind,
    pub greedy: bool,
    pub item:   Box<Expr>,
}

#[derive(Debug)]
pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Range(Range),
}

impl TryFrom<TokenKind> for RepetitionKind {
    type Error = error::TokenConvertError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Question => Ok(Self::ZeroOrOne),
            TokenKind::Star => Ok(Self::ZeroOrMore),
            TokenKind::Plus => Ok(Self::OneOrMore),
            _ => Err(error::TokenConvertError::InvalidTokenForRepetition(value)),
        }
    }
}

#[derive(Default, DefaultWithSpan, Debug)]
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
    type Error = error::TokenConvertError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::StartOfLine => Ok(Self::StartOfLine),
            TokenKind::EndOfLine => Ok(Self::EndOfLine),
            TokenKind::StartOfText => Ok(Self::StartOfText),
            TokenKind::EndOfText => Ok(Self::EndOfText),
            TokenKind::WordBoundary => Ok(Self::WordBoundary),
            TokenKind::NonWordBoundary => Ok(Self::NonWordBoundary),
            _ => Err(error::TokenConvertError::InvalidTokenForBoundary(value)),
        }
    }
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Group {
    pub span: Span,
    pub kind: GroupKind,
}

impl Group {
    unwrap_kind_fn!(capturing_group, GroupKind::Capturing, CapturingGroup);
    unwrap_kind_fn!(named_group, GroupKind::Named, NamedGroup);
    unwrap_kind_fn!(
        non_capturing_group,
        GroupKind::NonCapturing,
        NonCapturingGroup
    );
    unwrap_kind_fn!(flag_group, GroupKind::Flags, FlagGroup);
}

#[derive(Default, DefaultWithSpan, Debug)]
pub enum GroupKind {
    Capturing(CapturingGroup),
    Named(NamedGroup),
    NonCapturing(NonCapturingGroup),
    Flags(FlagGroup),
    #[default]
    Empty,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct CapturingGroup {
    pub span:  Span,
    pub index: usize,
    pub expr:  Box<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct NamedGroup {
    pub span: Span,
    pub name: StringSpan,
    pub expr: Box<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct NonCapturingGroup {
    pub span:  Span,
    pub flags: Option<Flags>,
    pub expr:  Box<Expr>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct FlagGroup {
    pub flags: Flags,
}

#[derive(Default, DefaultWithSpan, Debug)]
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

#[derive(Default, DefaultWithSpan, Debug)]
pub struct UnicodeClass {
    pub span:  Span,
    pub name:  Option<StringSpan>,
    pub value: StringSpan,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct SpecifiedClass {
    pub span:  Span,
    pub items: Vec<ClassSpec>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct ClassSpec {
    pub span: Span,
    pub kind: ClassSpecKind,
}

#[derive(Default, DefaultWithSpan, Debug)]
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
    #[default]
    Empty,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Intersection {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Difference {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct Symmetrical {
    pub span:  Span,
    pub left:  Box<ClassSpec>,
    pub right: Box<ClassSpec>,
}

#[derive(Default, DefaultWithSpan, Debug)]
pub struct PosixClass {
    pub span:    Span,
    pub kind:    PosixKind,
    pub negated: bool,
}

#[derive(Default, DefaultWithSpan, Debug)]
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
    #[default]
    Unknown,
}

impl TryFrom<&str> for PosixKind {
    type Error = error::TokenConvertError;

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
            _ => Err(error::TokenConvertError::InvalidPosixClass(
                value.to_string(),
            )),
        }
    }
}
