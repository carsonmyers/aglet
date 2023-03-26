use std::fmt;

use aglet_text::Span;

use crate::tokenize::state::StateStack;

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new_with_offsets(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            span: Span::from_offsets(start, end),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("tok")
            .field(&self.span)
            .field(&self.kind)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Literal(char),
    Digit(bool),
    Whitespace(bool),
    WordChar(bool),
    Dot,
    Alternate,

    // Boundaries
    StartOfLine,
    EndOfLine,
    StartOfText,
    EndOfText,
    WordBoundary,
    NonWordBoundary,

    // Repetition
    OpenBrace,
    CloseBrace,
    Number(usize),
    Comma,
    Question,
    Star,
    Plus,

    // Groups
    OpenGroup,
    CloseGroup,
    NonCapturing,
    Name(String),
    Flags(Vec<char>, Vec<char>),
    NonCapturingFlags(Vec<char>, Vec<char>),

    // Character classes
    OpenBracket,
    CloseBracket,
    Negated,
    Range,
    Symmetrical,
    Difference,
    Intersection,
    UnicodeShort(char, bool),
    UnicodeLongStart(bool),
    UnicodeLongEnd,
    UnicodePropName(String),
    UnicodeEqual(bool),
    UnicodePropValue(String),
    ClassName(String, bool),
}

impl TokenKind {
    pub fn is_literal(&self) -> bool {
        matches!(self, TokenKind::Literal(_))
    }

    pub fn is_any(&self) -> bool {
        matches!(self, TokenKind::Dot)
    }

    pub fn is_alternate(&self) -> bool {
        matches!(self, TokenKind::Alternate)
    }

    pub fn is_boundary(&self) -> bool {
        match self {
            TokenKind::StartOfLine
            | TokenKind::EndOfLine
            | TokenKind::StartOfText
            | TokenKind::EndOfText
            | TokenKind::WordBoundary
            | TokenKind::NonWordBoundary => true,
            _ => false,
        }
    }

    pub fn is_range(&self) -> bool {
        matches!(self, TokenKind::Range)
    }

    pub fn is_symmetrical(&self) -> bool {
        matches!(self, TokenKind::Symmetrical)
    }

    pub fn is_difference(&self) -> bool {
        matches!(self, TokenKind::Difference)
    }

    pub fn is_intersection(&self) -> bool {
        matches!(self, TokenKind::Intersection)
    }

    pub fn is_class_name(&self) -> bool {
        matches!(self, TokenKind::ClassName(_, _))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStack {
    pub token: Token,
    pub stack: StateStack,
}
