use std::fmt;

use aglet_text::Span;

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Literal(char),
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
    Number(u32),
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
    RangeSymmetrical,
    RangeDifference,
    RangeIntersection,
    UnicodeShort(char, bool),
    UnicodeLongStart(bool),
    UnicodeLongEnd,
    UnicodePropName(String),
    UnicodeEqual(bool),
    UnicodePropValue(String),
    Digit(bool),
    Whitespace(bool),
    WordChar(bool),
    ClassName(String, bool),
}

impl Token {
    pub fn new_with_offsets(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            span: Span::from_offsets(start, end),
        }
    }
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

    pub fn is_range_symmetrical(&self) -> bool {
        matches!(self, TokenKind::RangeSymmetrical)
    }

    pub fn is_range_difference(&self) -> bool {
        matches!(self, TokenKind::RangeDifference)
    }

    pub fn is_range_intersection(&self) -> bool {
        matches!(self, TokenKind::RangeIntersection)
    }

    pub fn is_class_name(&self) -> bool {
        matches!(self, TokenKind::ClassName(_, _))
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
