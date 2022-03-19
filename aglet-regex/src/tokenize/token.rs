use std::fmt;
use aglet_text::Span;

pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Literal(char),
    Any,
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
    ZeroOrMore,
    OneOrMore,

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

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("tok")
            .field(&self.span)
            .field(&self.kind)
            .finish()
    }
}