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
    Flags(Vec<char>),

    // Character classes
    OpenBracket,
    CloseBracket,
    Negated,
    Range,
    RangeSymmetrical,
    RangeDifference,
    RangeIntersection,
    UnicodeShort(char, bool),
    UnicodeLong(String, bool),
    Digit(bool),
    Whitespace(bool),
    WordChar(bool),
    ClassName(String, bool),
}