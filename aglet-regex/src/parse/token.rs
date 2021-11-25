use super::span::Span;

pub enum TokenKind {
    Literal(char),
    Or,

    // Character classes
    OpenBracket,
    CloseBracket,
    Negated,
    Range,
    RangeSubtract,
    RangeDifference,
    RangeIntersection,
    Any,
    Digit,
    NonDigit,
    AlphaNum,
    NonAlphaNum,
    Whitespace,
    NonWhitespace,
    Bell,
    Tab,
    Return,
    Linefeed,
    VTab,
    FormFeed,
    Backspace,
    Null,
    NamedClass(String),
    NonNamedClass(String),
    Control(char),
    CharCode(i8),
    UTF16(i16),
    Codepoint(i32),
    UnicodeProperty(String),
    NonUnicodeProperty(String),

    // Boundaries
    InputStart,
    MultilineInputStart,
    InputEnd,
    MultilineInputEnd,
    WordBoundary,
    NonWordBoundary,

    // Quantifiers
    OpenBrace,
    CloseBrace,
    Number(i32),
    Comma,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    Lazy,

    // Groups
    OpenGroup,
    CloseGroup,
    NonCapturing,
    Name(String),
    BackReference(i32),
    NamedBackReference(String),
    Flags(Vec<char>),
    NonCapturingFlags(Vec<char>),
}

pub struct Token<'a> {
    kind: TokenKind,
    span: self::Span<'a>,
}
