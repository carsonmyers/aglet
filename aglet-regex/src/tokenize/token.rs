use std::fmt;

use aglet_text::{Cursor, Span};

use crate::tokenize::error::Error;
use crate::tokenize::state::StateStack;

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
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
    /// A literal character to be matched, or as part of a character class
    Literal(char),

    /// Digit character class `\d`--or negated as `\D`--either to be matched
    /// or as a subclass in a specified character class (e.g. `[\d]`).
    ///
    /// # Items
    ///
    /// * `0` - whether the class is negated
    Digit(bool),

    /// Whitespace character class `\s`--or negated as `\S`--either to be matched
    /// or as a subclass in a specified character class (e.g., `[\s]`).
    ///
    /// # Items
    ///
    /// * `0` - whether the class is negated
    Whitespace(bool),

    /// Word character class `\w`--or negated as `\W`--either to be matched
    /// or as a subclass in a specified character class (e.g., `[\w]`).
    ///
    /// # Items
    ///
    /// * `0` - whether the class is negated
    WordChar(bool),

    /// "Any" character class `.` matching any input character
    Dot,

    /// Alternation operator `|` introducing another possible match
    Alternate,

    // Boundaries ==================================================
    /// Start of line anchor `^`
    StartOfLine,

    /// End of line anchor `$`
    EndOfLine,

    /// Start of text anchor `\A`
    StartOfText,

    /// End of text anchor `\z`
    EndOfText,

    /// Word boundary `\b`
    WordBoundary,

    /// Non-word boundary `\B`
    NonWordBoundary,

    // Repetition ==================================================
    /// Open brace `{` starting a specified repetition range (e.g. `.{1, 2}`)
    OpenBrace,

    /// Close brace `}` ending a specified repetition range (e.g. `.{1, 2}`)
    CloseBrace,

    /// A number inside a specified repetition range (e.g. `.{1, 2}`)
    ///
    /// # Items
    ///
    /// * `0` - value of the number
    Number(usize),

    /// Comma separating the start and end of a repetition range (e.g. `.{1, 2}`)
    Comma,

    /// Zero-or-one repetition `?`
    Question,

    /// Zero-or-more repetition `*`
    Star,

    /// One-or-more repetition `+`
    Plus,

    // Groups ==================================================
    /// Beginning of a group `(`
    OpenGroup,

    /// End of a group `)`
    CloseGroup,

    /// Non-capturing group tag `?:` (e.g. `(?:.*)`)
    NonCapturing,

    /// Group name tag `?P<Name>` (e.g. `(?P<Name>.*)`)
    ///
    /// # Items
    ///
    /// * `0` - group name
    Name(String),

    /// Flags group tag (e.g. `(?i-x)`)
    ///
    /// A flag group has no contents and matches no characters. Instead, it uses flags
    /// to alter the behaviour of the enclosing group (or the whole expression if not in
    /// an enclosing group)
    ///
    /// # Items
    ///
    /// * `0` - flags to be set (start taking effect)
    /// * `1` - flags to be cleared (stop taking effect)
    Flags(Vec<char>, Vec<char>),

    /// Non-capturing flags group tag (e.g. `(?i-x:.*)`)
    ///
    /// Flags set in this manner apply only to the non-capturing group in which they're
    /// specified, i.e. the opposite behaviour of the [flag group](TokenKind::Flags).
    ///
    /// # Items
    ///
    /// * `0` - flags to be set (start taking effect)
    /// * `1` - flags to be cleared (stop taking effect)
    NonCapturingFlags(Vec<char>, Vec<char>),

    // Character classes ==================================================
    /// Open bracket `[` beginning a specified character class or a posix class
    /// (e.g. `[a-z]`, `[[:alpha:]]`])
    OpenBracket,

    /// Close bracket `]` ending a specified character class or a posix class
    /// (e.g. `[a-z]`, `[[:alpha:]]`)
    CloseBracket,

    /// Negation marker `^` negating a specified character class or a posix class
    /// (e.g. `[^a-z]`, `[[:^alpha:]]`)
    Negated,

    /// Character range operator `-` for specified classes (e.g. `[a-z]`)
    Range,

    /// Symmetrical difference set operator `~~` for specified classes (e.g. `[a-d~~c-f]`)
    Symmetrical,

    /// Difference set operator `--` for specified classes (e.g. `[a-z--h-l]`)
    Difference,

    /// Intersection set operator `&&` for specified classes (e.g. `[a-d&&c-f]`)
    Intersection,

    /// Short unicode character class `\uL`--or negated as `\UL`--to be matched (not
    /// available in specified character classes).
    ///
    /// # Items
    ///
    /// * `0` - single-character unicode General Category specified (e.g. `L` for Letter)
    /// * `1` - whether the class is negated
    UnicodeShort(char, bool),

    /// Beginning of a long unicode character class specifier `\u{`--or negated as `\U{`--to
    /// be matched (not available in specified character classes). e.g. `\u{sc=Greek}`
    ///
    /// # Items
    ///
    /// * `0` - whether the class is negated
    UnicodeLongStart(bool),

    /// End of a long unicode character class specifier `}` (e.g. `\u{sc=Greek}`)
    UnicodeLongEnd,

    /// Property name for a long unicode class specifier, appearing on the left side of
    /// the equal (or inequal) sign if present (e.g. `\u{Script=Greek}`)
    ///
    /// # Items
    ///
    /// * `0` - property name
    UnicodePropName(String),

    /// Equal sign `=`--or negated as `!=`--for a long unicode class specifier
    /// (e.g. `\u{sc!=Greek}`)
    ///
    /// # Items
    ///
    /// * `0` - whether the equals sign is negated
    UnicodeEqual(bool),

    /// Property value for a long unicode class specifier, appearing on the right side
    /// of the equal (or inequal) sign if present, or as the only token within the
    /// start- and end-tokens (e.g. `\u{sc=Greek}`).
    ///
    /// # Items
    ///
    /// * `0` - property value
    UnicodePropValue(String),

    /// Name of a posix character class (e.g. `[[:alpha:]]`, negated as `[[:^alpha:]]`)
    ///
    /// # Items
    ///
    /// * `0` - class name
    /// * `1` - whether the class is negated
    ClassName(String, bool),

    /// Error generated during tokenization
    ///
    /// # Items
    ///
    /// * `0` - error that was generated
    Error(Error),
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
