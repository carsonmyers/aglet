use std::convert::TryFrom;
use std::fmt;

use aglet_text::{Cursor, Span};

use crate::tokenize::error::{Error, ErrorKind};
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
pub enum Flag {
    /// `i`: Letters match regardless of case
    CaseInsensitive,

    /// `m`: `^` and `$` match the beginning and end of a line
    MultiLine,

    /// `s`: allow `.` to match `\n`
    DotMatchesNewline,

    /// `R`: use `\r\n` when multi-line mode is enabled
    CRLFMode,

    /// `U`: swap the meaning of `x*` and `x*?`
    SwapGreed,

    /// `u`: enable unicode support
    Unicode,

    /// `x`: ignore whitespace and enable line comments (beginning with `#`)
    IgnoreWhitespace,
}

impl TryFrom<char> for Flag {
    type Error = ErrorKind;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'i' => Ok(Self::CaseInsensitive),
            'm' => Ok(Self::MultiLine),
            's' => Ok(Self::DotMatchesNewline),
            'R' => Ok(Self::CRLFMode),
            'U' => Ok(Self::SwapGreed),
            'u' => Ok(Self::Unicode),
            'x' => Ok(Self::IgnoreWhitespace),
            c => Err(ErrorKind::UnrecognizedFlag(c)),
        }
    }
}

impl Flag {
    pub fn is_flag_char(c: char) -> bool {
        match c {
            'i' | 'm' | 's' | 'U' | 'u' | 'x' => true,
            _ => false,
        }
    }

    pub fn is_ignore_whitespace(&self) -> bool {
        matches!(self, Self::IgnoreWhitespace)
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

    /// Start of group options `?` in e.g. `(?<name>...)`
    OpenGroupOptions,
    
    /// End of group options `:` in e.g. `(?isx:...)`
    CloseGroupOptions,

    /// Start of group name `<` or `P<` in e.g. `(?P<name>...)`
    OpenGroupName,

    /// End of group name `>` in e.g. `(?<name>...)`
    CloseGroupName,

    /// Group or class name
    ///
    /// # Items
    ///
    /// * `0` - group name
    Name(String),

    /// Flags in group options or a non-capturing flags group (e.g. `(?i-x)`)
    Flag(Flag),

    /// Delimiter character `-` between flags to be set and flags to be cleared (e.g. `(?i-x)`)
    FlagDelimiter,

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
    /// the equal (or unequal) sign if present (e.g. `\u{Script=Greek}`)
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
    /// of the equal (or unequal) sign if present, or as the only token within the
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
    #[inline]
    pub fn is_literal(&self) -> bool {
        matches!(self, TokenKind::Literal(_))
    }

    #[inline]
    pub fn is_digit(&self) -> bool {
        matches!(self, TokenKind::Digit(_))
    }

    #[inline]
    pub fn is_whitespace(&self) -> bool {
        matches!(self, TokenKind::Whitespace(_))
    }

    #[inline]
    pub fn is_word_char(&self) -> bool {
        matches!(self, TokenKind::WordChar(_))
    }

    #[inline]
    pub fn is_dot(&self) -> bool {
        matches!(self, TokenKind::Dot)
    }

    #[inline]
    pub fn is_alternate(&self) -> bool {
        matches!(self, TokenKind::Alternate)
    }

    #[inline]
    pub fn is_start_of_line(&self) -> bool {
        matches!(self, TokenKind::StartOfLine)
    }

    #[inline]
    pub fn is_end_of_line(&self) -> bool {
        matches!(self, TokenKind::EndOfLine)
    }

    #[inline]
    pub fn is_start_of_text(&self) -> bool {
        matches!(self, TokenKind::StartOfText)
    }

    #[inline]
    pub fn is_end_of_text(&self) -> bool {
        matches!(self, TokenKind::EndOfText)
    }

    #[inline]
    pub fn is_word_boundary(&self) -> bool {
        matches!(self, TokenKind::WordBoundary)
    }

    #[inline]
    pub fn is_non_word_boundary(&self) -> bool {
        matches!(self, TokenKind::NonWordBoundary)
    }

    #[inline]
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

    #[inline]
    pub fn is_open_brace(&self) -> bool {
        matches!(self, TokenKind::OpenBrace)
    }

    #[inline]
    pub fn is_close_brace(&self) -> bool {
        matches!(self, TokenKind::CloseBrace)
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, TokenKind::Number(_))
    }

    #[inline]
    pub fn is_comma(&self) -> bool {
        matches!(self, TokenKind::Comma)
    }

    #[inline]
    pub fn is_question(&self) -> bool {
        matches!(self, TokenKind::Question)
    }

    #[inline]
    pub fn is_star(&self) -> bool {
        matches!(self, TokenKind::Star)
    }

    #[inline]
    pub fn is_plus(&self) -> bool {
        matches!(self, TokenKind::Plus)
    }

    #[inline]
    pub fn is_open_group(&self) -> bool {
        matches!(self, TokenKind::OpenGroup)
    }

    #[inline]
    pub fn is_close_group(&self) -> bool {
        matches!(self, TokenKind::CloseGroup)
    }

    #[inline]
    pub fn is_open_group_options(&self) -> bool {
        matches!(self, TokenKind::OpenGroupOptions)
    }

    #[inline]
    pub fn is_close_group_options(&self) -> bool {
        matches!(self, TokenKind::CloseGroupOptions)
    }

    #[inline]
    pub fn is_open_group_name(&self) -> bool {
        matches!(self, TokenKind::OpenGroupName)
    }

    #[inline]
    pub fn is_close_group_name(&self) -> bool {
        matches!(self, TokenKind::CloseGroupName)
    }

    #[inline]
    pub fn is_name(&self) -> bool {
        matches!(self, TokenKind::Name(_))
    }

    #[inline]
    pub fn is_flag(&self) -> bool {
        matches!(self, TokenKind::Flag(_))
    }

    #[inline]
    pub fn is_flag_delimiter(&self) -> bool {
        matches!(self, TokenKind::FlagDelimiter)
    }

    #[inline]
    pub fn is_flag_or_delimiter(&self) -> bool {
        self.is_flag() || self.is_flag_delimiter()
    }

    #[inline]
    pub fn is_open_bracket(&self) -> bool {
        matches!(self, TokenKind::OpenBracket)
    }

    #[inline]
    pub fn is_close_bracket(&self) -> bool {
        matches!(self, TokenKind::CloseBracket)
    }

    #[inline]
    pub fn is_negated(&self) -> bool {
        matches!(self, TokenKind::Negated)
    }

    #[inline]
    pub fn is_range(&self) -> bool {
        matches!(self, TokenKind::Range)
    }

    #[inline]
    pub fn is_symmetrical(&self) -> bool {
        matches!(self, TokenKind::Symmetrical)
    }

    #[inline]
    pub fn is_difference(&self) -> bool {
        matches!(self, TokenKind::Difference)
    }

    #[inline]
    pub fn is_intersection(&self) -> bool {
        matches!(self, TokenKind::Intersection)
    }

    #[inline]
    pub fn is_set_operator(&self) -> bool {
        match self {
            TokenKind::Symmetrical
            | TokenKind::Difference
            | TokenKind::Intersection => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_unicode_short(&self) -> bool {
        matches!(self, TokenKind::UnicodeShort(_, _))
    }

    #[inline]
    pub fn is_unicode_long_start(&self) -> bool {
        matches!(self, TokenKind::UnicodeLongStart(_))
    }

    #[inline]
    pub fn is_unicode_long_end(&self) -> bool {
        matches!(self, TokenKind::UnicodeLongEnd)
    }

    #[inline]
    pub fn is_unicode_prop_name(&self) -> bool {
        matches!(self, TokenKind::UnicodePropName(_))
    }

    #[inline]
    pub fn is_unicode_equal(&self) -> bool {
        matches!(self, TokenKind::UnicodeEqual(_))
    }

    #[inline]
    pub fn is_unicode_prop_value(&self) -> bool {
        matches!(self, TokenKind::UnicodePropValue(_))
    }

    #[inline]
    pub fn is_class_name(&self) -> bool {
        matches!(self, TokenKind::ClassName(_, _))
    }

    #[inline]
    pub fn is_error(&self) -> bool {
        matches!(self, TokenKind::Error(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStack {
    pub token: Token,
    pub stack: StateStack,
}
