use std::io::Write;
use std::iter;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::tokenize::error::*;
use crate::tokenize::input::*;
use crate::tokenize::state::*;
use crate::tokenize::token::*;
#[cfg(test)]
use crate::tokenize::{assert_next_err, assert_next_none, assert_next_tok};

/// Tokenizer for a regular expression
pub struct Tokenizer<'a> {
    input:            Input<'a>,
    state:            StateStack,
    last_token_kind:  Option<TokenKind>,
    print_debug_info: bool,
    dbgout:           StandardStream,
}

impl<'a> Tokenizer<'a> {
    /// Create a new regular expression tokenizer
    ///
    /// # Arguments
    ///
    /// * `input` - A string-like object that contains the entire regular
    ///     expression to be tokenized
    ///
    /// # Examples
    ///
    /// ```
    /// use aglet_regex::tokenize;
    /// let _ = tokenize::Tokenizer::new("[a-z-]{1, 4}");
    /// let _ = tokenize::Tokenizer::new("^hello, world$");
    /// ```
    pub fn new<T: Into<&'a str>>(input: T) -> Self {
        Tokenizer {
            input:            Input::new(input),
            state:            StateStack::new(),
            last_token_kind:  None,
            print_debug_info: false,
            dbgout:           StandardStream::stderr(ColorChoice::Always),
        }
    }

    /// Cause the tokenizer to print debugging information for every token
    /// output. This includes each token, the span information, and the
    /// internal state stack.
    ///
    /// # Examples
    ///
    /// ```
    /// use aglet_regex::tokenize::*;
    /// let mut tr = Tokenizer::new("a*");
    /// tr.set_debug();
    ///
    /// // 1:1[0]-1:2[1]                  Literal('a')                   -> [State(Main, [])]
    /// let _ = tr.next_token();
    ///
    /// // 1:2[1]-1:3[2]                  ZeroOrMore                     -> [State(Main, [])]
    /// let _ = tr.next_token();
    ///
    /// // !!!                            Err(EndOfFile)                 -> [State(Main, [])]
    /// let _ = tr.next_token();
    /// ```
    pub fn set_debug(&mut self) {
        self.print_debug_info = true;
    }

    /// Get the next token from the input. A token includes both the kind of the
    /// token along with the kind's associated data (e.g. a literal includes the
    /// literal value) and the span over which the token was found in the input.
    ///
    /// # Examples
    ///
    /// ```
    /// use aglet_regex::tokenize::*;
    /// let mut tr = Tokenizer::new("(?x-i)");
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::OpenGroup, 0, 1)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::Flags(vec!['x'], vec!['i']), 1, 5)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::CloseGroup, 5, 6)),
    /// );
    /// ```
    pub fn next_token(&mut self) -> Result<Token> {
        let token = match self.state.get() {
            Ok(State::Main) => self.next_token_main(),
            Ok(State::Group) => self.next_token_group(),
            Ok(State::Class) => self.next_token_class(),
            Ok(State::ClassName) => self.next_token_class_name(),
            Ok(State::Range) => self.next_token_range(),
            Ok(State::UnicodeProperties) => self.next_token_unicode_properties(),
            Err(err) => Err(self.input.error(err.into())),
        };

        if self.print_debug_info {
            if let Ok(ref tok) = token {
                let _ = writeln!(
                    &mut self.dbgout,
                    "{:<30} {:<30} -> {:?}",
                    format!("{:?}", &tok.span),
                    format!("{:?}", &tok.kind),
                    self.state
                );
            } else {
                let err = token.clone().unwrap_err();
                let _ = self
                    .dbgout
                    .set_color(ColorSpec::new().set_fg(Some(Color::Red)));

                let _ = writeln!(
                    &mut self.dbgout,
                    "{:<30} {:<30} -> {:?}",
                    format!("{:?}", &err.span),
                    format!("{:?}", &err.kind),
                    self.state
                );
            }
        }

        self.last_token_kind = token.as_ref().ok().map(|t| t.kind.clone());

        token
    }

    /// Get a token in the `Main` state
    ///
    /// This state is used to tokenize the majority of a regular expression -
    /// most literals, boundaries, repeaters, etc. are parsed here, being the
    /// default parser for the input. Only groups, certain escapes, certain
    /// repetitions, and character classes are parsed in a different state;
    /// even then, groups fall back on the main state once their headers have
    /// been parsed.
    fn next_token_main(&mut self) -> Result<Token> {
        // If the ignore_whitespace flag is set, try to skip any whitespace
        // before reading the next token in the main state
        let flags = self.flags()?;
        if flags.ignore_space {
            self.skip_whitespace();
        }

        match self.input.next() {
            Some('^') => Ok(self.input.token(TokenKind::StartOfLine)),
            Some('$') => Ok(self.input.token(TokenKind::EndOfLine)),
            Some('.') => Ok(self.input.token(TokenKind::Dot)),
            Some('?') => Ok(self.input.token(TokenKind::Question)),
            Some('*') => Ok(self.input.token(TokenKind::Star)),
            Some('+') => Ok(self.input.token(TokenKind::Plus)),
            Some('|') => Ok(self.input.token(TokenKind::Alternate)),

            // The `(` token enters the group parsing state, e.g. `(?:abc)`
            // This state is swapped with another `Main` state after the header
            // is matched - a group name, flags, non-capturing token, etc.
            Some('(') => {
                self.state.push(State::Group);
                Ok(self.input.token(TokenKind::OpenGroup))
            },

            // The group state swaps to the main state as soon as its header
            // has been parsed; so this state is responsible for matching the
            // `)` token and popping the state
            Some(')') => match self.state.pop() {
                Ok(_) => Ok(self.input.token(TokenKind::CloseGroup)),
                Err(StateError::PoppedFinalState) => Ok(self.input.token(TokenKind::CloseGroup)),
                Err(err @ _) => Err(self.input.error(err.into())),
            },

            // The `[` token pushes the character class state, e.g. `[a-z]`
            Some('[') => {
                self.state.push(State::Class);
                Ok(self.input.token(TokenKind::OpenBracket))
            },

            // The main state is responsible for popping the class state
            Some(']') => Ok(self.input.token(TokenKind::CloseBracket)),

            // The `{` token pushes the range state, e.g. `{1,3}`
            Some('{') => {
                self.state.push(State::Range);
                Ok(self.input.token(TokenKind::OpenBrace))
            },

            // The main state is responsible for popping the range state
            Some('}') => Ok(self.input.token(TokenKind::CloseBrace)),

            // escape sequences can be a single character after a backslash,
            // or a more complicated unicode escapse e.g. `\x{01A1}`
            Some('\\') => self.parse_escape_sequence_main(),

            // All other input characters are literals
            Some(c) => Ok(self.input.token(TokenKind::Literal(c))),
            None => Err(self.input.error(ErrorKind::EndOfFile)),
        }
    }

    /// Get a token in the `Group` state
    ///
    /// This state matches the header of a group, represented by a single
    /// token. This could be a non-capturing marker, some flags, a group name,
    /// etc.
    ///
    /// Before this function returns, the `Group` state will be swapped with
    /// another `Main` state, in case there is an expression inside the group
    /// that needs to be matched
    fn next_token_group(&mut self) -> Result<Token> {
        match self.input.peek() {
            // The group header always begins with a `?`
            Some('?') => {
                // Consume the `?` to set up `peek` to read the next character
                self.input.next();
                match self.input.peek() {
                    // `?:` marks a non-capturing group
                    Some(':') => {
                        self.input.next();
                        Ok(self.input.token(TokenKind::NonCapturing))
                    },
                    // `?P` is the beginning of a named group, e.g. `(?P<name>)`
                    Some('P') => {
                        self.input.next();
                        self.parse_named_group()
                    },
                    // anything else is setting flags, either for the scope
                    // above or for whatever follows in the group
                    Some(_) => self.parse_flags(),
                    None => {
                        self.state
                            .pop_all()
                            .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;

                        Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                            "expected `:`, `P`, or group flags",
                        ))))
                    },
                }
            },
            // Once the group header has been turned into tokens, swap with the
            // main state to tokenize the remainder of the group (even if the
            // remainder is just a `)` token to close the group and pop the
            // state)
            _ => {
                self.state
                    .swap(State::Main)
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                self.next_token_main()
            },
        }
    }

    /// Get a token in the `Class` state
    ///
    /// This state matches elaborated character classes between `[` and `]`
    /// brackets, which can include ranges, differences, unions, etc.
    fn next_token_class(&mut self) -> Result<Token> {
        // If the ignore_whitespace flag is set, skip any whitespace and
        // comments before matching the next class token
        let flags = self.flags()?;
        if flags.ignore_space {
            self.skip_whitespace();
        }

        match self.input.next() {
            // `-` at the end of the class (e.g. `-]`) is just a literal
            Some('-') if matches!(self.input.peek(), Some(']')) => {
                Ok(self.input.token(TokenKind::Literal('-')))
            },
            // `--` is the difference token
            Some('-') if matches!(self.input.peek(), Some('-')) => {
                self.input.next();
                Ok(self.input.token(TokenKind::Difference))
            },
            // `-` following a literal is a range token (e.g. `a-`)
            Some('-') if matches!(self.last_token_kind, Some(TokenKind::Literal(_))) => {
                Ok(self.input.token(TokenKind::Range))
            },
            // In any other case, `-` is just a literal
            Some('-') => Ok(self.input.token(TokenKind::Literal('-'))),
            // `[:` begins a named class, e.g. `[:alpha:]`
            Some('[') if matches!(self.input.peek(), Some(':')) => {
                self.state.push(State::ClassName);
                Ok(self.input.token(TokenKind::OpenBracket))
            },
            // If not part of `[:`, `[` just begins a new sub-class
            Some('[') => {
                self.state.push(State::Class);
                Ok(self.input.token(TokenKind::OpenBracket))
            },
            // `]` ends a class name or a character class and pops its own state
            Some(']') => {
                self.state
                    .pop()
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                Ok(self.input.token(TokenKind::CloseBracket))
            },
            // `^` at the beginning of a character class is a negation
            Some('^') if matches!(self.last_token_kind, Some(TokenKind::OpenBracket)) => {
                Ok(self.input.token(TokenKind::Negated))
            },
            // `&&` is an intersection token
            Some('&') if matches!(self.input.peek(), Some('&')) => {
                self.input.next();
                Ok(self.input.token(TokenKind::Intersection))
            },
            // `~~` is a symmetrical difference token
            Some('~') if matches!(self.input.peek(), Some('~')) => {
                self.input.next();
                Ok(self.input.token(TokenKind::Symmetrical))
            },
            // Special characters can be escaped in classes
            Some('\\') => self.parse_escape_sequence_class(),
            // All other characters are just literals
            Some(c) => Ok(self.input.token(TokenKind::Literal(c))),
            None => {
                self.state
                    .pop_all()
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;

                Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                    "expected end of character class `]`",
                ))))
            },
        }
    }

    /// Read a named class within a character class
    ///
    /// A named class can be used in lieu of a more detailed character class
    /// for clarity, e.g. `[[:alpha:]]` instead of `[a-zA-Z]`. These classes
    /// are different from unicode classes (e.g. `\p{alpha}`) and are only
    /// stand-ins for relatively simple character classes:
    ///
    /// * `[[:alnum:]]` - `[0-9a-zA-Z]`
    /// * `[[:alpha:]]` - `[a-zA-Z]`
    /// * `[[:ascii:]]` - `[\x00-\x7F]`
    /// * `[[:blank:]]` - `[\t ]`
    /// * `[[:cntrl:]]` - `[\x00-\x1F\x7F]`
    /// * `[[:digit:]]` - `[0-9]`
    /// * `[[:graph:]]` - `[!-^]`
    /// * `[[:lower:]]` - `[a-z]`
    /// * `[[:print:]]` - `[ -~]`
    /// * `[[:punct:]]` - `[!-/:-@\[-\`{-~]`
    /// * `[[:space:]]` - `[\t\n\v\f\r ]`
    /// * `[[:upper:]]` - `[A-Z]`
    /// * `[[:word:]]` - `[0-9a-zA-Z_]`
    /// * `[[:xdigit:]]` - `[0-9a-fA-F]`
    fn next_token_class_name(&mut self) -> Result<Token> {
        // The initial `:` was already checked for by the `Class` state
        self.input.expect(':')?;

        // A `^` immediately following the opening `:` is a negation character
        let negated = self.input.matches('^');

        let mut name = String::new();
        loop {
            match self.input.next() {
                // The final `:` character is included in the token
                Some(':') => break,
                // Any alphabetic characters are included in the class name
                Some(c) if c.is_ascii_alphabetic() => name.push(c),
                // Any other characters are rejected
                Some(c) => return Err(self.input.error(ErrorKind::UnexpectedChar(c))),
                None => {
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of named character class `:]`",
                    ))))
                },
            }
        }

        self.state
            .swap(State::Class)
            .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
        Ok(self.input.token(TokenKind::ClassName(name, negated)))
    }

    /// Get a token in the `Range` state
    ///
    /// A range is a special kind of repetition operator specifying a minimum
    /// and maximum number of characters to match - e.g. `.\{3,5}`
    fn next_token_range(&mut self) -> Result<Token> {
        // Whitespace is always skipped in a range, because it cannot be
        // significant to what is matched. Anything between braces, numbers,
        // and commas is insignificant, but within numbers, whitespace is
        // not tolerated
        self.skip_whitespace();

        match self.input.peek() {
            // The end of the range pops its own state
            Some('}') => {
                self.input.next();
                self.state
                    .pop()
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                Ok(self.input.token(TokenKind::CloseBrace))
            },
            // A comma separates the start and end for the range
            Some(',') => {
                self.input.next();
                Ok(self.input.token(TokenKind::Comma))
            },
            // Anything else is interpreted as a number; this may fail
            Some(_) => self.parse_number(),
            None => {
                self.state
                    .pop_all()
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                    "expected end of rance `}` or `,`",
                ))))
            },
        }
    }

    /// Get a token in the `UnicodeProperties` state
    ///
    /// UnicodeProperties is a special character class that matches characters
    /// based on their unicode class, language, etc.
    ///
    /// Properties can be specified with one character shorthand, e.g.:
    ///
    /// * \pN - matches unicode numbers
    /// * \PN - matches anything except unicode numbers
    ///
    /// However, this state is only used once a brace is introduced to the
    /// pattern. Braces can be used for more control, e.g.:
    ///
    /// * \p{Lu} - matches an uppercase letter
    /// * \p{sc=Greek} - matches a letter in the Greek character set
    fn next_token_unicode_properties(&mut self) -> Result<Token> {
        // Whitespace is always skipped here, because whitespace cannot
        // be relevant to what is matched
        self.skip_whitespace();
        let mut item = String::new();

        loop {
            match self.input.peek() {
                // `=` is also matched via peek when some characters have been
                // collected in `item` - so if item is empty, then just match
                // the equal sign since the property name has been returned
                Some('=') if item.len() == 0 => {
                    self.input.next();
                    return Ok(self.input.token(TokenKind::UnicodeEqual(false)));
                },
                // `=` when some characters have already been matched denotes
                // the end of a property name; so return the property name and
                // leave the `=` character to be matched by the next invocation
                // of this function
                Some('=') => return Ok(self.input.token(TokenKind::UnicodePropName(item))),
                // `!` can be the beginning of a negated `=` - but only if it's
                // followed by an `=`
                Some('!') if item.len() == 0 => {
                    self.input.next();
                    return match self.input.peek() {
                        // It's a `!=` token
                        Some('=') => {
                            self.input.next();
                            Ok(self.input.token(TokenKind::UnicodeEqual(true)))
                        },
                        // It's just a random `!` - unexpected
                        Some(c) => Err(self.input.error(ErrorKind::UnexpectedChar(c))),
                        None => Err(self.input.error(ErrorKind::EndOfFile)),
                    };
                },
                // `!` ends the current property name, like `=`
                Some('!') => return Ok(self.input.token(TokenKind::UnicodePropName(item))),
                // `}` ends `UnicodeProperties` state and pops its own state
                Some('}') if item.len() == 0 => {
                    self.input.next();
                    self.state
                        .pop()
                        .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                    return Ok(self.input.token(TokenKind::UnicodeLongEnd));
                },
                // `}` when there is text stored in `item` denotes the end of
                // a unicode property value; return the value token and leave
                // the closing brace for the next invocation of this function
                Some('}') => return Ok(self.input.token(TokenKind::UnicodePropValue(item))),
                // Alphanumeric characters get added to `item` to be returned
                // as either property name or value tokens
                Some(c) if c.is_ascii_alphanumeric() => {
                    self.input.next();
                    item.push(c)
                },
                // Anything else is an unexpected character
                Some(c) => return Err(self.input.error(ErrorKind::UnexpectedChar(c))),
                None => {
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of unicode class `}`, `,` `!=` or `=`",
                    ))))
                },
            }
        }
    }

    /// Get an immutable copy of the state flags
    fn flags(&mut self) -> Result<&Flags> {
        match self.state.flags() {
            Ok(flags) => Ok(flags),
            Err(err) => Err(self.input.error(ErrorKind::InternalStateError(err))),
        }
    }

    /// Get a mutable copy of the state flags
    fn flags_mut(&mut self) -> Result<&mut Flags> {
        match self.state.flags_mut() {
            Ok(flags) => Ok(flags),
            Err(err) => Err(self.input.error(ErrorKind::InternalStateError(err))),
        }
    }

    /// Skip whitespace and comments in the input
    ///
    /// Any whitespace, or anything following a `#` character up to a newline
    /// will be skipped. This is mostly used when the `ignore_whitespace` flag
    /// is set for the main and class states; but it's also used in states where
    /// whitespace is not significant in any case (e.g. in ranges).
    ///
    /// # Examples
    ///
    /// The input:
    ///
    /// `a b c\nd e f #this is a comment\ng h i`
    ///
    /// will be replaced with
    ///
    /// `abcdefghi`
    fn skip_whitespace(&mut self) {
        // Keep track of whether a comment is being skipped; this will cause
        // every character except a newling to be skipped, instead of only
        // whitespace and `#` characters.
        let mut skipping_comment = false;
        loop {
            match self.input.peek() {
                // Terminate comment skipping once a newline is seen
                Some('\n') if skipping_comment => skipping_comment = false,
                // Skip all whitespace, or any character if in a comment
                Some(c) if c.is_whitespace() || skipping_comment => (),
                // Enter a comment when `#` is encountered
                Some('#') => skipping_comment = true,
                // In any other case, all whitespace has been skipped
                _ => break,
            }

            self.input.next();
        }
    }

    /// Match an escape sequence in the `Main` state
    ///
    /// This matches some anchors, literals, boundaries, and character classes
    /// which are represented as simple one-letter escape sequences. Unicode
    /// classes are also parsed here, as well as hexadecimal numerical escapes.
    fn parse_escape_sequence_main(&mut self) -> Result<Token> {
        match self.input.next() {
            Some('A') => Ok(self.input.token(TokenKind::StartOfText)),
            Some('z') => Ok(self.input.token(TokenKind::EndOfText)),
            Some('b') => Ok(self.input.token(TokenKind::WordBoundary)),
            Some('B') => Ok(self.input.token(TokenKind::NonWordBoundary)),
            Some('a') => Ok(self.input.token(TokenKind::Literal('\x07'))),
            Some('f') => Ok(self.input.token(TokenKind::Literal('\x0C'))),
            Some('t') => Ok(self.input.token(TokenKind::Literal('\t'))),
            Some('n') => Ok(self.input.token(TokenKind::Literal('\n'))),
            Some('r') => Ok(self.input.token(TokenKind::Literal('\r'))),
            Some('v') => Ok(self.input.token(TokenKind::Literal('\x0B'))),
            Some('d') => Ok(self.input.token(TokenKind::Digit(false))),
            Some('D') => Ok(self.input.token(TokenKind::Digit(true))),
            Some('s') => Ok(self.input.token(TokenKind::Whitespace(false))),
            Some('S') => Ok(self.input.token(TokenKind::Whitespace(true))),
            Some('w') => Ok(self.input.token(TokenKind::WordChar(false))),
            Some('W') => Ok(self.input.token(TokenKind::WordChar(true))),
            // Parse a unicode class, whether a simple one-character escape
            // sequence or a braced expression
            Some('p') => self.parse_unicode(false),
            // Parse a negated unicode class, whether a simple one-character
            // escape sequence or a braced expression
            Some('P') => self.parse_unicode(true),
            // Parse a hex escape, which is a numerical representation of a
            // single literal character (e.g. \x7F)
            Some(c) if Tokenizer::is_hex_escape(&c) => self.parse_hex(c),
            // Certain special characters can be escaped to a literal (e.g., \})
            Some(c) if Tokenizer::escapes_to_literal_main(&c) => {
                Ok(self.input.token(TokenKind::Literal(c)))
            },
            // Everything else is an invalid escape sequence
            Some(c) => Err(self.input.error(ErrorKind::UnrecognizedEscape(c))),
            // In lieu of an end of file error, we know that an errant \ exists
            // in the input, so use a more specific error
            None => Err(self.input.error(ErrorKind::UnexpectedChar('\\'))),
        }
    }

    /// Parse a hex escape sequence to create a literal
    ///
    /// Hex sequences without braces can match 2, 4, or 8 hexadecimal digits
    /// as a literal:
    ///
    /// * \x7F
    /// * \u007F
    /// * \U0000007F
    ///
    /// Any of these sequences can be surrounded by braces to match any number
    /// of digits. Any number that doesn't represent a valid codepoint will be
    /// rejected.
    ///
    /// * \x{007F}
    /// * \u{0000007F}
    /// * \U{7F}
    fn parse_hex(&mut self, ident: char) -> Result<Token> {
        let mut number = String::new();
        let bounded = self.input.matches('{');

        let digits = match (bounded, ident) {
            (true, _) => 8,
            (_, 'x') => 2,
            (_, 'u') => 4,
            (_, 'U') => 8,
            _ => panic!("accepted unknown hex escape specifier `{}`", ident),
        };

        for _ in 0..digits {
            match self.input.next() {
                Some(c) if c.is_ascii_hexdigit() => number.push(c),
                Some('}') if bounded => break,
                Some(c) => return Err(self.input.error(ErrorKind::InvalidHexDigit(c))),
                None => {
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of hex literal",
                    ))))
                },
            };
        }

        let value = u32::from_str_radix(&number, 16)
            .expect(&format!("accepted invalid hex string: {}", number));
        let c = char::from_u32(value)
            .ok_or_else(|| self.input.error(ErrorKind::InvalidCharCode(number)))?;
        Ok(self.input.token(TokenKind::Literal(c)))
    }

    /// Parse a unicode character class, either as a simple one-character
    /// escape sequence or as a braced expression
    ///
    /// As a one-character escape, the class represents a single-character
    /// general category (e.g., N for Numeric). The braced expression can
    /// specify the same general category, as well as other properties:
    ///
    /// * `\p{Alphabetic}` or `\p{alpha}`: alphabetic class
    /// * `\p{Letter}` or `\pL`: letter class
    /// * `\p{script!=Greek}` or `\p{sc!=Greek}` or `\P{sc=Greek}`:
    ///     negated match of the Greek script
    fn parse_unicode(&mut self, negated: bool) -> Result<Token> {
        // Check for a braced expression and enter the `UnicodeProperties` state
        if self.input.matches('{') {
            self.state.push(State::UnicodeProperties);
            return Ok(self.input.token(TokenKind::UnicodeLongStart(negated)));
        }

        // Otherwise, it's a single-character class
        match self.input.next() {
            Some(c) if c.is_ascii_alphabetic() => {
                Ok(self.input.token(TokenKind::UnicodeShort(c, negated)))
            },
            Some(c) => Err(self.input.error(ErrorKind::UnexpectedChar(c))),
            None => Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                "expected single-character unicode general category",
            )))),
        }
    }

    /// Read the name of a named group, e.g. `(?P<name>)`
    ///
    /// This is called from the `Group` state and is used to simplify the
    /// match expression in that `next` function. No states are modified here,
    /// instead, the name and surrounding `<>` characters are consumed and
    /// returned as a token
    fn parse_named_group(&mut self) -> Result<Token> {
        // The initial `<` character was checked for in `next_token_group`
        self.input.expect('<')?;

        let mut name = String::new();
        loop {
            match self.input.next() {
                // The closing `>` character ends the name
                Some('>') => break,
                // Any character between `<` and `>` goes into the name
                Some(c) => name.push(c),
                None => {
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of group name `>`",
                    ))))
                },
            }
        }

        // The `Group` state ends when a group name is matched
        self.state
            .pop()
            .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
        Ok(self.input.token(TokenKind::Name(name)))
    }

    /// Parse group flags, whether they apply to the above scope or withing
    /// a non-capturing group.
    ///
    /// Group flags are as follows:
    ///
    /// * `i` - case insensitive
    /// * `m` - multiline mode (^ and * match begin/end of line)
    /// * `s` - `.` matches `\n`
    /// * `U` - swap the meaning of `x*` and `x*?`
    /// * `u` - unicode support (enabled by default)
    /// * `x` - ignore whitespace and allow line comments (starting with `#`)
    ///
    /// Flags can be set for a non-capturing group such that they don't escape
    /// that group:
    ///
    /// `(?i:NoT cAsE sEnSiTiVe) case sensitive`
    ///
    /// Or can set the flags of the enclosing scope going forwards:
    ///
    /// `case sensitive (?i) NoT cAsE sEnSiTiVe`
    ///
    /// Flags can be set by appearing in the front of the group, or cleared
    /// by appearing in the end of a group after a `-` character:
    ///
    /// `{?i:NoT cAsE sEnSiTiVe (?-i:case sensitive))`
    ///
    /// Both set and cleared flags are stored in the token to be used in the
    /// interpreter, but the ignore whitespace flag `x` changes the function of
    /// the tokenizer itself, as it will stop returning whitespace and comments
    /// as literal tokens and skip them instead. For this reason, the
    /// tokenizer's state stack keeps track of the ignore_whitespace flag.
    fn parse_flags(&mut self) -> Result<Token> {
        // Flags set by the group
        let mut set_flags = Vec::new();
        // Flags cleared by the group
        let mut clear_flags = Vec::new();
        // Whether the tokenizer is in a clearing state or not; all flags from
        // here are considered cleared
        let mut clearing = false;
        // Whether the group is non-capturing, i.e. the flag state is contained
        // within the group or escapes to the enclosing scope
        let mut non_capturing = false;
        // Whether to update the ignore_whitespace flag in either the current
        // group or the enclosing scope, depending on whether this is a
        // non-capturing group or not
        let mut set_ignore_whitespace = None;

        loop {
            match self.input.peek() {
                // Only valid flag specifiers are accepted here
                Some(c) if Tokenizer::is_group_flag(&c) => {
                    self.input.next();
                    // If the ignore_whitespace flag is specified, make sure to
                    // set or clear the ignore_whitespace flag in the
                    // tokenizer's state
                    if c == 'x' {
                        set_ignore_whitespace = Some(!clearing);
                    }

                    if clearing {
                        // Clear the flag if we're in the clearing portion of
                        // the flag section
                        clear_flags.push(c)
                    } else {
                        // Set the flag if we're not in the clearing portion
                        // of the flag section
                        set_flags.push(c)
                    }
                },
                // The `:` character terminates the flag section and specifies a
                // non-capturing group
                Some(':') => {
                    self.input.next();
                    non_capturing = true;
                    break;
                },
                // The `-` character separates the set and clear flags
                Some('-') => {
                    self.input.next();
                    clearing = true;
                },
                // The `)` character terminates the flag section and specifies a
                // flag group that affects the enclosing scope (i.e. not a non-
                // capturing group)
                Some(')') => break,
                // Any other character is invalid here
                Some(c) => {
                    self.input.next();
                    return Err(self.input.error(ErrorKind::UnrecognizedFlag(c)));
                },
                None => {
                    self.state
                        .pop_all()
                        .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of group `)` or end of flags `:`",
                    ))));
                },
            }
        }

        let result = if non_capturing {
            // If this is just a non-capturing group with flags, then the
            // ignore_whitespace flag shouldn't be set for the preceeding state
            // and only the current state needs it:
            //
            // e.g. turning the ignore_whitespace flag on:
            //
            // * [Main(false), Group(false)] - start of parse_flags
            // * [Main(false), Group(true)]  - set the flag here
            // * [Main(false), Main(true)]   - Group is swapped with Main once
            //                                 the group header is parsed
            // * [Main(false)]               - original flag used once the `)`
            //                                 token pops a state from the stack
            if let Some(val) = set_ignore_whitespace {
                let mut flags = self.flags_mut()?;
                flags.ignore_space = val
            }

            self.input
                .token(TokenKind::NonCapturingFlags(set_flags, clear_flags))
        } else {
            // If we're setting the whitespace flag in a flag token, then the
            // state below this one needs to get the flag. The next_main
            // function will pop the group state, so replace it here after
            // setting the flag:
            //
            // e.g. turning the ignore_whitespace flag on:
            //
            // * [Main(false), Group(false)] - start of parse_flags
            // * [Main(false)]               - pop Group state
            // * [Main(true)]                - set the flag
            // * [Main(true), Group(true)]   - re-push the original state,
            //                                 the flags will be copied
            // * [Main(true), Main(true)]    - Group is swapped with Main once
            //                                 the group header is parsed
            // * [Main(true)]                - next_main will pop the state
            if let Some(val) = set_ignore_whitespace {
                let this_state = self
                    .state
                    .pop()
                    .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;

                let mut flags = self.flags_mut()?;
                flags.ignore_space = val;
                self.state.push(this_state);
            }

            self.input.token(TokenKind::Flags(set_flags, clear_flags))
        };

        Ok(result)
    }

    /// Parse an escape sequence in the `Class` state
    ///
    /// Escape sequences in the class state differ from the main state in that
    /// the anchors and boundaries don't count, and the literal escapes are
    /// different.
    ///
    /// The class escapes still match (e.g. \s, \w), and a subset of the same
    /// literals (e.g. \], \^) as well as some escapes not valid in the `Main`
    /// state (e.g. \&, \-) can be escaped here.
    fn parse_escape_sequence_class(&mut self) -> Result<Token> {
        match self.input.next() {
            Some('s') => Ok(self.input.token(TokenKind::Whitespace(false))),
            Some('S') => Ok(self.input.token(TokenKind::Whitespace(true))),
            Some('d') => Ok(self.input.token(TokenKind::Digit(false))),
            Some('D') => Ok(self.input.token(TokenKind::Digit(true))),
            Some('w') => Ok(self.input.token(TokenKind::WordChar(false))),
            Some('W') => Ok(self.input.token(TokenKind::WordChar(true))),
            // Parse a hex escape, which is a numerical representation of a
            // single literal character (e.g. \x7F)
            Some(c) if Tokenizer::is_hex_escape(&c) => self.parse_hex(c),
            Some(c) if Tokenizer::escapes_to_literal_class(&c) => {
                Ok(self.input.token(TokenKind::Literal(c)))
            },
            Some(c) => Err(self.input.error(ErrorKind::UnrecognizedEscape(c))),
            None => Err(self.input.error(ErrorKind::UnexpectedChar('\\'))),
        }
    }

    /// Parse a number in the `Range` state
    ///
    /// This is specifically for tokenizing numbers withing a character range,
    /// e.g. `.{3,5}`
    fn parse_number(&mut self) -> Result<Token> {
        let mut number = String::new();
        loop {
            match self.input.peek() {
                // A comma indicates that the next number in the range is coming
                Some(',') => break,
                // The closing brace signifies the end of the range
                Some('}') => break,
                // Break on whitespace because `next_token_range` is skipping it
                Some(c) if c.is_whitespace() => break,
                // Any numeric character is added to the current number
                Some(c) if c.is_numeric() => {
                    self.input.next();
                    number.push(c);
                },
                // Anything else is an unexpected character
                Some(c) => {
                    self.input.next();
                    return Err(self.input.error(ErrorKind::UnexpectedChar(c)));
                },
                None => {
                    self.state
                        .pop_all()
                        .map_err(|err| self.input.error(ErrorKind::InternalStateError(err)))?;
                    return Err(self.input.error(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of range `}`",
                    ))));
                },
            }
        }

        // This function is only for parsing the number, so a number should
        // always be the output. Parse the input as an unsigned integer
        let value = number
            .parse::<u32>()
            .expect(&format!("accepted invalid number: {}", number));

        Ok(self.input.token(TokenKind::Number(value)))
    }

    /// Check whether a character can be escaped in the `Main` state
    fn escapes_to_literal_main(c: &char) -> bool {
        match c {
            '^' | '$' | '.' | '?' | '*' | '+' | '|' | '(' | ')' | '[' | ']' | '{' | '}' | '\\' => {
                true
            },
            _ => false,
        }
    }

    /// Check whether a character can be escaped in the `Class` state
    fn escapes_to_literal_class(c: &char) -> bool {
        match c {
            '^' | '&' | '~' | '-' | '[' | ']' | ':' | '\\' => true,
            _ => false,
        }
    }

    /// Check whether a character is a valid group flag (case insensitive,
    /// ignore whitespace, enable unicode, etc.)
    fn is_group_flag(c: &char) -> bool {
        match c {
            'i' | 'm' | 's' | 'U' | 'u' | 'x' => true,
            _ => false,
        }
    }

    /// Check whether a character is the beginning of a hex escape sequence
    /// (e.g. in `\x7F`, `\u007F`, `\x{12AB}`, etc.)
    fn is_hex_escape(c: &char) -> bool {
        match c {
            'x' | 'u' | 'U' => true,
            _ => false,
        }
    }
}

impl<'a> iter::Iterator for Tokenizer<'a> {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Err(Error {
                kind: ErrorKind::EndOfFile,
                ..
            }) => None,
            result @ _ => Some(result),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::assert_tokens;

    #[test]
    fn simple_main() {
        // Check the basic tokens in the main state
        let tr = Tokenizer::new(r"a^$.?*+|()]}");
        assert_tokens(
            tr,
            vec![
                TokenKind::Literal('a'),
                TokenKind::StartOfLine,
                TokenKind::EndOfLine,
                TokenKind::Dot,
                TokenKind::Question,
                TokenKind::Star,
                TokenKind::Plus,
                TokenKind::Alternate,
                TokenKind::OpenGroup,
                TokenKind::CloseGroup,
                TokenKind::CloseBracket,
                TokenKind::CloseBrace,
            ],
        );
    }

    #[test]
    fn extra_group_end() {
        let tr = Tokenizer::new(r"a))");
        let expected = vec![
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::CloseGroup,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn expected_group_end() {
        let mut tr = Tokenizer::new(r"(");
        assert_next_tok!(tr, TokenKind::OpenGroup);
    }

    #[test]
    fn simple_escapes() {
        let tr = Tokenizer::new(r"\A\z\b\B\a\f\t\n\r\v\d\D\s\S\w\W");
        let expected = vec![
            TokenKind::StartOfText,
            TokenKind::EndOfText,
            TokenKind::WordBoundary,
            TokenKind::NonWordBoundary,
            TokenKind::Literal('\x07'),
            TokenKind::Literal('\x0C'),
            TokenKind::Literal('\t'),
            TokenKind::Literal('\n'),
            TokenKind::Literal('\r'),
            TokenKind::Literal('\x0B'),
            TokenKind::Digit(false),
            TokenKind::Digit(true),
            TokenKind::Whitespace(false),
            TokenKind::Whitespace(true),
            TokenKind::WordChar(false),
            TokenKind::WordChar(true),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn literal_escapes() {
        // Check all the literal escapes in the main state
        let tr = Tokenizer::new(r"\^\$\.\?\*\+\|\(\)\[\]\{\}\\");
        let expected = vec![
            TokenKind::Literal('^'),
            TokenKind::Literal('$'),
            TokenKind::Literal('.'),
            TokenKind::Literal('?'),
            TokenKind::Literal('*'),
            TokenKind::Literal('+'),
            TokenKind::Literal('|'),
            TokenKind::Literal('('),
            TokenKind::Literal(')'),
            TokenKind::Literal('['),
            TokenKind::Literal(']'),
            TokenKind::Literal('{'),
            TokenKind::Literal('}'),
            TokenKind::Literal('\\'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn main_escape_errors() {
        let mut tr = Tokenizer::new(r"\:");
        assert_next_err!(tr, ErrorKind::UnrecognizedEscape(':'));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"\");
        assert_next_err!(tr, ErrorKind::UnexpectedChar('\\'));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"\p");
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"\u00");
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"\u00t");
        assert_next_err!(tr, ErrorKind::InvalidHexDigit('t'));
        assert_next_none!(tr);

        // 0xD83F is part of a surrogate pair, and is not a valid codepoint
        let mut tr = Tokenizer::new(r"\uD83F");
        assert_next_err!(tr, ErrorKind::InvalidCharCode(_));
        assert_next_none!(tr);
    }

    #[test]
    fn class_escape_errors() {
        let mut tr = Tokenizer::new(r"[\*");
        assert_next_tok!(tr, TokenKind::OpenBracket);
        assert_next_err!(tr, ErrorKind::UnrecognizedEscape('*'));
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"[\");
        assert_next_tok!(tr, TokenKind::OpenBracket);
        assert_next_err!(tr, ErrorKind::UnexpectedChar('\\'));
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);
    }

    #[test]
    fn basic_group() {
        // Check basic group functionality
        let tr = Tokenizer::new(r"(a)b");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::Literal('b'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn non_capturing_group() {
        // Test a non-capturing group
        let tr = Tokenizer::new(r"(?:ab)");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::NonCapturing,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::CloseGroup,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn named_group() {
        // Test a named group
        let tr = Tokenizer::new(r"(?P<name>");
        let expected = vec![TokenKind::OpenGroup, TokenKind::Name(String::from("name"))];

        assert_tokens(tr, expected);
    }

    #[test]
    fn group_errors() {
        let mut tr = Tokenizer::new(r"(?");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"(?P<name");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"(?x");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"(?q");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_err!(tr, ErrorKind::UnrecognizedFlag('q'));
        assert_next_none!(tr);
    }

    #[test]
    fn flag_group() {
        // Test a flags group
        let tr = Tokenizer::new(r"(?isUx)");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['i', 's', 'U', 'x'], vec![]),
            TokenKind::CloseGroup,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn non_capturing_flag_group() {
        // Test a non-capturing flags group
        let tr = Tokenizer::new(r"(?mx:a)b");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::NonCapturingFlags(vec!['m', 'x'], vec![]),
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::Literal('b'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn flag_parse() {
        // Test the flags tokens in a variety of configurations
        let tr = Tokenizer::new(r"(?is-Ux)(?-iU:)(?sx-)");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['i', 's'], vec!['U', 'x']),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::NonCapturingFlags(vec![], vec!['i', 'U']),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['s', 'x'], vec![]),
            TokenKind::CloseGroup,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn linear_flag_settings() {
        // Test that flag ignore_whitespace settings escape the group
        let tr = Tokenizer::new(" a\nb(?x) a\nb(?-x) a\nb");
        let expected = vec![
            TokenKind::Literal(' '),
            TokenKind::Literal('a'),
            TokenKind::Literal('\n'),
            TokenKind::Literal('b'),
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['x'], vec![]),
            TokenKind::CloseGroup,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::OpenGroup,
            TokenKind::Flags(vec![], vec!['x']),
            TokenKind::CloseGroup,
            TokenKind::Literal(' '),
            TokenKind::Literal('a'),
            TokenKind::Literal('\n'),
            TokenKind::Literal('b'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn nested_flag_settings() {
        // Test that ignore_whitespace can be limited to group scope
        let tr = Tokenizer::new("(?x: \nz(?-x: \ny) \nx) \nw");

        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::NonCapturingFlags(vec!['x'], vec![]),
            TokenKind::Literal('z'),
            TokenKind::OpenGroup,
            TokenKind::NonCapturingFlags(vec![], vec!['x']),
            TokenKind::Literal(' '),
            TokenKind::Literal('\n'),
            TokenKind::Literal('y'),
            TokenKind::CloseGroup,
            TokenKind::Literal('x'),
            TokenKind::CloseGroup,
            TokenKind::Literal(' '),
            TokenKind::Literal('\n'),
            TokenKind::Literal('w'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn comment_skip() {
        // Test that ignore_whitespace will skip comments
        let tr = Tokenizer::new("(?x)#skip this comment\na$");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['x'], vec![]),
            TokenKind::CloseGroup,
            TokenKind::Literal('a'),
            TokenKind::EndOfLine,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn simple_class() {
        // Test basic class tokens
        let tr = Tokenizer::new(r"[^-^a-z-]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Negated,
            TokenKind::Literal('-'),
            TokenKind::Literal('^'),
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::Literal('-'),
            TokenKind::CloseBracket,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn posix_class() {
        let mut tr = Tokenizer::new(r"[:a:][[:a:]a-z]");
        tr.set_debug();

        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal(':'),
            TokenKind::Literal('a'),
            TokenKind::Literal(':'),
            TokenKind::CloseBracket,
            TokenKind::OpenBracket,
            TokenKind::OpenBracket,
            TokenKind::ClassName(String::from("a"), false),
            TokenKind::CloseBracket,
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::CloseBracket,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn class_names_and_differences() {
        // Test named classes and some difference operators
        let tr = Tokenizer::new(r"[x[aA0~~[:^lower:]--[:alnum:]]&&a-z]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal('x'),
            TokenKind::OpenBracket,
            TokenKind::Literal('a'),
            TokenKind::Literal('A'),
            TokenKind::Literal('0'),
            TokenKind::Symmetrical,
            TokenKind::OpenBracket,
            TokenKind::ClassName(String::from("lower"), true),
            TokenKind::CloseBracket,
            TokenKind::Difference,
            TokenKind::OpenBracket,
            TokenKind::ClassName(String::from("alnum"), false),
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn named_classes_and_bad_negations() {
        // Test that named class negation works correctly
        let tr = Tokenizer::new(r"[[^:abc:]][:abc:]]]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::OpenBracket,
            TokenKind::Negated,
            TokenKind::Literal(':'),
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::Literal(':'),
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
            TokenKind::OpenBracket,
            TokenKind::Literal(':'),
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::Literal(':'),
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn class_escapes() {
        // Check that class escapes work correctly
        let tr = Tokenizer::new(r"[\^\&&~\~\]\[[\:a:]\s\W\D\--]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal('^'),
            TokenKind::Literal('&'),
            TokenKind::Literal('&'),
            TokenKind::Literal('~'),
            TokenKind::Literal('~'),
            TokenKind::Literal(']'),
            TokenKind::Literal('['),
            TokenKind::OpenBracket,
            TokenKind::Literal(':'),
            TokenKind::Literal('a'),
            TokenKind::Literal(':'),
            TokenKind::CloseBracket,
            TokenKind::Whitespace(false),
            TokenKind::WordChar(true),
            TokenKind::Digit(true),
            TokenKind::Literal('-'),
            TokenKind::Literal('-'),
            TokenKind::CloseBracket,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn range() {
        // Check that basic ranges work correctly with whitespace skipping
        let tr = Tokenizer::new(r"{ 1 , 234 }{,}");
        let expected = vec![
            TokenKind::OpenBrace,
            TokenKind::Number(1),
            TokenKind::Comma,
            TokenKind::Number(234),
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Comma,
            TokenKind::CloseBrace,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn range_errors() {
        let mut tr = Tokenizer::new(r"{:");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_err!(tr, ErrorKind::UnexpectedChar(':'));
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"{1a");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_err!(tr, ErrorKind::UnexpectedChar('a'));
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"{");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);
    }

    #[test]
    fn basic_unicode() {
        // Check unbraced hex escapes
        let tr = Tokenizer::new(r"\x7F1\u4E2AE\U0000007F0");
        let expected = vec![
            TokenKind::Literal('\x7F'),
            TokenKind::Literal('1'),
            TokenKind::Literal('\u{4E2A}'),
            TokenKind::Literal('E'),
            TokenKind::Literal('\x7F'),
            TokenKind::Literal('0'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn braced_unicode_x() {
        // Check that braced hex escapes work with \x
        let tr = Tokenizer::new(r"\x{1}\x{12}\x{123}\x{1234}\x{12345}");
        let expected = vec![
            TokenKind::Literal('\x01'),
            TokenKind::Literal('\x12'),
            TokenKind::Literal('\u{0123}'),
            TokenKind::Literal('\u{1234}'),
            TokenKind::Literal('\u{012345}'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn braced_unicode_u() {
        // Check that braced hex escapes work with \u
        let tr = Tokenizer::new(r"\u{1}\u{12}\u{123}\u{1234}\u{12345}");
        let expected = vec![
            TokenKind::Literal('\x01'),
            TokenKind::Literal('\x12'),
            TokenKind::Literal('\u{0123}'),
            TokenKind::Literal('\u{1234}'),
            TokenKind::Literal('\u{012345}'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn braced_unicode_upper_u() {
        // Check that braced hex escapes work with \U
        let tr = Tokenizer::new(r"\U{1}\U{12}\U{123}\U{1234}\U{12345}");
        let expected = vec![
            TokenKind::Literal('\x01'),
            TokenKind::Literal('\x12'),
            TokenKind::Literal('\u{0123}'),
            TokenKind::Literal('\u{1234}'),
            TokenKind::Literal('\u{012345}'),
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn unicode_properties() {
        // Check that unicode property classes work
        let tr = Tokenizer::new(r"\pL\PN\p{Mn}\P{sc!=Greek}");
        let expected = vec![
            TokenKind::UnicodeShort('L', false),
            TokenKind::UnicodeShort('N', true),
            TokenKind::UnicodeLongStart(false),
            TokenKind::UnicodePropValue(String::from("Mn")),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeLongStart(true),
            TokenKind::UnicodePropName(String::from("sc")),
            TokenKind::UnicodeEqual(true),
            TokenKind::UnicodePropValue(String::from("Greek")),
            TokenKind::UnicodeLongEnd,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn iter_basic() {
        let tr = Tokenizer::new(r"a(b[c-d]{1,2})$");
        let kinds = tr
            .filter_map(|tok_result| tok_result.map(|tok| tok.kind).ok())
            .collect::<Vec<TokenKind>>();

        assert_eq!(
            kinds,
            vec![
                TokenKind::Literal('a'),
                TokenKind::OpenGroup,
                TokenKind::Literal('b'),
                TokenKind::OpenBracket,
                TokenKind::Literal('c'),
                TokenKind::Range,
                TokenKind::Literal('d'),
                TokenKind::CloseBracket,
                TokenKind::OpenBrace,
                TokenKind::Number(1),
                TokenKind::Comma,
                TokenKind::Number(2),
                TokenKind::CloseBrace,
                TokenKind::CloseGroup,
                TokenKind::EndOfLine,
            ]
        );
    }

    #[test]
    fn iter_error() {
        let mut tr = Tokenizer::new(r"{");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_err!(tr, ErrorKind::UnexpectedEOF(_));
        assert_next_none!(tr);
    }
}
