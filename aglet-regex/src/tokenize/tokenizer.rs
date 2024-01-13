use std::convert::TryInto;

use aglet_text::Cursor;

use crate::tokenize::error::*;
use crate::tokenize::state::*;
use crate::tokenize::token::*;
#[cfg(test)]
use crate::tokenize::{assert_next_err, assert_next_none, assert_next_tok};

/// Tokenizer for a regular expression
pub struct Tokenizer<'a> {
    cursor:          Cursor<'a>,
    state:           StateStack,
    last_token_kind: Option<TokenKind>,
    is_eof:          bool,
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
    pub fn new<C: Into<Cursor<'a>>>(cursor: C) -> Self {
        Tokenizer {
            cursor:          cursor.into(),
            state:           StateStack::new(),
            last_token_kind: None,
            is_eof:          false,
        }
    }

    /// Get the next token from the input. A token includes both the kind of the
    /// token along with the kind's associated data (e.g. a literal includes the
    /// literal value) and the span over which the token was found in the input.
    ///
    /// # Examples
    ///
    /// ```
    /// use aglet_regex::tokenize::*;
    /// use aglet_regex::tokenize::token::Flag;
    /// let mut tr = Tokenizer::new("(?x-i)");
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::OpenGroup, 0, 1)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::OpenGroupOptions, 1, 2)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::Flag(Flag::IgnoreWhitespace), 2, 3)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::FlagDelimiter, 3, 4)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::Flag(Flag::CaseInsensitive), 4, 5)),
    /// );
    /// assert_eq!(
    ///     tr.next_token(),
    ///     Ok(Token::new_with_offsets(TokenKind::CloseGroup, 5, 6)),
    /// );
    /// ```
    pub fn next_token(&mut self) -> Result<Token> {
        // short-circuit the token matching code if the tokenizer has already been marked EOF
        if self.is_eof {
            return self.err(ErrorKind::EndOfFile);
        }

        let res = self.next_token_inner();

        if let Err(err) = &res {
            match &err.cause {
                ErrorCause::FatalError(_) => self.is_eof = true,
                ErrorCause::Error(err) => match err {
                    ErrorKind::EndOfFile | ErrorKind::UnexpectedEOF(_) => self.is_eof = true,
                    _ => (),
                },
            };
        }

        res
    }

    /// Get the next token from the input, along with the current state stack.
    ///
    /// When a state is added to the stack during an iteration, then the stack including the new
    /// state will be returned. When a state is popped, then the stack will be returned as it was
    /// before the iteration (i.e. will include the popped state). This is to group the terminating
    /// token with the state it ends in the output.
    ///
    /// # Example
    ///
    /// For the regular expression:
    ///
    /// `[ab].+`
    ///
    /// The stacks returned with each token will be:
    ///
    /// * `[` [Main, Class]
    /// * 'a' [Main, Class]
    /// * 'b' [Main, Class]
    /// * `]` [Main, Class]
    /// * `.` [Main]
    /// * `+` [Main]
    ///
    /// If this function always returned the state stack as it was _after_ each iteration, then
    /// the result would be:
    ///
    /// * `[` [Main, Class]
    /// * 'a' [Main, Class]
    /// * 'b' [Main, Class]
    /// * `]` [Main]
    /// * `.` [Main]
    /// * `+` [Main]
    ///
    /// disconnecting the closing bracket from the class state. This only really impacts the
    /// colorization of pretty-printed token lists when, for example, a brace can be colorized as
    /// part of a repetition or a unicode class. The open brace is always part of one state or the
    /// other, but the close brace would never have that associated information.
    pub fn next_token_stack(&mut self) -> StackResult<TokenStack> {
        let (token, stack) = {
            let stack_before = self.state.clone();
            let token = self
                .next_token()
                .map_err(|e| StackError::from_error(e, stack_before.clone()))?;

            if stack_before.stack.len() > self.state.stack.len() {
                (token, stack_before)
            } else {
                (token, self.state.clone())
            }
        };

        Ok(TokenStack { token, stack })
    }

    /// Create an iterator over
    pub fn into_token_stack_iter(self) -> TokenStackIterator<'a> {
        TokenStackIterator { tokenizer: self }
    }

    /// Get the next token from the input
    fn next_token_inner(&mut self) -> Result<Token> {
        let token = match self.state.get() {
            Ok(State::Main) => self.next_token_main(),
            Ok(State::GroupOptions) => self.next_token_group_options(),
            Ok(State::Class) => self.next_token_class(),
            Ok(State::ClassName) => self.next_token_class_name(),
            Ok(State::Range) => self.next_token_range(),
            Ok(State::UnicodeProperties) => self.next_token_unicode_properties(),
            Err(err) => self.fatal_err(err.into()),
        };

        if let Ok(tok) = token.as_ref() {
            self.last_token_kind = Some(tok.kind.clone());
        }

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

        match self.cursor.next() {
            Some('^') => self.tok_kind(TokenKind::StartOfLine),
            Some('$') => self.tok_kind(TokenKind::EndOfLine),
            Some('.') => self.tok_kind(TokenKind::Dot),
            Some('?') => self.tok_kind(TokenKind::Question),
            Some('*') => self.tok_kind(TokenKind::Star),
            Some('+') => self.tok_kind(TokenKind::Plus),
            Some('|') => self.tok_kind(TokenKind::Alternate),

            // The `(` token enters a group state - if it's just a capturing group
            // (e.g. `(xyz)`) then just another `Main` state is used to parse the sub-expression.
            // The group might have a header that starts with `?` (e.g. `(?P<name>xyz)`), in which
            // case enter the `Group` state to tokenize it before optionally returning to the
            // `Main` state.
            Some('(') if matches!(self.cursor.first(), Some('?')) => {
                self.push_state(State::GroupOptions);
                self.tok_kind(TokenKind::OpenGroup)
            },
            Some('(') => {
                self.push_state(State::Main);
                self.tok_kind(TokenKind::OpenGroup)
            },

            // The group state swaps to the main state as soon as its header
            // has been parsed; so this state is responsible for matching the
            // `)` token and popping the state
            Some(')') => match self.state.pop() {
                Ok(_) => self.tok_kind(TokenKind::CloseGroup),
                Err(StateError::PoppedFinalState) => self.tok_kind(TokenKind::CloseGroup),
                Err(err) => self.fatal_err(err.into()),
            },

            // The `[` token pushes the character class state, e.g. `[a-z]`
            Some('[') => {
                self.push_state(State::Class);
                self.tok_kind(TokenKind::OpenBracket)
            },

            // The class state is responsible for matching these and popping the class state.
            // If one is found here, it'll be a parse error, but the token can still be matched
            Some(']') => self.tok_kind(TokenKind::CloseBracket),

            // The `{` token pushes the range state, e.g. `{1,3}`
            Some('{') => {
                self.push_state(State::Range);
                self.tok_kind(TokenKind::OpenBrace)
            },

            // The range state is responsible for matching these and popping the state.
            // If one is found here, it'll be a parse error, but the token can still be matched
            Some('}') => self.tok_kind(TokenKind::CloseBrace),

            // escape sequences can be a single character after a backslash,
            // or a more complicated unicode escape e.g. `\x{01A1}`
            Some('\\') => self.parse_escape_sequence_main(),

            // All other input characters are literals
            Some(c) => self.tok_kind(TokenKind::Literal(c)),
            None => self.err(ErrorKind::EndOfFile),
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
    fn next_token_group_options(&mut self) -> Result<Token> {
        if matches!(self.last_token_kind, Some(TokenKind::OpenGroupName)) {
            let name = self.parse_id();
            if !name.is_empty() {
                return self.tok_kind(TokenKind::Name(name));
            }
        }

        match self.cursor.next() {
            Some('?') => self.tok_kind(TokenKind::OpenGroupOptions),
            Some(':') => {
                self.swap_state(State::Main)?;
                self.tok_kind(TokenKind::CloseGroupOptions)
            },
            Some('P') if matches!(self.cursor.first(), Some('<')) => {
                self.cursor.skip(1);
                self.tok_kind(TokenKind::OpenGroupName)
            },
            Some('<') => self.tok_kind(TokenKind::OpenGroupName),
            Some('>') => {
                self.swap_state(State::Main)?;
                self.tok_kind(TokenKind::CloseGroupName)
            },
            Some('-') => {
                {
                    let flags = self.flags_mut()?;
                    flags.unset_flags = true;
                }

                self.tok_kind(TokenKind::FlagDelimiter)
            },
            Some(')') => {
                let is_flag_group = matches!(
                    self.last_token_kind,
                    Some(TokenKind::Flag(_) | TokenKind::FlagDelimiter)
                );

                if is_flag_group {
                    let ignore_space = self.flags()?.ignore_space;
                    self.pop_state()?;

                    {
                        let flags = self.flags_mut()?;
                        flags.ignore_space = ignore_space;
                    }
                } else {
                    self.pop_state()?;
                }

                self.tok_kind(TokenKind::CloseGroup)
            },
            Some(c) if Flag::is_flag_char(c) => {
                let flag: Flag = c.try_into().unwrap();

                {
                    let flags = self.flags_mut()?;
                    if flag.is_ignore_whitespace() {
                        flags.ignore_space = !flags.unset_flags;
                    }
                }

                self.tok_kind(TokenKind::Flag(flag))
            },
            Some(c) if c.is_alphabetic() => self.err(ErrorKind::UnrecognizedFlag(c)),
            Some(c) => self.err(ErrorKind::UnexpectedChar(c)),
            None => self.err(ErrorKind::EndOfFile),
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

        match self.cursor.next() {
            // `-` at the end of the class (e.g. `-]`) is just a literal
            Some('-') if matches!(self.cursor.first(), Some(']')) => {
                self.tok_kind(TokenKind::Literal('-'))
            },
            // `--` is the difference token
            Some('-') if matches!(self.cursor.first(), Some('-')) => {
                self.cursor.skip(1);
                self.tok_kind(TokenKind::Difference)
            },
            // `-` following a literal is a range token (e.g. `a-`)
            Some('-') if matches!(self.last_token_kind, Some(TokenKind::Literal(_))) => {
                self.tok_kind(TokenKind::Range)
            },
            // In any other case, `-` is just a literal
            Some('-') => self.tok_kind(TokenKind::Literal('-')),
            // `[:` begins a named class, e.g. `[:alpha:]`
            Some('[') if matches!(self.cursor.first(), Some(':')) => {
                self.push_state(State::ClassName);
                self.tok_kind(TokenKind::OpenBracket)
            },
            // If not part of `[:`, `[` just begins a new sub-class
            Some('[') => {
                self.push_state(State::Class);
                self.tok_kind(TokenKind::OpenBracket)
            },
            // `]` ends a class name or a character class and pops its own state
            Some(']') => {
                self.pop_state()?;
                self.tok_kind(TokenKind::CloseBracket)
            },
            // `^` at the beginning of a character class is a negation
            Some('^') if matches!(self.last_token_kind, Some(TokenKind::OpenBracket)) => {
                self.tok_kind(TokenKind::Negated)
            },
            // `&&` is an intersection token
            Some('&') if matches!(self.cursor.first(), Some('&')) => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Intersection)
            },
            // `~~` is a symmetrical difference token
            Some('~') if matches!(self.cursor.first(), Some('~')) => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Symmetrical)
            },
            // Special characters can be escaped in classes
            Some('\\') => self.parse_escape_sequence_class(),
            // All other characters are just literals
            Some(c) => self.tok_kind(TokenKind::Literal(c)),
            None => self.err(ErrorKind::EndOfFile),
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
        match self.cursor.first() {
            Some(']') => {
                self.cursor.bump();
                self.pop_state()?;
                self.tok_kind(TokenKind::CloseBracket)
            },
            Some(':') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Colon)
            },
            Some('^') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Negated)
            },
            Some(c) if unicode_ident::is_xid_start(c) => {
                let name = self.parse_id();
                self.tok_kind(TokenKind::Name(name))
            },
            // Misplaced id characters shouldn't pop the state; there's probably more
            // valid posix class tokens to find
            Some(c) if unicode_ident::is_xid_continue(c) => {
                self.cursor.bump();
                self.err(ErrorKind::UnexpectedChar(c))
            },
            Some(c) => {
                self.cursor.bump();
                self.pop_state()?;
                self.err(ErrorKind::UnexpectedChar(c))
            },
            None => self.err(ErrorKind::EndOfFile),
        }
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

        match self.cursor.first() {
            // The end of the range pops its own state
            Some('}') => {
                self.cursor.bump();
                self.pop_state()?;
                self.tok_kind(TokenKind::CloseBrace)
            },
            // A comma separates the start and end for the range
            Some(',') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Comma)
            },
            Some(c) if c.is_numeric() => self.parse_number(),
            Some(c) => {
                self.cursor.bump();
                self.pop_state()?;
                self.err(ErrorKind::UnexpectedChar(c))
            },
            None => self.err(ErrorKind::EndOfFile),
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

        match self.cursor.first() {
            Some('{') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::OpenBrace)
            },
            Some('=') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Equal)
            },
            Some('!') => {
                self.cursor.bump();
                self.tok_kind(TokenKind::Bang)
            },
            // `}` ends `UnicodeProperties` state and pops its own state
            Some('}') => {
                self.cursor.bump();
                self.pop_state()?;
                self.tok_kind(TokenKind::CloseBrace)
            },
            // Alphanumeric characters get added to `item` to be returned
            // as either property name or value tokens
            Some(c) if unicode_ident::is_xid_start(c) => {
                let name = self.parse_id();
                self.tok_kind(TokenKind::Name(name))
            },
            // Anything that looks like it was supposed to be part of an ID is an unexpected char,
            // but the state won't be popped because there could be more class tokens to find
            Some(c) if unicode_ident::is_xid_continue(c) => {
                self.cursor.bump();
                self.err(ErrorKind::UnexpectedChar(c))
            },
            // Anything weirder that doesn't belong in a class specifier should create an error
            // _and_ pop the state so that a different mode can look for valid tokens
            Some(c) => {
                self.cursor.bump();
                self.pop_state()?;
                self.err(ErrorKind::UnexpectedChar(c))
            },
            None => self.err(ErrorKind::EndOfFile),
        }
    }

    #[inline]
    fn push_state(&mut self, state: State) {
        self.state.push(state)
    }

    #[inline]
    fn swap_state(&mut self, state: State) -> Result<State> {
        self.state
            .swap(state)
            .map_err(|err| self.raw_err(err.into()))
    }

    #[inline]
    fn pop_state(&mut self) -> Result<State> {
        self.state.pop().map_err(|err| self.raw_err(err.into()))
    }

    #[inline]
    fn pop_all_states(&mut self) -> Result<()> {
        self.state.pop_all().map_err(|err| self.raw_err(err.into()))
    }

    /// Get an immutable copy of the state flags
    #[inline]
    fn flags(&mut self) -> Result<&Flags> {
        let span = self.cursor.span();
        let res = self.state.flags();

        match res {
            Ok(flags) => Ok(flags),
            Err(err) => Err(Error::new(span, err.into())),
        }
    }

    /// Get a mutable copy of the state flags
    #[inline]
    fn flags_mut(&mut self) -> Result<&mut Flags> {
        let span = self.cursor.span();
        let res = self.state.flags_mut();

        match res {
            Ok(flags) => Ok(flags),
            Err(err) => Err(Error::new(span, err.into())),
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
        // every character except a newline to be skipped, instead of only
        // whitespace and `#` characters.
        let mut skipping_comment = false;
        loop {
            match self.cursor.first() {
                // Terminate comment skipping once a newline is seen
                Some('\n') if skipping_comment => skipping_comment = false,
                // Skip all whitespace, or any character if in a comment
                Some(c) if c.is_whitespace() || skipping_comment => (),
                // Enter a comment when `#` is encountered
                Some('#') => skipping_comment = true,
                // In any other case, all whitespace has been skipped
                _ => break,
            }

            self.cursor.skip(1);
        }
    }

    /// Match an escape sequence in the `Main` state
    ///
    /// This matches some anchors, literals, boundaries, and character classes
    /// which are represented as simple one-letter escape sequences. Unicode
    /// classes are also parsed here, as well as hexadecimal numerical escapes.
    fn parse_escape_sequence_main(&mut self) -> Result<Token> {
        match self.cursor.next() {
            Some('A') => self.tok_kind(TokenKind::StartOfText),
            Some('z') => self.tok_kind(TokenKind::EndOfText),
            Some('b') => self.tok_kind(TokenKind::WordBoundary),
            Some('B') => self.tok_kind(TokenKind::NonWordBoundary),
            Some('a') => self.tok_kind(TokenKind::Literal('\x07')),
            Some('f') => self.tok_kind(TokenKind::Literal('\x0C')),
            Some('t') => self.tok_kind(TokenKind::Literal('\t')),
            Some('n') => self.tok_kind(TokenKind::Literal('\n')),
            Some('r') => self.tok_kind(TokenKind::Literal('\r')),
            Some('v') => self.tok_kind(TokenKind::Literal('\x0B')),
            Some('d') => self.tok_kind(TokenKind::Digit(false)),
            Some('D') => self.tok_kind(TokenKind::Digit(true)),
            Some('s') => self.tok_kind(TokenKind::Whitespace(false)),
            Some('S') => self.tok_kind(TokenKind::Whitespace(true)),
            Some('w') => self.tok_kind(TokenKind::WordChar(false)),
            Some('W') => self.tok_kind(TokenKind::WordChar(true)),
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
                self.tok_kind(TokenKind::Literal(c))
            },
            // Everything else is an invalid escape sequence
            Some(c) => self.err(ErrorKind::UnrecognizedEscape(c)),
            // In lieu of an end of file error, we know that an errant \ exists
            // in the input, so use a more specific error
            None => self.err(ErrorKind::UnexpectedChar('\\')),
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
        let bounded = self.cursor.matches('{');

        let digits = match (bounded, ident) {
            (true, _) => 8,
            (_, 'x') => 2,
            (_, 'u') => 4,
            (_, 'U') => 8,
            _ => panic!("accepted unknown hex escape specifier `{}`", ident),
        };

        for _ in 0..digits {
            match self.cursor.next() {
                Some(c) if c.is_ascii_hexdigit() => number.push(c),
                Some('}') if bounded => break,
                Some(c) => return self.err(ErrorKind::InvalidHexDigit(c)),
                None => {
                    return self.err(ErrorKind::UnexpectedEOF(String::from(
                        "expected end of hex literal",
                    )))
                },
            };
        }

        let value = u32::from_str_radix(&number, 16)
            .unwrap_or_else(|_| panic!("accepted invalid hex string: {}", number));
        let c = char::from_u32(value)
            .ok_or_else(|| self.raw_err(ErrorKind::InvalidCharCode(number).into()))?;

        self.tok_kind(TokenKind::Literal(c))
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
        if let Some('{') = self.cursor.first() {
            self.push_state(State::UnicodeProperties);
            return self.tok_kind(TokenKind::UnicodeLong(negated));
        }

        // Otherwise, it's a single-character class
        match self.cursor.next() {
            Some(c) if c.is_ascii_alphabetic() => {
                self.tok_kind(TokenKind::UnicodeShort(c, negated))
            },
            Some(c) => self.err(ErrorKind::UnexpectedChar(c)),
            None => self.err(ErrorKind::UnexpectedEOF(String::from(
                "expected single-character unicode general category",
            ))),
        }
    }

    /// Read a valid identifier using the XID_START and XID_CONTINUE classes from the unicode
    /// identifiers and syntax annex #31.
    ///
    /// Characters will be read from the stream so long as they belong to the XID_CONTINUE category,
    /// except for the first character, which must belong to the XID_START category (a subgroup
    /// of XID_CONTINUE). This allows, for example, numbers to appear in the identifier except in
    /// the starting position.
    fn parse_id(&mut self) -> String {
        let mut id = String::new();
        let mut first = true;
        loop {
            match self.cursor.first() {
                Some(c) if first && unicode_ident::is_xid_start(c) => id.push(c),
                Some(c) if !first && unicode_ident::is_xid_continue(c) => id.push(c),
                _ => break,
            }

            self.cursor.bump();
            first = false;
        }

        id
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
        match self.cursor.next() {
            Some('s') => self.tok_kind(TokenKind::Whitespace(false)),
            Some('S') => self.tok_kind(TokenKind::Whitespace(true)),
            Some('d') => self.tok_kind(TokenKind::Digit(false)),
            Some('D') => self.tok_kind(TokenKind::Digit(true)),
            Some('w') => self.tok_kind(TokenKind::WordChar(false)),
            Some('W') => self.tok_kind(TokenKind::WordChar(true)),
            // Parse a hex escape, which is a numerical representation of a
            // single literal character (e.g. \x7F)
            Some(c) if Tokenizer::is_hex_escape(&c) => self.parse_hex(c),
            Some(c) if Tokenizer::escapes_to_literal_class(&c) => {
                self.tok_kind(TokenKind::Literal(c))
            },
            Some(c) => self.err(ErrorKind::UnrecognizedEscape(c)),
            None => self.err(ErrorKind::UnexpectedChar('\\')),
        }
    }

    /// Parse a number in the `Range` state
    ///
    /// This is specifically for tokenizing numbers withing a character range,
    /// e.g. `.{3,5}`
    fn parse_number(&mut self) -> Result<Token> {
        let mut number = String::new();
        loop {
            match self.cursor.first() {
                // Any numeric character is added to the current number
                Some(c) if c.is_numeric() => {
                    self.cursor.bump();
                    number.push(c);
                },
                // Otherwise just break
                Some(_) => break,
                None => {
                    self.pop_all_states()?;
                    break;
                },
            }
        }

        // This function is only for parsing the number, so a number should
        // always be the output. Parse the input as an unsigned integer
        let value = number
            .parse::<usize>()
            .unwrap_or_else(|_| panic!("accepted invalid number: {}", number));

        self.tok_kind(TokenKind::Number(value))
    }

    /// Check whether a character can be escaped in the `Main` state
    fn escapes_to_literal_main(c: &char) -> bool {
        matches!(
            c,
            '^' | '$' | '.' | '?' | '*' | '+' | '|' | '(' | ')' | '[' | ']' | '{' | '}' | '\\'
        )
    }

    /// Check whether a character can be escaped in the `Class` state
    fn escapes_to_literal_class(c: &char) -> bool {
        matches!(c, '^' | '&' | '~' | '-' | '[' | ']' | ':' | '\\')
    }

    /// Check whether a character is the beginning of a hex escape sequence
    /// (e.g. in `\x7F`, `\u007F`, `\x{12AB}`, etc.)
    fn is_hex_escape(c: &char) -> bool {
        matches!(c, 'x' | 'u' | 'U')
    }

    fn tok_kind(&mut self, kind: TokenKind) -> Result<Token> {
        Ok(self.cursor.map_span(|span| Token { span, kind }))
    }

    #[inline]
    fn err<T>(&mut self, kind: ErrorKind) -> Result<T> {
        Err(self.raw_err(kind.into()))
    }

    fn fatal_err<T>(&mut self, kind: FatalErrorKind) -> Result<T> {
        Err(self.raw_err(kind.into()))
    }

    #[inline]
    fn raw_err(&mut self, cause: ErrorCause) -> Error {
        self.cursor.map_span(|span| Error { span, cause })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Err(Error {
                cause: ErrorCause::Error(ErrorKind::EndOfFile),
                ..
            }) => None,
            result => Some(result),
        }
    }
}

pub struct TokenStackIterator<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> Iterator for TokenStackIterator<'a> {
    type Item = StackResult<TokenStack>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokenizer.next_token_stack() {
            Err(StackError {
                cause: ErrorCause::Error(ErrorKind::EndOfFile),
                ..
            }) => None,
            result => Some(result),
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
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"[\");
        assert_next_tok!(tr, TokenKind::OpenBracket);
        assert_next_err!(tr, ErrorKind::UnexpectedChar('\\'));
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
            TokenKind::OpenGroupOptions,
            TokenKind::CloseGroupOptions,
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
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::OpenGroupName,
            TokenKind::Name(String::from("name")),
            TokenKind::CloseGroupName,
        ];

        assert_tokens(tr, expected);
    }

    #[test]
    fn group_errors() {
        // This used to create an unexpected EOF error because the group name was all one token.
        // now, the parser should handle this error as the group is not complete
        let mut tr = Tokenizer::new(r"(?P<name");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_tok!(tr, TokenKind::OpenGroupOptions);
        assert_next_tok!(tr, TokenKind::OpenGroupName);
        assert_next_tok!(tr, TokenKind::Name(_));
        assert_next_none!(tr);

        // This will now create an error, as the tokenizer will keep trying to build the name
        // until a non-identifier character is found
        let mut tr = Tokenizer::new(r"(?P<name%");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_tok!(tr, TokenKind::OpenGroupOptions);
        assert_next_tok!(tr, TokenKind::OpenGroupName);
        assert_next_tok!(tr, TokenKind::Name(_));
        assert_next_err!(tr, ErrorKind::UnexpectedChar('%'));

        // Different requirements for the first character in a name
        let mut tr = Tokenizer::new(r"(?P<4name");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_tok!(tr, TokenKind::OpenGroupOptions);
        assert_next_tok!(tr, TokenKind::OpenGroupName);
        assert_next_err!(tr, ErrorKind::UnexpectedChar('4'));
        assert_next_tok!(tr, TokenKind::Name(_));

        let mut tr = Tokenizer::new(r"(?P<name4");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_tok!(tr, TokenKind::OpenGroupOptions);
        assert_next_tok!(tr, TokenKind::OpenGroupName);
        assert_next_tok!(tr, TokenKind::Name(_));
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"(?q");
        assert_next_tok!(tr, TokenKind::OpenGroup);
        assert_next_tok!(tr, TokenKind::OpenGroupOptions);
        assert_next_err!(tr, ErrorKind::UnrecognizedFlag('q'));
        assert_next_none!(tr);
    }

    #[test]
    fn flag_group() {
        // Test a flags group
        let tr = Tokenizer::new(r"(?isUx)");
        let expected = vec![
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::Flag(Flag::DotMatchesNewline),
            TokenKind::Flag(Flag::SwapGreed),
            TokenKind::Flag(Flag::IgnoreWhitespace),
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
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::MultiLine),
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroupOptions,
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
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::Flag(Flag::DotMatchesNewline),
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::SwapGreed),
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::Flag(Flag::SwapGreed),
            TokenKind::CloseGroupOptions,
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::DotMatchesNewline),
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::FlagDelimiter,
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
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroup,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::IgnoreWhitespace),
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
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroupOptions,
            TokenKind::Literal('z'),
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroupOptions,
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
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::IgnoreWhitespace),
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
        let tr = Tokenizer::new(r"[:a:][[:a:]a-z]");
        let expected = vec![
            TokenKind::OpenBracket,
            TokenKind::Literal(':'),
            TokenKind::Literal('a'),
            TokenKind::Literal(':'),
            TokenKind::CloseBracket,
            TokenKind::OpenBracket,
            TokenKind::OpenBracket,
            TokenKind::Colon,
            TokenKind::Name(String::from("a")),
            TokenKind::Colon,
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
            TokenKind::Colon,
            TokenKind::Negated,
            TokenKind::Name(String::from("lower")),
            TokenKind::Colon,
            TokenKind::CloseBracket,
            TokenKind::Difference,
            TokenKind::OpenBracket,
            TokenKind::Colon,
            TokenKind::Name(String::from("alnum")),
            TokenKind::Colon,
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
            TokenKind::Intersection,
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
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
        assert_next_none!(tr);

        let mut tr = Tokenizer::new(r"{1a");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_tok!(tr, TokenKind::Number(1));
        assert_next_err!(tr, ErrorKind::UnexpectedChar('a'));
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
            TokenKind::UnicodeLong(false),
            TokenKind::OpenBrace,
            TokenKind::Name(String::from("Mn")),
            TokenKind::CloseBrace,
            TokenKind::UnicodeLong(true),
            TokenKind::OpenBrace,
            TokenKind::Name(String::from("sc")),
            TokenKind::Bang,
            TokenKind::Equal,
            TokenKind::Name(String::from("Greek")),
            TokenKind::CloseBrace,
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
        let mut tr = Tokenizer::new(r"{\");
        assert_next_tok!(tr, TokenKind::OpenBrace);
        assert_next_err!(tr, ErrorKind::UnexpectedChar('\\'));
        assert_next_none!(tr);
    }
}
