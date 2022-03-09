use crate::tokenize::error::*;
use crate::tokenize::input::*;
use crate::tokenize::state::*;
use crate::tokenize::token::*;

pub struct Tokenizer<'a> {
    input: Input<'a>,
    state: StateStack,
    last_token_kind: Option<TokenKind>,
}

impl<'a> Tokenizer<'a> {
    pub fn new<T: Into<&'a str>>(input: T) -> Self {
        Tokenizer {
            input: Input::new(input),
            state: StateStack::new(),
            last_token_kind: None,
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let token = match *self.state.get() {
            State::Main => self.next_token_main(),
            State::Group => self.next_token_group(),
            State::Class => self.next_token_class(),
            State::Range => self.next_token_range(),
        };

        self.last_token_kind = token.as_ref().ok()
            .map(|t| t.kind.clone());

        token
    }

    fn next_token_main(&mut self) -> Result<Token> {
        match self.input.next() {
            Some('^') => self.input.token(TokenKind::StartOfLine),
            Some('$') => self.input.token(TokenKind::EndOfLine),
            Some('.') => self.input.token(TokenKind::Any),
            Some('?') => self.input.token(TokenKind::Question),
            Some('*') => self.input.token(TokenKind::ZeroOrMore),
            Some('+') => self.input.token(TokenKind::OneOrMore),
            Some('|') => self.input.token(TokenKind::Alternate),
            Some('(') => {
                self.state.push(State::Group);
                self.input.token(TokenKind::OpenGroup)
            },
            Some(')') => self.input.token(TokenKind::CloseGroup),
            Some('[') => {
                self.state.push(State::Class);
                self.input.token(TokenKind::OpenBracket)
            },
            Some(']') => self.input.token(TokenKind::CloseBracket),
            Some('{') => {
                self.state.push(State::Range);
                self.input.token(TokenKind::OpenBrace)
            },
            Some('}') => self.input.token(TokenKind::CloseBrace),
            Some('\\') => self.parse_escape_sequence_main(),
            Some(c) => self.input.token(TokenKind::Literal(c)),
            None => Err(TokenizeError::EndOfFile),
        }
    }

    fn next_token_group(&mut self) -> Result<Token> {
        match self.input.peek() {
            Some('?') => {
                self.input.next();
                match self.input.peek() {
                    Some(':') => {
                        self.input.next();
                        self.input.token(TokenKind::NonCapturing)
                    },
                    Some('P') => {
                        self.input.next();
                        self.parse_named_group()
                    },
                    Some(_) => {
                        self.parse_flags()
                    },
                    None => Err(TokenizeError::EndOfFile),
                }
            },
            _ => {
                self.state.pop();
                self.next_token_main()
            },
        }
    }

    fn next_token_class(&mut self) -> Result<Token> {
        match self.input.next() {
            Some('-') if matches!(self.input.peek(), Some(']')) =>
                self.input.token(TokenKind::Literal('-')),
            Some('-') if matches!(self.last_token_kind, Some(TokenKind::Literal(_))) =>
                self.input.token(TokenKind::Range),
            Some('-') if matches!(self.input.peek(), Some('-')) => {
                self.input.next();
                self.input.token(TokenKind::RangeDifference)
            },
            Some('-') => self.input.token(TokenKind::Literal('-')),
            Some('[') if matches!(self.input.peek(), Some(':')) =>
                self.parse_class_name(),
            Some('[') => {
                self.state.push(State::Class);
                self.input.token(TokenKind::OpenBracket)
            }
            Some(']') => {
                self.state.pop();
                self.input.token(TokenKind::CloseBracket)
            },
            Some('^') if matches!(self.last_token_kind, Some(TokenKind::OpenBracket)) =>
                self.input.token(TokenKind::Negated),
            Some('&') if matches!(self.input.peek(), Some('&')) => {
                self.input.next();
                self.input.token(TokenKind::RangeIntersection)
            },
            Some('~') if matches!(self.input.peek(), Some('~')) => {
                self.input.next();
                self.input.token(TokenKind::RangeSymmetrical)
            }
            Some('\\') => self.parse_escape_sequence_class(),
            Some(c) => self.input.token(TokenKind::Literal(c)),
            None => Err(TokenizeError::EndOfFile),
        }
    }

    fn next_token_range(&mut self) -> Result<Token> {
        match self.input.peek() {
            Some('}') => {
                self.input.next();
                self.state.pop();
                self.input.token(TokenKind::CloseBrace)
            },
            Some(',') => {
                self.input.next();
                self.input.token(TokenKind::Comma)
            },
            Some(c) => self.parse_number(),
            None => Err(TokenizeError::EndOfFile),
        }
    }

    fn parse_escape_sequence_main(&mut self) -> Result<Token> {
        match self.input.next() {
            Some('A') => self.input.token(TokenKind::StartOfText),
            Some('z') => self.input.token(TokenKind::EndOfText),
            Some('b') => self.input.token(TokenKind::WordBoundary),
            Some('B') => self.input.token(TokenKind::NonWordBoundary),
            Some('a') => self.input.token(TokenKind::Literal('\x07')),
            Some('f') => self.input.token(TokenKind::Literal('\x0C')),
            Some('t') => self.input.token(TokenKind::Literal('\t')),
            Some('n') => self.input.token(TokenKind::Literal('\n')),
            Some('r') => self.input.token(TokenKind::Literal('\r')),
            Some('v') => self.input.token(TokenKind::Literal('\x0B')),
            Some('d') => self.input.token(TokenKind::Digit(false)),
            Some('D') => self.input.token(TokenKind::Digit(true)),
            Some('s') => self.input.token(TokenKind::Whitespace(false)),
            Some('S') => self.input.token(TokenKind::Whitespace(true)),
            Some('w') => self.input.token(TokenKind::WordChar(false)),
            Some('W') => self.input.token(TokenKind::WordChar(true)),
            Some('p') => self.parse_unicode(false),
            Some('P') => self.parse_unicode(true),
            Some(c) if Tokenizer::is_hex_escape(&c) => self.parse_hex(c),
            Some(c) if Tokenizer::escapes_to_literal_main(&c) =>
                self.input.token(TokenKind::Literal(c)),
            Some(c) => Err(TokenizeError::UnrecognizedEscape(c)),
            None => Err(TokenizeError::UnexpectedChar('\\')),
        }
    }

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
                Some(c) => return Err(TokenizeError::InvalidHexDigit(c)),
                None => return Err(TokenizeError::EndOfFile),
            };
        }

        let value = u32::from_str_radix(&number, 16)
            .expect(&format!("accepted invalid hex string: {}", number));
        let c = char::from_u32(value)
            .ok_or(TokenizeError::InvalidCharCode(number))?;
        self.input.token(TokenKind::Literal(c))
    }

    fn parse_unicode(&mut self, negated: bool) -> Result<Token> {
        let mut class = String::new();
        let mut class_char = '\0';
        let bounded = self.input.matches('{');

        loop {
            match self.input.next() {
                Some('}') if bounded => break,
                Some(c) => {
                    class.push(c);
                    class_char = c;
                },
                None => return Err(TokenizeError::EndOfFile),
            }

            if !bounded {
                break;
            }
        }

        if bounded {
            self.input.token(TokenKind::UnicodeLong(class, negated))
        } else {
            self.input.token(TokenKind::UnicodeShort(class_char, negated))
        }
    }

    fn parse_named_group(&mut self) -> Result<Token> {
        self.input.expect('<')?;
        let mut name = String::new();
        loop {
            match self.input.next() {
                Some('>') => break,
                Some(c) => name.push(c),
                None => return Err(TokenizeError::EndOfFile),
            }
        }

        self.state.pop();
        self.input.token(TokenKind::Name(name))
    }

    fn parse_flags(&mut self) -> Result<Token> {
        let mut flags = Vec::new();
        loop {
            match self.input.peek() {
                Some(c) if Tokenizer::is_group_flag(&c) => {
                    self.input.next();
                    flags.push(c);
                },
                Some(':') => {
                    self.input.next();
                    break;
                }
                Some(')') => break,
                Some(c) => return Err(TokenizeError::UnrecognizedFlag(c)),
                None => return Err(TokenizeError::EndOfFile),
            }
        }

        self.state.pop();
        self.input.token(TokenKind::Flags(flags))
    }

    fn parse_class_name(&mut self) -> Result<Token> {
        self.input.expect(':')?;
        let negated = self.input.matches('^');
        let mut name = String::new();

        loop {
            match self.input.next() {
                Some(':') => break,
                Some(c) => name.push(c),
                None => return Err(TokenizeError::EndOfFile),
            }
        }

        self.input.token(TokenKind::ClassName(name, negated))
    }

    fn parse_escape_sequence_class(&mut self) -> Result<Token> {
        match self.input.next() {
            Some(c) if Tokenizer::escapes_to_literal_class(&c) =>
                self.input.token(TokenKind::Literal(c)),
            Some(c) => Err(TokenizeError::UnrecognizedEscape(c)),
            None => Err(TokenizeError::UnexpectedChar('\\')),
        }
    }

    fn parse_number(&mut self) -> Result<Token> {
        let mut number = String::new();
        loop {
            match self.input.peek() {
                Some(',') => break,
                Some('}') => break,
                Some(c) if c.is_numeric() => {
                    self.input.next();
                    number.push(c);
                }
                Some(c) => return Err(TokenizeError::UnexpectedChar(c)),
                None => return Err(TokenizeError::EndOfFile),
            }
        }

        let value = number.parse::<u32>()
            .expect(&format!("accepted invalid number: {}", number));
        self.input.token(TokenKind::Number(value))
    }

    fn escapes_to_literal_main(c: &char) -> bool {
        match c {
            '^' |
            '$' |
            '.' |
            '?' |
            '*' |
            '+' |
            '|' |
            '(' |
            ')' |
            '[' |
            ']' |
            '{' |
            '}' |
            '\\' => true,
            _ => false,
        }
    }

    fn escapes_to_literal_class(c: &char) -> bool {
        match c {
            '^' |
            '&' |
            '~' |
            '-' |
            '[' |
            ']' |
            ':' |
            '\\' => true,
            _ => false,
        }
    }

    fn is_group_flag(c: &char) -> bool {
        match c {
            'i' |
            'm' |
            's' |
            'U' |
            'u' |
            'x' => true,
            _ => false,
        }
    }

    fn is_hex_escape(c: &char) -> bool {
        match c {
            'x' |
            'u' |
            'U' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::tokenize::assert_tokens;

    #[test]
    fn test_simple_main() {
        let mut tr = Tokenizer::new(r"a^$.?*+|)]}");
        assert_tokens(tr, vec![
            TokenKind::Literal('a'),
            TokenKind::StartOfLine,
            TokenKind::EndOfLine,
            TokenKind::Any,
            TokenKind::Question,
            TokenKind::ZeroOrMore,
            TokenKind::OneOrMore,
            TokenKind::Alternate,
            TokenKind::CloseGroup,
            TokenKind::CloseBracket,
            TokenKind::CloseBrace,
        ]);
    }

    #[test]
    fn test_escapes() {
        let mut tr = Tokenizer::new(r"\A\z\b\B\a\f\t\n\r\v\d\D\s\S\w\W");
        assert_tokens(tr, vec![
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
        ]);

        let mut tr = Tokenizer::new(r"\^\$\.\?\*\+\|\(\)\[\]\{\}\\");
        assert_tokens(tr, vec![
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
        ]);

        let mut tr = Tokenizer::new(r"\:");
        assert!(matches!(tr.next_token(), Err(TokenizeError::UnrecognizedEscape(':'))));
    }

    #[test]
    fn test_groups() {
        let mut tr = Tokenizer::new(r"(a)b");
        assert_tokens(tr, vec![
            TokenKind::OpenGroup,
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::Literal('b'),
        ]);

        let mut tr = Tokenizer::new(r"(?:ab)");
        assert_tokens(tr, vec![
            TokenKind::OpenGroup,
            TokenKind::NonCapturing,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::CloseGroup,
        ]);

        let mut tr = Tokenizer::new(r"(?P<name>");
        assert_tokens(tr, vec![
            TokenKind::OpenGroup,
            TokenKind::Name(String::from("name")),
        ]);

        let mut tr = Tokenizer::new(r"(?isUx)");
        assert_tokens(tr, vec![
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['i', 's', 'U', 'x']),
            TokenKind::CloseGroup,
        ]);

        let mut tr = Tokenizer::new(r"(?mx:a)b");
        assert_tokens(tr, vec![
            TokenKind::OpenGroup,
            TokenKind::Flags(vec!['m', 'x']),
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::Literal('b'),
        ]);
    }

    #[test]
    fn test_classes() {
        let mut tr = Tokenizer::new(r"[^-^a-z-]");
        assert_tokens(tr, vec![
            TokenKind::OpenBracket,
            TokenKind::Negated,
            TokenKind::Literal('-'),
            TokenKind::Literal('^'),
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::Literal('-'),
            TokenKind::CloseBracket,
        ]);

        let mut tr = Tokenizer::new(r"[x[aA0~~[:^lower:]--[:alnum:");
        assert_tokens(tr, vec![
            TokenKind::OpenBracket,
            TokenKind::Literal('x'),
            TokenKind::OpenBracket,
            TokenKind::Literal('a'),
            TokenKind::Literal('A'),
            TokenKind::Literal('0'),
            TokenKind::RangeSymmetrical,
            TokenKind::ClassName(String::from("lower"), true),
            TokenKind::RangeDifference,
            TokenKind::ClassName(String::from("alnum"), false),
        ]);

        let mut tr = Tokenizer::new(r"[[^:abc:]][:abc:]]]");
        assert_tokens(tr, vec![
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
        ]);
    }
}