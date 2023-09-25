use std::convert::TryFrom;

use aglet_text::Span;

use crate::parse::ast::*;
use crate::parse::error::*;
use crate::parse::input::Input;
use crate::tokenize::{self, Flag, Token, TokenKind};

/// Parse regular expressions from a [token stream](tokenize::Tokenizer)
///
/// Uses a recursive-descent approach to parse a regular expression, with a grammar free
/// of left-recursion.
///
/// # Grammar:
///
/// ```grammar
/// expr ->
///     | alternation
///     | \e
/// alternation ->
///     | concatenation ('|' alternation)?
/// concatenation ->
///     | repetition concatenation?
/// repetition ->
///     | item repetition-spec?
/// repetition_spec ->
///     | repetition_range
///     | QUESTION
///     | STAR
///     | PLUS
/// repetition_range ->
///     | '{' repetition_range_contents '}'
/// repetition_range_contents ->
///     | NUMBER? (',' NUMBER?)?
/// item ->
///     | DOT
///     | LITERAL
///     | DIGIT_CLASS
///     | WHITESPACE_CLASS
///     | WORD_CLASS
///     | BOUNDARY
///     | group
///     | class
/// group ->
///     | '(' group_contents ')'
/// group_contents ->
///     | '?' flags? ':' expr
///     | '?' flags
///     | '?' 'P'? '<' GROUP_NAME '>' expr
///     | expr
/// class ->
///     | unicode_class
///     | '[' specified_class ']'
/// unicode_class ->
///     | unicode_escape CHAR
///     | unicode_escape '{' UNICODE_PROP_VALUE '}'
///     | unicode_escape '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
///     | unicode_escape '{' UNICODE_PROP_NAME '!=' UNICODE_PROP_VALUE '}'
/// unicode_escape ->
///     | '\p'
///     | '\P'
/// specified_class ->
///     | NEGATED? spec_item+
/// spec_item ->
///     | spec_term spec_set?
/// spec_term ->
///     | LITERAL ('-' LITERAL)?
///     | DIGIT_CLASS
///     | WHITESPACE_CLASS
///     | WORD_CLASS
///     | '[' POSIX_NAME ']'
///     | '[' specified_class ']'
/// spec_set ->
///     | '~~' spec_term spec_set?
///     | '--' spec_item spec_set?
///     | '&&' spec_item spec_set?
/// ```
pub struct Parser<'a> {
    input: Input<'a>,
    group_index: usize,
}

impl<'a> Parser<'a> {
    /// Create a new parser from a token iterator
    ///
    /// Since [`Tokenizer`](tokenize::Tokenizer) implements Iterator, a parser can be built
    /// from it directly
    pub fn new<T>(input: T) -> Self
    where
        T: Iterator<Item = tokenize::Result<Token>> + 'a,
    {
        Parser {
            input: Input::new(input),
            group_index: 1,
        }
    }

    /// Parse the regular expression into a [syntax tree](Ast)
    pub fn parse(mut self) -> Result<Ast> {
        Ok(Ast {
            head: self.parse_expr()?,
        })
    }

    /// Parse an expression, starting with alternations as the weakest binding operation
    ///
    /// An alternation is a list of expressions separated by `|` symbols, where the
    /// regular expression must match one of the alternatives.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// expr ->
    ///     | alternation
    ///     | \e
    /// alternation ->
    ///     | concatenation ('|' alternation)?
    /// ```
    pub fn parse_expr(&mut self) -> Result<Expr> {
        let mut items = Vec::new();

        // don't recursively match concatenation and then alternation as in the grammar,
        // rather just use a loop to match a series of concatenations
        loop {
            // parse an alternate
            let item = match self.parse_concatenation()? {
                Some(item) => item,

                // If there is nothing in the alternate (e.g., `/abc|/`) then an empty
                // expression is allowed
                None => Expr {
                    span: Span::new(self.input.position(), self.input.position()),
                    kind: ExprKind::Empty,
                },
            };

            // there will always be at least one item, since even an empty token stream will
            // first match an `ExprKind::Empty`
            items.push(item);

            // continue matching expressions as alternates as long as there are more
            // `|` symbols separating them
            if !self.input.has_where(TokenKind::is_alternate)? {
                break;
            }
        }

        if items.len() > 1 {
            // The alternation expression type only makes sense if there is more than one
            // alternate
            let span = Span::wrap(&items[0].span, &items[items.len() - 1].span);
            let kind = ExprKind::Alternation(Alternation { span, items });

            Ok(Expr { span, kind })
        } else {
            // If there is only one alternate, don't bother wrapping it in an `Alternation`
            // and just use the underlying expression instead. There is always at least one
            // item so it's safe to unwrap
            Ok(items.pop().unwrap())
        }
    }

    /// Parse a concatenation, using repetition as the next weakest binding operation
    ///
    /// A concatenation is a series of sub-expressions directly next to each other with
    /// no conjoining symbol, where the regular expression will match them one after the other.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// concatenation ->
    ///     | repetition concatination?
    /// ```
    pub fn parse_concatenation(&mut self) -> Result<Option<Expr>> {
        let mut items = Vec::new();

        // don't recursively match repetitions and concatenations, rather just use a loop
        // to match a series of repetitions, accomplishing the same thing
        while let Some(item) = self.parse_repetition()? {
            items.push(item);
        }

        // A concatenation only makes sense so long as there's more than one element
        // to concatenate, otherwise the sub-expression type should pass through
        if items.len() > 1 {
            let span = Span::wrap(&items[0].span, &items[items.len() - 1].span);
            Ok(Some(Expr {
                span,
                kind: ExprKind::Concatenation(Concatenation { span, items }),
            }))
        } else {
            // If there's less than two items, return the subexpression instead of a new
            // concatenation (or None if no items were matched)
            Ok(items.pop())
        }
    }

    /// Parse a repetition of items
    ///
    /// Repetition specifies the number of times an item should be matched:
    ///
    /// * exactly one time (default),
    /// * zero or one times,
    /// * zero or more times,
    /// * one or more times,
    /// * exactly `n` times,
    /// * `n` or more times,
    /// * up to `n` times,
    /// * between `n` and `m` times
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// repetition ->
    ///     | item repetition-spec?
    /// repetition_spec ->
    ///     | repetition_range
    ///     | QUESTION
    ///     | STAR
    ///     | PLUS
    /// repetition_range ->
    ///     | '{' repetition_range_contents '}'
    /// repetition_range_contents ->
    ///     | NUMBER? (',' NUMBER?)?
    /// ```
    pub fn parse_repetition(&mut self) -> Result<Option<Expr>> {
        // match the item to be repeated
        let Some(item) = self.parse_item()? else {
            return Ok(None);
        };

        // the default quantity (exactly once) doesn't require any text to be matched,
        // so matching a repetition specifier is optional. When not present, the sub-item
        // will pass through instead of a repetition expr
        if let Some(kind) = self.parse_repetition_spec()? {
            let span = Span::new(item.span.start, self.input.position());
            let repetition = Repetition {
                span,
                kind,
                item: Box::new(item),
            };

            // a repetition specifier is present, so encode it in a repetition expression
            Ok(Some(Expr {
                span,
                kind: ExprKind::Repetition(repetition),
            }))
        } else {
            // no repetition specifier was present, so pass through the matched item
            Ok(Some(item))
        }
    }

    /// Parse a repetition specifier
    ///
    /// The specifier is the optional part of a repetition expression, so the parser generates
    /// a [`RepetitionKind`] rather than an [`Expr`].
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// repetition_spec ->
    ///     | repetition_range
    ///     | QUESTION
    ///     | STAR
    ///     | PLUS
    /// repetition_range ->
    ///     | '{' repetition_range_contents '}'
    /// repetition_range_contents ->
    ///     | NUMBER? (',' NUMBER?)?
    /// ```
    pub fn parse_repetition_spec(&mut self) -> Result<Option<RepetitionKind>> {
        self.parse_alts(vec![
            Self::parse_question,
            Self::parse_star,
            Self::parse_plus,
            Self::parse_repetition_range,
        ])
    }

    /// Parse the zero-or-one quantity specifier (`?`)
    pub fn parse_question(&mut self) -> Result<Option<RepetitionKind>> {
        if self.input.has_where(TokenKind::is_question)? {
            self.input.next();
        }

        Ok(Some(RepetitionKind::ZeroOrOne))
    }

    /// Parse the zero-or-more quantity specifier (`*`)
    pub fn parse_star(&mut self) -> Result<Option<RepetitionKind>> {
        if self.input.has_where(TokenKind::is_star)? {
            self.input.next();
        }

        Ok(Some(RepetitionKind::ZeroOrMore))
    }

    /// Parse the one-or-more quantity specifier (`+`)
    pub fn parse_plus(&mut self) -> Result<Option<RepetitionKind>> {
        if self.input.has_where(TokenKind::is_plus)? {
            self.input.next();
        }

        Ok(Some(RepetitionKind::OneOrMore))
    }

    /// Parse a specified range for a repetition
    ///
    /// A repetition range specifies a custom quantity for an item not expressible by
    /// the `?`, `*`, and `+` specifiers:
    ///
    /// * `n` to `m`: `{n,m}`
    /// * up to `n`: `{,n}` or `{0,n}`
    /// * `n` or more: `{n,}`
    /// * exactly `n`: `{n}`
    ///
    /// The can also be used in place of the `?`, `*`, and `+` specifiers:
    ///
    /// * zero or one: `{0,1}`
    /// * zero or more: `{0,}`
    /// * one or more: `{1,}`
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// repetition_range ->
    ///     | '{' repetition_range_contents '}'
    /// repetition_range_contents ->
    ///     | NUMBER? (',' NUMBER?)?
    /// ```
    pub fn parse_repetition_range(&mut self) -> Result<Option<RepetitionKind>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_open_brace)? else {
            return Ok(None)
        };

        // both numbers are optional, `{,}` is equivalent to `{0,}` and `*`
        let mut start: Option<usize> = None;
        let mut end: Option<usize> = None;

        // first number
        if let Some(tok) = self.input.match_where(TokenKind::is_number)? {
            let TokenKind::Number(number) = tok.kind;
            start = Some(number);
        }

        // the second number is only allowed if a comma is present
        if let Some(_) = self.input.match_where(TokenKind::is_comma)? {
            if let Some(tok) = self.input.match_where(TokenKind::is_number)? {
                let TokenKind::Number(number) = tok.kind;
                end = Some(number);
            }
        }

        let close_tok = self.expect_match("end of range `}`", TokenKind::is_close_brace)?;

        let span = Span::wrap(&open_tok.span, &close_tok.span);
        Ok(Some(RepetitionKind::Range(Range { span, start, end })))
    }

    /// Parse an item to be matched by the regular expression
    ///
    /// Items are the basic units of the regular expression and represent actual text to
    /// be matched. The simplest items (`DOT`, `LITERAL`) match a single character (unless
    /// accompanied by a repetition specifier), and a `BOUNDARY` matches a zero-width location
    /// in the input (e.g. the beginning of a word). The non-terminal `class` item also
    /// matches a single character, but is constructed from a more complicated specification.
    ///
    /// A `group` item is a sub-expression that may match anything. It is matched at this
    /// level because it can be a component of an alternation, concatenation, or repetition.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// item ->
    ///     | DOT
    ///     | LITERAL
    ///     | DIGIT_CLASS
    ///     | WHITESPACE_CLASS
    ///     | WORD_CLASS
    ///     | BOUNDARY
    ///     | group
    ///     | class
    /// ```
    pub fn parse_item(&mut self) -> Result<Option<Expr>> {
        match self.input.peek_kind() {
            Some(Ok(
                | TokenKind::Star
                | TokenKind::Plus
                | TokenKind::Question
                | TokenKind::OpenBrace,
            )) => {
                Err(self.illegal_tok("`.`, `[`, `(`, boundary, class, or literal"))
            },
            _ => Ok(())
        }?;

        self.parse_alts(vec![
            Self::parse_dot,
            Self::parse_literal,
            Self::parse_digit_class,
            Self::parse_whitespace_class,
            Self::parse_word_class,
            Self::parse_boundary,
            Self::parse_group,
            Self::parse_class,
        ])
    }

    /// Parse the "any" item (`.`)
    pub fn parse_dot(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_dot)? else {
            return Ok(None)
        };

        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Any,
        }))
    }

    /// Parse a literal item (a single character)
    pub fn parse_literal(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_literal)? else {
            return Ok(None)
        };

        let TokenKind::Literal(c) = tok.kind;
        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Literal(c),
        }))
    }

    /// Parse a digit short class, `\d` or `\D`
    pub fn parse_digit_class(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_digit)? else {
            return Ok(None)
        };

        let TokenKind::Digit(negated) = tok.kind;
        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Digit(negated),
        }))
    }

    /// Parse a whitespace short class, `\s` or `\S`
    pub fn parse_whitespace_class(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_whitespace)? else {
            return Ok(None)
        };

        let TokenKind::Whitespace(negated) = tok.kind;
        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Whitespace(negated),
        }))
    }

    /// Parse a word character short class, `\w` or `\W`
    pub fn parse_word_class(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_word_char)? else {
            return Ok(None)
        };

        let TokenKind::WordChar(negated) = tok.kind;
        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::WordChar(negated),
        }))
    }

    /// Parse a boundary item
    ///
    /// Boundaries have already been condensed into single tokens by the tokenizer,
    /// but may be a single character (e.g. `$` or `^`) or several (`\b`)
    pub fn parse_boundary(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_boundary)? else {
            return Ok(None);
        };

        let kind = match BoundaryKind::try_from(tok.kind) {
            Ok(kind) => Ok(kind),
            Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
        }?;

        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Boundary(Boundary { span: tok.span, kind }),
        }))
    }

    /// Parse a group item
    ///
    /// A group is usually a sub-expression that contains another entire regex. It is surrounded
    /// by parentheses and can begin with some options that change its behaviour.
    ///
    /// Groups can be capturing (the text matched by the sub-expression can be retrieved
    /// on its own), named, non-capturing, and contain flags which alter the behaviour of the
    /// engine. For example, the case-insensitive flag will make matches within the group
    /// case-insensitive.
    ///
    /// The `FLAGS` group does not contain a sub-expression, but applies the effect of the
    /// specified flags to the current expression.
    ///
    /// The ignore-whitespace ('x') flag was already processed by the tokenizer and has no
    /// effect from this stage.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// group ->
    ///     | '(' group_contents ')'
    /// group_contents ->
    ///     | NON_CAPTURING expr
    ///     | NON_CAPTURING_FLAGS expr
    ///     | GROUP_NAME expr
    ///     | FLAGS
    /// ```
    pub fn parse_group(&mut self) -> Result<Option<Expr>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_open_group)? else {
            return Ok(None);
        };

        // use a sub-parse to match the group's type and its contents, if applicable
        let kind = self.parse_group_contents()?;

        let close_tok = self.expect_match("end of group `)`", TokenKind::is_close_group)?;

        let span = Span::wrap(&open_tok.span, &close_tok.span);

        Ok(Some(Expr {
            span,
            kind: ExprKind::Group(Group { span, kind }),
        }))
    }

    /// Parse the type and contents of a group
    ///
    /// Because the [`parse_group`][1] parser is responsible for the open and close braces
    /// surrounding the group and therefore its span, this parser returns only the group
    /// kind (which contains the subexpression, if applicable).
    ///
    /// This parser cannot return `None`, as an empty group that expects an expression
    /// will be populated with an [`ExprKind::Empty`]
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// group_contents ->
    ///     | '?' flags? ':' expr
    ///     | '?' flags
    ///     | '?' 'P'? '<' GROUP_NAME '>' expr
    ///     | expr
    /// ```
    ///
    /// [1]: crate::parse::Parser::parse_group
    pub fn parse_group_contents(&mut self) -> Result<GroupKind> {
        let res = self
            .parse_alts(vec![
                Self::parse_group_with_header,
                Self::parse_capturing_group,
            ])?
            // one of the group contents parsers must always produce a match
            .expect("group contents should not be None");

        Ok(res)
    }

    pub fn parse_group_with_header(&mut self) -> Result<Option<GroupKind>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_open_group_options)? else {
            return Ok(None);
        };

        let group_kind = if self.input.match_where(TokenKind::is_open_group_name)?.is_some() {
            let name_tok = self.expect_match("group name", TokenKind::is_name)?;
            self.expect_match("end of group name `>`", TokenKind::is_close_group_name)?;

            let expr = self.parse_expr()?;
            let TokenKind::Name(n) = name_tok.kind;
            let name = StringSpan { span: name_tok.span, value: n };

            GroupKind::Named(NamedGroup {
                name,
                span: Span::wrap(&open_tok.span, &expr.span),
                expr: Box::new(expr),
            })
        } else {
            let flags = self.parse_flags()?;
            if self.input.match_where(TokenKind::is_close_group_options)?.is_some() {
                let expr = self.parse_expr()?;

                GroupKind::NonCapturing(NonCapturingGroup {
                    span: expr.span,
                    expr: Box::new(expr),
                    flags,
                })
            } else {
                let flags = flags.unwrap_or_else(|| Flags {
                    span: Span::new(open_tok.span.end, open_tok.span.end),
                    set_flags: Vec::new(),
                    clear_flags: Vec::new(),
                });

                GroupKind::Flags(FlagGroup {
                    flags,
                })
            }
        };

        Ok(Some(group_kind))
    }

    /// Parse a named group
    ///
    /// Named groups are capturing groups, but instead of being identified by a
    /// number, are also identified with a string.
    ///
    /// Named groups begin with `?P<name>` or `?<name>`, where `name` is the name of the
    /// sub-expression. This parser expects the `?` token to have already been consumed
    pub fn parse_named_group(&mut self) -> Result<Option<GroupKind>> {
        // TODO: remove this but preserve the documentation (parse_group_with_header)
        unimplemented!()
    }

    /// Parse a non-capturing group.
    ///
    /// A non-capturing group is not represented by a name or index; it optionally matches some
    /// text and optionally specifies some flags.
    ///
    /// This parser expects the `?` token to have already been consumed
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// group_contents ->
    ///     | '?' flags? ':' expr
    ///     | '?' flags
    /// ```
    pub fn parse_non_capturing_group(&mut self) -> Result<Option<GroupKind>> {
        // TODO: remove this but preserve the documentation (parse_group_with_header)
        unimplemented!()
    }

    pub fn parse_flags(&mut self) -> Result<Option<Flags>> {
        let mut span = None;
        let mut set_flags = vec![];
        let mut clear_flags = vec![];
        let mut clearing = false;

        while let Some(tok) = self.input.match_where(TokenKind::is_flag_or_delimiter)? {
            if let Some(s) = span {
                span = Some(Span::wrap(&s, &tok.span));
            } else {
                span = Some(tok.span);
            }

            if tok.kind.is_flag() {
                let TokenKind::Flag(f) = tok.kind;
                if clearing {
                    clear_flags.push(f.into())
                } else {
                    set_flags.push(f.into())
                }
            } else if tok.kind.is_flag_delimiter() {
                // TODO: multiple delimiters should produce an error
                clearing = true;
            }
        }

        let span = span.unwrap_or_else(|| Span::new(self.input.position(), self.input.position()));

        Ok(Some(Flags {
            span,
            set_flags,
            clear_flags,
        }))
    }

    /// Parse a non-capturing group with flags
    ///
    /// Non-capturing groups with flags also begin with `:?`, but the flag specification
    /// is between the `:` and `?`.
    ///
    /// Available flags are:
    ///
    /// * `i`: case-insensitive
    /// * `m`: multi-line
    /// * `s`: `.` matches newlines
    /// * `U`: swaps the meaning of `.*` and `.*?`
    /// * `u`: enable unicode support (default)
    /// * `x`: ignore whitespace and allow comments
    ///
    /// Flags are set or cleared like so:
    ///
    /// `ix-um`
    ///
    /// * Set the case-insensitive flag `i`
    /// * Set the ignore-whitespace flag `x`
    /// * Clear the unicode support flag `u`
    /// * Clear the multi-line flag `m`
    ///
    /// Together, the `?ix-um:` is a [`TokenKind::NonCapturingFlags`]
    pub fn parse_non_capturing_flags_group(&mut self) -> Result<Option<GroupKind>> {
        // TODO: remove this but preserve the documentation (parse_group_with_header)
        unimplemented!()
    }

    /// Parse a flag group
    ///
    /// A flag group is similar to a non-capturing group with flags, however instead of
    /// applying the effects of the flags to a sub-expression, they are applied to the
    /// current expression. This group type does not match a sub-expression.
    ///
    /// Flag groups are specified the same way as non-capturing groups with flags,
    /// but without the trailing `:` after the flags, and without the subexpression. E.g.:
    /// `(?ix)`.
    ///
    /// Available flags are:
    ///
    /// * `i`: case-insensitive
    /// * `m`: multi-line
    /// * `s`: `.` matches newlines
    /// * `U`: swaps the meaning of `.*` and `.*?`
    /// * `u`: enable unicode support (default)
    /// * `x`: ignore whitespace and allow comments
    ///
    /// Flags are set or cleared like so:
    ///
    /// `ix-um`
    ///
    /// * Set the case-insensitive flag `i`
    /// * Set the ignore-whitespace flag `x`
    /// * Clear the unicode support flag `u`
    /// * Clear the multi-line flag `m`
    ///
    /// Together, the `?ix-um` is a [`TokenKind::Flags`]
    pub fn parse_flag_group(&mut self) -> Result<Option<GroupKind>> {
        // TODO: remove this but preserve the documentation (parse_group_with_header)
        unimplemented!()
    }

    /// Parse a capturing group
    ///
    /// The matched contents of a capturing group can be referenced after the match. Named groups
    /// are reference by a string name, whereas basic capturing groups are identified by a
    /// sequenced number. The first capturing group is referenced by the number `1`. The number
    /// `0` references the entire match.
    ///
    /// Capturing groups are not prefixed with any tokens
    pub fn parse_capturing_group(&mut self) -> Result<Option<GroupKind>> {
        // The only part of a capturing group (besides the parentheses) is the sub-expression
        let expr = self.parse_expr()?;

        // assign an incrementing index to the group
        let index = self.group_index;
        self.group_index += 1;

        Ok(Some(GroupKind::Capturing(CapturingGroup {
            span: expr.span,
            index,
            expr: Box::new(expr),
        })))
    }

    /// Parse a character class
    ///
    /// Character classes match a single character from the input. The can be specified either
    /// as a unicode class (any character belonging to a certain unicode category, or having
    /// some other property) or as a specified class, where they can be matched with ranges and
    /// sets of characters, or by POSIX class.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// class ->
    ///     | unicode_class
    ///     | '[' specified_class ']'
    /// unicode_class ->
    ///     | unicode_escape CHAR
    ///     | unicode_escape '{' UNICODE_PROP_VALUE '}'
    ///     | unicode_escape '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
    ///     | unicode_escape '{' UNICODE_PROP_NAME '!=' UNICODE_PROP_VALUE '}'
    /// unicode_escape ->
    ///     | '\p'
    ///     | '\P'
    /// specified_class ->
    ///     | NEGATED? spec_item+
    /// spec_item ->
    ///     | spec_term spec_set?
    /// spec_term ->
    ///     | LITERAL ('-' LITERAL)?
    ///     | DIGIT_CLASS
    ///     | WHITESPACE_CLASS
    ///     | WORD_CLASS
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    /// spec_set ->
    ///     | '~~' spec_term spec_set?
    ///     | '--' spec_item spec_set?
    ///     | '&&' spec_item spec_set?
    ///     | \e
    /// ```
    pub fn parse_class(&mut self) -> Result<Option<Expr>> {
        self.parse_alts(vec![Self::parse_specified_class, Self::parse_unicode_class])
    }

    /// Parse a unicode character class
    ///
    /// Unicode classes match a character with a specified property value, such as the general
    /// category, script, script extension, block, etc.
    ///
    /// The short unicode class `\pL` uses the single-character variant of the general category,
    /// e.g. `L` for `Letter`. The longer version, `\p{Ll}` can use a longer specification,
    /// e.g. `Ll` for lowercase letters, `Letter` for letters, etc.
    ///
    /// Matching for a property other than general category uses the syntax `\p{scx=Greek}`,
    /// which matches a character in the greek script extension.
    ///
    /// The match can be negated using `\P` instead of `\p`, or in the property name syntax,
    /// using `\p{Script_Extension!=Greek}`. Double negations are treated as positive, i.e.
    /// `\P{sc!=Greek}` is equivalent to `\p{sc=Greek}`.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// unicode_class ->
    ///     | UNICODE_SHORT
    ///     | UNICODE_LONG '{' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '!=' UNICODE_PROP_VALUE '}'
    /// ```
    pub fn parse_unicode_class(&mut self) -> Result<Option<Expr>> {
        self.parse_alts(vec![
            Self::parse_unicode_short_class,
            Self::parse_unicode_long_class,
        ])
    }

    /// Parse a short-form unicode character class
    ///
    /// Unicode classes of the form `\pL` are condensed into a single token by the tokenizer.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// unicode_class ->
    ///     | UNICODE_SHORT
    /// ```
    pub fn parse_unicode_short_class(&mut self) -> Result<Option<Expr>> {
        let Some(tok) = self.input.match_where(TokenKind::is_unicode_short)? else {
            return Ok(None);
        };

        // create a unicode class for the general category specified by the token
        let TokenKind::UnicodeShort(category, negated) = tok.kind;
        let unicode_class = UnicodeClass {
            span: tok.span,
            name: None,
            value: StringSpan {
                span: tok.span,
                value: format!("{}", category),
            },
        };

        // construct the class expression with the negation value from the token
        Ok(Some(Expr {
            span: tok.span,
            kind: ExprKind::Class(Class {
                span: tok.span,
                negated,
                kind: ClassKind::Unicode(unicode_class),
            }),
        }))
    }

    /// Parse a long-form unicode character class
    ///
    /// Unicode classes of the form `\p{Property=Value}` are broken into start and end
    /// tokens, name, value, and equality tokens, with name and equality tokens being optional.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// unicode_class ->
    ///     | UNICODE_LONG '{' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '!=' UNICODE_PROP_VALUE '}'
    /// ```
    pub fn parse_unicode_long_class(&mut self) -> Result<Option<Expr>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_unicode_long_start)? else {
            return Ok(None);
        };

        let TokenKind::UnicodeLongStart(mut negated) = open_tok.kind;
        let mut name: Option<StringSpan> = None;
        let mut value: Option<StringSpan> = None;

        if let Some(val_tok) = self.input.match_where(TokenKind::is_unicode_prop_value)? {
            // for the form `\p{Value}`
            let TokenKind::UnicodePropValue(v) = val_tok.kind;
            value = Some(StringSpan { span: val_tok.span, value: v })
        } else if let Some(name_tok) = self.input.match_where(TokenKind::is_unicode_prop_name)? {
            // for the form `\p{Name=Value}`
            let TokenKind::UnicodePropName(n) = name_tok.kind;
            name = Some(StringSpan { span: name_tok.span, value: n });

            // the long version of a unicode class can be double negated, e.g.:
            //
            // \P{name!=value}
            //  ^     ^
            // here  here
            //
            // the resulting negation is the exclusive or (equivalent to NEQ) of both
            let eq_tok = self.expect_match("equal sign", TokenKind::is_unicode_equal)?;
            let TokenKind::UnicodeEqual(eq_negated) = eq_tok.kind;
            negated = negated != eq_negated;

            let val_tok = self.expect_match("unicode property value", TokenKind::is_unicode_prop_value)?;
            let TokenKind::UnicodePropValue(v) = val_tok.kind;
            value = Some(StringSpan { span: val_tok.span, value: v });
        } else {
            self.expected("unicode property name or value")?;
        }

        let close_tok = self.expect_match("unicode property closing brace `}`", TokenKind::is_unicode_long_end)?;

        // construct the unicode class with the name and value collected from
        // UnicodePropValue and (optionally) UnicodePropName tokens
        let span = Span::wrap(&open_tok.span, &close_tok.span);
        let unicode_class = UnicodeClass {
            span,
            name,
            value: value.expect("property value"),
        };

        // construct a class expression with the negation value from the UnicodeLongStart
        // and (optionally) UnicodeEqual tokens
        Ok(Some(Expr {
            span,
            kind: ExprKind::Class(Class {
                span,
                negated,
                kind: ClassKind::Unicode(unicode_class),
            }),
        }))
    }

    /// Parse a specified class
    ///
    /// A specified class represents a set of characters which can be matched from the input.
    /// They are specified using a combination of literals, ranges, set operations, sub-classes
    /// and POSIX classes:
    ///
    /// * literals: `[abc]` matches any character `a`, `b`, or `c`
    /// * ranges: `[a-c]` matches any character `a`, `b`, or `c`
    /// * set difference: `[[abcdef]--[def]]` matches any character `a`, `b`, or `c`
    /// * set intersection: `[[abcdef]&&[cdxyz]]` matches any character `c` or `d`
    /// * symmetrical set difference: `[[abc]~~[bcd]]` matches any character `a` or `d`
    /// * posix classes: `[[:alnum:]]` matches any alphanumeric character
    ///
    /// Classes can be negated using a `^` token at the beginning. POSIX classes can themselves
    /// be negated using the same token at the beginning of the name:
    ///
    /// * `[^abc]` matches any character except `a`, `b`, and `c`
    /// * `[[:^lower:]]` matches any character except lowercase letters
    ///
    /// POSIX classes are unaware of unicode properties, and so only apply to ASCII characters.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// specified_class ->
    ///     | NEGATED? spec_item+
    /// spec_item ->
    ///     | spec_term spec_set?
    /// spec_term ->
    ///     | LITERAL ('-' LITERAL)?
    ///     | DIGIT_CLASS
    ///     | WHITESPACE_CLASS
    ///     | WORD_CLASS
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    /// spec_set ->
    ///     | '~~' spec_term spec_set?
    ///     | '--' spec_item spec_set?
    ///     | '&&' spec_item spec_set?
    /// ```
    pub fn parse_specified_class(&mut self) -> Result<Option<Expr>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_open_bracket)? else {
            return Ok(None);
        };

        // optionally match a negation token for the class
        let negated = self.input.match_where(TokenKind::is_negated)?.is_some();

        // match all specification items
        let mut items = Vec::new();
        while let Some(item) = self.parse_specified_class_item()? {
            items.push(item);
        }

        let close_tok = self.expect_match("end of character class `]`", TokenKind::is_close_bracket)?;

        let span = Span::wrap(&open_tok.span, &close_tok.span);
        let inner_span_start = items
            .first()
            .map(|item| item.span.start)
            .unwrap_or(open_tok.span.end);
        let inner_span_end = items
            .last()
            .map(|item| item.span.end)
            .unwrap_or(close_tok.span.start);
        let inner_span = Span::new(inner_span_start, inner_span_end);

        let kind = ClassKind::Specified(SpecifiedClass {
            items,
            span: inner_span,
        });

        Ok(Some(Expr {
            span,
            kind: ExprKind::Class(Class {
                span,
                negated,
                kind,
            }),
        }))
    }

    /// Parse a specified class item
    ///
    /// Class items consist of literals, ranges, POSIX classes, subclasses, and set
    /// operations. See [`parse_specified_class`][1] for more details. To eliminate
    /// left-recursion from the grammar, subclasses and terminal productions are matched
    /// first before matching set operations.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// spec_item ->
    ///     | spec_term spec_set?
    /// spec_term ->
    ///     | LITERAL ('-' LITERAL)?
    ///     | DIGIT_CLASS
    ///     | WHITESPACE_CLASS
    ///     | WORD_CLASS
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    /// spec_set ->
    ///     | '~~' spec_term spec_set?
    ///     | '--' spec_item spec_set?
    ///     | '&&' spec_item spec_set?
    /// ```
    ///
    /// [1]: crate::parse::Parser::parse_specified_class
    pub fn parse_specified_class_item(&mut self) -> Result<Option<ClassSpec>> {
        let Some(term) = self.parse_specified_class_term()? else {
            return Ok(None)
        };

        let with_set = self.parse_class_item_set(term)?;
        Ok(Some(with_set))
    }

    /// Parse a specified class item, excluding set operations
    ///
    /// Any specification item that can be positively identified by a terminal can be
    /// parsed here, to ensure that the grammar is not left-recursive. Set operations
    /// can match further items to build a left-associated tree of set operations.
    ///
    /// See [parse_specified_class][1] for details on class items
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// spec_term ->
    ///     | LITERAL ('-' LITERAL)?
    ///     | DIGIT_CLASS
    ///     | WHITESPACE_CLASS
    ///     | WORD_CLASS
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    /// spec_set ->
    ///     | '~~' spec_term spec_set?
    ///     | '--' spec_item spec_set?
    ///     | '&&' spec_item spec_set?
    /// ```
    ///
    /// [1]: crate::parse::Parser::parse_specified_class
    pub fn parse_specified_class_term(&mut self) -> Result<Option<ClassSpec>> {
        self.parse_alts(vec![
            Self::parse_class_term_literal,
            Self::parse_class_term_digit,
            Self::parse_class_term_whitespace,
            Self::parse_class_term_word,
            Self::parse_class_term_bracket,
        ])
    }

    /// Parse a character class item beginning with a literal
    ///
    /// Either a literal or a literal range can be parsed here
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// spec_term ->
    ///     | LITERAL ('-' LITERAL)?
    /// ```
    pub fn parse_class_term_literal(&mut self) -> Result<Option<ClassSpec>> {
        // match a literal, which will either be on its own or at the start
        // of a literal range
        let Some(start_tok) = self.input.match_where(TokenKind::is_literal)? else {
            return Ok(None);
        };

        let TokenKind::Literal(c_start) = start_tok.kind;

        // optionally match the second half of a range
        let mut spec: Option<ClassSpec> = None;
        if self.input.match_where(TokenKind::is_range)?.is_some() {
            let end_tok = self.expect_match("end of range", TokenKind::is_literal)?;
            let TokenKind::Literal(c_end) = end_tok.kind;

            // if a range is matched, a range class specifier will be returned
            let span = Span::wrap(&start_tok.span, &end_tok.span);
            let kind = ClassSpecKind::Range(c_start, c_end);
            spec = Some(ClassSpec { span, kind })
        } else {
            // if a range wasn't matched, then the literal class specifier will be
            // returned on its own
            let span = start_tok.span;
            let kind = ClassSpecKind::Literal(c_start);
            spec = Some(ClassSpec { span, kind });
        }

        Ok(spec)
    }

    /// Parse a digit short class, `\d` or `\D`
    pub fn parse_class_term_digit(&mut self) -> Result<Option<ClassSpec>> {
        let Some(tok) = self.input.match_where(TokenKind::is_digit)? else {
            return Ok(None);
        };

        let TokenKind::Digit(negated) = tok.kind;
        Ok(Some(ClassSpec {
            span: tok.span,
            kind: ClassSpecKind::Digit(negated),
        }))
    }

    /// Parse a whitespace short class, `\s` or `\S`
    pub fn parse_class_term_whitespace(&mut self) -> Result<Option<ClassSpec>> {
        let Some(tok) = self.input.match_where(TokenKind::is_whitespace)? else {
            return Ok(None);
        };

        let TokenKind::Digit(negated) = tok.kind;
        Ok(Some(ClassSpec {
            span: tok.span,
            kind: ClassSpecKind::Whitespace(negated),
        }))
    }

    /// Parse a word char short class, `\w` or `\W`
    pub fn parse_class_term_word(&mut self) -> Result<Option<ClassSpec>> {
        let Some(tok) = self.input.match_where(TokenKind::is_word_char)? else {
            return Ok(None);
        };

        let TokenKind::WordChar(negated) = tok.kind;
        Ok(Some(ClassSpec {
            span: tok.span,
            kind: ClassSpecKind::WordChar(negated),
        }))
    }

    /// Parse a specified class item that begins with an opening bracket
    ///
    /// This includes parsing a nested class. [`parse_specified_class`][1] can't really be reused
    /// here because it needs to consume the first bracket - here the bracket is consumed so that
    /// the beginning of POSIX_NAME can be peeked to disambiguate the two productions; once the
    /// branch has been determined, control can't be passed to [`parse_specified_class`][1] because
    /// the bracket was already consumed.
    ///
    /// Inverting this dependency and making [`parse_specified_class`][1] assume the bracket was
    /// already consumed doesn't work that well because that token is needed to compute the
    /// ast node's span.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// spec_term ->
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    /// ```
    ///
    /// [1]: crate::parse::Parser::parse_specified_class
    pub fn parse_class_term_bracket(&mut self) -> Result<Option<ClassSpec>> {
        let Some(open_tok) = self.input.match_where(TokenKind::is_open_bracket)? else {
            return Ok(None);
        };

        let mut kind: Option<ClassSpecKind> = None;
        let mut span: Option<Span> = None;

        // attempt to match a POSIX class name
        if let Some(name_tok) = self.input.match_where(TokenKind::is_class_name)? {
            let close_tok = self.expect_match("end of posix class `]`", TokenKind::is_close_bracket)?;
            let TokenKind::ClassName(name, negated) = name_tok.kind;

            // convert the matched name into a POSIX class item
            let posix_kind = match PosixKind::try_from(name.as_ref()) {
                Ok(kind) => Ok(kind),
                Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
            }?;

            let kind_span = Span::wrap(&open_tok.span, &close_tok.span);
            span = Some(kind_span);
            kind = Some(ClassSpecKind::Posix(PosixClass {
                span: kind_span,
                kind: posix_kind,
                negated,
            }));
        } else {
            // if a POSIX class wasn't matched, then this must be the beginning of a subclass.
            let negated = self.input.match_where(TokenKind::is_negated)?.is_some();

            // match subclass items
            let mut items = Vec::new();
            while let Some(item) = self.parse_specified_class_item()? {
                items.push(item);
            }

            let close_tok = self.expect_match("end of character class `]`", TokenKind::is_close_bracket)?;

            let inner_span_start = items
                .first()
                .map(|item| item.span.start)
                .unwrap_or(open_tok.span.start);
            let inner_span_end = items
                .last()
                .map(|item| item.span.end)
                .unwrap_or(close_tok.span.end);
            let inner_span = Span::new(inner_span_start, inner_span_end);
            let kind_span = Span::wrap(&open_tok.span, &close_tok.span);
            let class_kind = ClassKind::Specified(SpecifiedClass {
                span: inner_span,
                items,
            });

            // create a subclass class specifier
            span = Some(kind_span);
            kind = Some(ClassSpecKind::Class(Class {
                span: kind_span,
                kind: class_kind,
                negated,
            }));
        }

        let kind = kind.expect("posix class or specified class");
        let span = span.expect("class term span");
        Ok(Some(ClassSpec { span, kind }))
    }

    /// Parse a set operation in a class specification
    ///
    /// Set operations include:
    ///
    /// * difference `A--B`: members of set A that are not in set B
    /// * symmetric difference `A~~B`: members that are not in both A and B
    /// * intersection: `A&&B`: members that are in both set A and set B
    ///
    /// This parser accepts a [`ClassSpec`][1] as the left hand side of the operation,
    /// and matches the right hand side itself. [`parse_specified_class_item`][2] parses
    /// the left-hand-side before optionally parsing a `spec_set` in order to eliminate
    /// left-recursion and create a left-associative structure.
    ///
    /// Multiple subsequent set operations can be matched, e.g.
    /// `[[:ascii:]--[:upper:]--[:lower:]]`
    ///
    /// # Arguments
    ///
    /// * `start` - the left-hand-side of the operation. The parser takes ownership of it,
    ///     but if no operator is matched, it will be returned again.
    ///
    /// # Grammar
    ///
    /// ```grammar
    /// spec_set ->
    ///     | '~~' spec_term spec_set?
    ///     | '--' spec_term spec_set?
    ///     | '&&' spec_term spec_set?
    /// ```
    ///
    /// [1]: crate::parse::ast::ClassSpec
    /// [2]: crate::parse::Parser::parse_specified_class_item
    pub fn parse_class_item_set(&mut self, start: ClassSpec) -> Result<ClassSpec> {
        // match a set operator to begin parsing the class spec. If none is found
        // then the left hand side will be returned unchanged.
        let set_kind = if let Some(tok) = self.input.match_where(TokenKind::is_set_operator)? {
            tok.kind
        } else {
            return Ok(start);
        };


        // parse the right-hand side of the operation
        let Some(end) = self.parse_specified_class_term()? else {
            return Err(self.input.error(ErrorKind::UnexpectedToken(set_kind, "end of set".to_string())));
        };

        // construct a `ClassSpecKind` depending on which operator was found
        let span = Span::wrap(&start.span, &end.span);
        let kind = match set_kind {
            TokenKind::Symmetrical => ClassSpecKind::Symmetrical(Symmetrical {
                span,
                left: Box::new(start),
                right: Box::new(end),
            }),
            TokenKind::Difference => ClassSpecKind::Difference(Difference {
                span,
                left: Box::new(start),
                right: Box::new(end),
            }),
            TokenKind::Intersection => ClassSpecKind::Intersection(Intersection {
                span,
                left: Box::new(start),
                right: Box::new(end),
            }),
            _ => unreachable!(),
        };

        let nested_set = self.parse_class_item_set(ClassSpec { span, kind })?;
        Ok(nested_set)
    }

    fn parse_alts<F, R>(&mut self, alts: Vec<F>) -> Result<Option<R>>
    where
        F: FnMut(&mut Self) -> Result<Option<R>>,
    {
        for alt in alts {
            match alt(self) {
                Ok(None) => continue,
                result => return result,
            }
        }

        Ok(None)
    }

    fn illegal_tok(&mut self, expect: &str) -> Error {
        match self.input.next() {
            Some(Ok(tok)) => self.input.error(
                ErrorKind::UnexpectedToken(
                    tok.kind,
                    expect.to_string()
                )),
            Some(Err(err)) => err,
            None => self.input.error(ErrorKind::UnexpectedEOF(
                expect.to_string()
            )),
        }
    }

    fn expect_match<F>(&mut self, expect: &str, f: F) -> Result<Token>
    where
        F: Fn(&TokenKind) -> bool
    {
        match self.input.next() {
            Some(Ok(tok)) if f(&tok.kind) => Ok(tok),
            Some(Ok(tok)) => Err(self.input.error(ErrorKind::UnexpectedToken(tok.kind, expect.to_string()))),
            Some(Err(err)) => Err(err),
            None => Err(self.input.error(ErrorKind::UnexpectedEOF(expect.to_string()))),
        }
    }

    fn expected(&mut self, expect: &str) -> Result<Token> {
        match self.input.next() {
            Some(Ok(tok)) => Err(self.input.error(ErrorKind::UnexpectedToken(tok.kind, expect.to_string()))),
            Some(Err(err)) => Err(err),
            None => Err(self.input.error(ErrorKind::UnexpectedEOF(expect.to_string()))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{span_token_iter, token_iter};

    fn unwrap_parse<T>(r: Result<Option<T>>) -> T {
        let Ok(Some(thing)) = r else {
            panic!("parse failed");
        };

        thing
    }

    fn get_alternation(r: Result<Expr>) -> Alternation {
        let Ok(expr) = r else {
            panic!("parse failed");
        };
        let ExprKind::Alternation(alternation) = expr.kind else {
            panic!("not an alternation");
        };

        alternation
    }

    fn get_concatenation(r: Result<Option<Expr>>) -> Concatenation {
        let expr = unwrap_parse(r);
        let ExprKind::Concatenation(concatenation) = expr.kind else {
            panic!("not a concatenation");
        };

        concatenation
    }

    fn get_repetition(r: Result<Option<Expr>>) -> Repetition {
        let expr = unwrap_parse(r);
        let ExprKind::Repetition(repetition) = expr.kind else {
            panic!("not a repetition");
        };

        repetition
    }

    fn get_range(r: Result<Option<RepetitionKind>>) -> Range {
        let kind = unwrap_parse(r);
        let RepetitionKind::Range(range) = kind else {
            panic!("not a range");
        };

        range
    }

    fn get_boundary(r: Result<Option<Expr>>) -> Boundary {
        let expr = unwrap_parse(r);
        let ExprKind::Boundary(boundary) = expr.kind else {
            panic!("expression is not a boundary");
        };

        boundary
    }

    fn get_group(r: Result<Option<Expr>>) -> Group {
        let expr = unwrap_parse(r);
        let ExprKind::Group(group) = expr.kind else {
            panic!("expression is not a group");
        };

        group
    }

    fn get_class(r: Result<Option<Expr>>) -> Class {
        let expr = unwrap_parse(r);
        let ExprKind::Class(class) = expr.kind else {
            panic!("expression is not a class");
        };

        class
    }

    fn get_class_spec(r: Result<Option<ClassSpec>>) -> ClassSpec {
        let Ok(r) = r else {
            panic!("parse failed");
        };

        let Some(spec) = r else {
            panic!("empty parse");
        };

        spec
    }

    fn get_unicode_class(class: Class) -> UnicodeClass {
        let ClassKind::Unicode(class) = class.kind else {
            panic!("class is not unicode class");
        };

        class
    }

    #[test]
    fn expr() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Alternate,
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::Alternate,
            TokenKind::OpenGroup,
            TokenKind::CloseGroup,
            TokenKind::Alternate,
        ]));

        let alt = get_alternation(p.parse_expr());
        assert_eq!(alt.items.len(), 4);
        assert!(matches!(alt.items[0].kind, ExprKind::Literal('a')));
        assert!(matches!(alt.items[1].kind, ExprKind::Concatenation(_)));
        assert!(matches!(alt.items[2].kind, ExprKind::Group(_)));
        assert!(matches!(alt.items[3].kind, ExprKind::Empty));

        let ExprKind::Concatenation(Concatenation { items, .. }) = &alt.items[1].kind else {
            panic!("not a concatenation");
        };
        assert_eq!(items.len(), 2);
        assert!(matches!(items[0].kind, ExprKind::Literal('b')));
        assert!(matches!(items[1].kind, ExprKind::Literal('c')));

        let ExprKind::Group(Group { kind: GroupKind::Capturing(group), .. }) = &alt.items[2].kind else {
            panic!("not a group");
        };
        assert!(matches!(group.expr.kind, ExprKind::Empty));
    }

    #[test]
    fn concatenation() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::OpenGroup,
            TokenKind::Literal('c'),
            TokenKind::CloseGroup,
        ]));

        let concat = get_concatenation(p.parse_concatenation());
        assert_eq!(concat.items.len(), 3);
        assert!(matches!(concat.items[0].kind, ExprKind::Literal('a')));
        assert!(matches!(concat.items[1].kind, ExprKind::Literal('b')));
        assert!(matches!(concat.items[2].kind, ExprKind::Group(_)));
        let ExprKind::Group(Group { kind: GroupKind::Capturing(group), .. }) = &concat.items[2].kind else {
            panic!("not a group");
        };

        assert!(matches!(group.expr.kind, ExprKind::Literal('c')));
    }

    #[test]
    fn repetition() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Dot,
            TokenKind::Question,
            TokenKind::Literal('a'),
            TokenKind::OpenBrace,
            TokenKind::Number(2),
            TokenKind::Comma,
            TokenKind::Number(3),
            TokenKind::CloseBrace,
            TokenKind::Literal('b'),
        ]));

        let rep = get_repetition(p.parse_repetition());
        assert!(matches!(rep.kind, RepetitionKind::ZeroOrOne));
        assert!(matches!(rep.item.kind, ExprKind::Any));

        let rep = get_repetition(p.parse_repetition());
        assert!(matches!(rep.kind, RepetitionKind::Range(_)));
        assert!(matches!(rep.item.kind, ExprKind::Literal('a')));

        let expr = unwrap_parse(p.parse_repetition());
        assert!(matches!(expr.kind, ExprKind::Literal('b')));

        // TODO: detect stray repetition tokens where they don't belong
    }

    #[test]
    fn repetition_spec() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Question,
            TokenKind::Star,
            TokenKind::Plus,
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
            TokenKind::Dot,
        ]));

        let rep = unwrap_parse(p.parse_repetition_spec());
        assert!(matches!(rep, RepetitionKind::ZeroOrOne));

        let rep = unwrap_parse(p.parse_repetition_spec());
        assert!(matches!(rep, RepetitionKind::ZeroOrMore));

        let rep = unwrap_parse(p.parse_repetition_spec());
        assert!(matches!(rep, RepetitionKind::OneOrMore));

        let rep = unwrap_parse(p.parse_repetition_spec());
        assert!(matches!(rep, RepetitionKind::Range(_)));

        let res = p.parse_repetition_spec();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn repetition_range() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Comma,
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Number(1),
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Number(2),
            TokenKind::Comma,
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Number(3),
            TokenKind::Comma,
            TokenKind::Number(4),
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Comma,
            TokenKind::Number(5),
            TokenKind::CloseBrace,
            TokenKind::OpenBrace,
            TokenKind::Literal('a'),
        ]));

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, None);
        assert_eq!(range.end, None);

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, None);
        assert_eq!(range.end, None);

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, Some(1));
        assert_eq!(range.end, None);

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, Some(2));
        assert_eq!(range.end, None);

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, Some(3));
        assert_eq!(range.end, Some(4));

        let range = get_range(p.parse_repetition_range());
        assert_eq!(range.start, None);
        assert_eq!(range.end, Some(5));

        let res = p.parse_repetition_range();
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::UnexpectedToken(TokenKind::Literal('a'), _),
                ..
            })
        ));
    }

    #[test]
    fn item() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Dot,
            TokenKind::Literal('a'),
            TokenKind::EndOfText,
            TokenKind::OpenGroup,
            TokenKind::Literal('b'),
            TokenKind::CloseGroup,
            TokenKind::Digit(true),
            TokenKind::Whitespace(false),
            TokenKind::WordChar(true),
            TokenKind::UnicodeShort('L', true),
            TokenKind::OpenBracket,
            TokenKind::Literal('c'),
            TokenKind::CloseBracket,
            TokenKind::Star,
        ]));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Any));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Literal('a')));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Boundary(_)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Group(_)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Digit(true)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Whitespace(false)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::WordChar(true)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Class(_)));

        let expr = unwrap_parse(p.parse_item());
        assert!(matches!(expr.kind, ExprKind::Class(_)));

        let res = p.parse_item();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn boundary() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::StartOfLine,
            TokenKind::EndOfLine,
            TokenKind::StartOfText,
            TokenKind::EndOfText,
            TokenKind::WordBoundary,
            TokenKind::NonWordBoundary,
            TokenKind::OpenGroup,
            TokenKind::CloseGroup,
        ]));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::StartOfLine));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::EndOfLine));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::StartOfText));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::EndOfText));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::WordBoundary));

        let boundary = get_boundary(p.parse_boundary());
        assert!(matches!(boundary.kind, BoundaryKind::NonWordBoundary));

        let res = p.parse_boundary();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn group() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroup,
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::Name("name".to_string()),
            TokenKind::Literal('b'),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::CloseGroupOptions,
            TokenKind::Literal('c'),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroupOptions,
            TokenKind::Literal('d'),
            TokenKind::CloseGroup,
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::CloseGroup,
            TokenKind::Literal('e'),
        ]));

        let group = get_group(p.parse_group());
        assert!(matches!(group.kind, GroupKind::Capturing(_)));

        let group = get_group(p.parse_group());
        assert!(matches!(group.kind, GroupKind::Named(_)));

        let group = get_group(p.parse_group());
        assert!(matches!(group.kind, GroupKind::NonCapturing(_)));
        let GroupKind::NonCapturing(NonCapturingGroup { flags, .. }) = group.kind else {
            panic!("not a non-capturing group");
        };
        assert!(flags.is_none());

        let group = get_group(p.parse_group());
        assert!(matches!(group.kind, GroupKind::NonCapturing(_)));
        let GroupKind::NonCapturing(NonCapturingGroup { flags, .. }) = group.kind else {
            panic!("not a non-capturing group");
        };
        assert!(flags.is_some());

        let group = get_group(p.parse_group());
        assert!(matches!(group.kind, GroupKind::Flags(_)));

        let res = p.parse_group();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn group_errors() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroup,
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::CloseGroupOptions,
            TokenKind::Literal('a'),
            TokenKind::CloseGroup,
        ]));

        let res = p.parse_group();
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::UnexpectedToken(TokenKind::Literal('a'), _),
                ..
            })
        ));

        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroup,
            TokenKind::Literal('a'),
        ]));

        let res = p.parse_group();
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::UnexpectedEOF(_),
                ..
            })
        ));

        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroup,
            TokenKind::CloseBracket,
        ]));

        let res = p.parse_group();
        assert!(matches!(
            res,
            Err(Error {
                kind: ErrorKind::UnexpectedToken(TokenKind::CloseBracket, _),
                ..
            })
        ));
    }

    #[test]
    fn non_capturing() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroupOptions,
            TokenKind::CloseGroupOptions,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
        ]));

        let Ok(Some(kind)) = p.parse_non_capturing_group() else {
            panic!("parse failed");
        };
        assert!(matches!(kind, GroupKind::NonCapturing(_)));

        let GroupKind::NonCapturing(NonCapturingGroup { flags, expr, .. }) = kind else {
            panic!("non a non-capturing group");
        };
        assert!(flags.is_none());
        assert!(matches!(expr.kind, ExprKind::Concatenation(_)));
    }

    #[test]
    fn non_capturing_flags() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenGroupOptions,
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
        ]));

        let Ok(Some(kind)) = p.parse_non_capturing_flags_group() else {
            panic!("parse failed");
        };
        assert!(matches!(kind, GroupKind::NonCapturing(_)));

        let GroupKind::NonCapturing(NonCapturingGroup { flags, expr, .. }) = kind else {
            panic!("non a non-capturing group");
        };
        assert!(flags.is_some());
        assert_eq!(
            flags.as_ref().unwrap().set_flags,
            vec![FlagKind::CaseInsensitive]
        );
        assert_eq!(
            flags.as_ref().unwrap().clear_flags,
            vec![FlagKind::IgnoreWhitespace]
        );
        assert!(matches!(expr.kind, ExprKind::Concatenation(_)));
    }

    #[test]
    fn named_group() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Name("name".to_string()),
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('b'),
        ]));

        let Ok(Some(kind)) = p.parse_named_group() else {
            panic!("parse failed");
        };
        assert!(matches!(kind, GroupKind::Named(_)));

        let GroupKind::Named(NamedGroup { name, expr, .. }) = kind else {
            panic!("not a named group");
        };
        assert_eq!(name.value, "name".to_string());
        assert!(matches!(expr.kind, ExprKind::Concatenation(_)));
    }

    #[test]
    fn flag_group() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Flag(Flag::CaseInsensitive),
            TokenKind::Flag(Flag::IgnoreWhitespace),
            TokenKind::FlagDelimiter,
            TokenKind::Flag(Flag::MultiLine),
            TokenKind::Flag(Flag::SwapGreed),
        ]));

        let Ok(Some(kind)) = p.parse_flag_group() else {
            panic!("parse failed");
        };
        assert!(matches!(kind, GroupKind::Flags(_)));

        let GroupKind::Flags(flags) = kind else {
            panic!("not a flag group");
        };
        assert_eq!(
            flags.flags.set_flags,
            vec![FlagKind::CaseInsensitive, FlagKind::IgnoreWhitespace],
        );
        assert_eq!(
            flags.flags.clear_flags,
            vec![FlagKind::MultiLine, FlagKind::SwapGreed],
        );
    }

    #[test]
    fn capturing_group() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
        ]));

        let Ok(Some(kind)) = p.parse_capturing_group() else {
            panic!("parse failed");
        };
        assert!(matches!(kind, GroupKind::Capturing(_)));
        let GroupKind::Capturing(group) = kind else {
            panic!("not a capturing group");
        };

        assert!(matches!(group.expr.kind, ExprKind::Concatenation(_)));

        // can't usefully test for a non-matching condition here, it's the base case
        // for groups which is expected always to match. If a group name or flag group
        // is found by this parser it just winds up with an Empty expression, since those
        // cases are handled above `parse_capturing_group`
    }

    #[test]
    fn flags() {
        let p = Parser::new(token_iter(Vec::new()));
        let flag_chars = vec!['i', 'm', 's', 'U', 'u', 'x'];
        let flags = p.parse_flags(flag_chars);

        assert!(flags.is_ok());
        let expected = vec![
            FlagKind::CaseInsensitive,
            FlagKind::MultiLine,
            FlagKind::DotMatchesNewline,
            FlagKind::SwapGreed,
            FlagKind::Unicode,
            FlagKind::IgnoreWhitespace,
        ];
        for (expected_flag, actual_flag) in expected.into_iter().zip(flags.unwrap()) {
            assert_eq!(expected_flag, actual_flag);
        }

        let invalid_flags = vec!['q'];
        let flags = p.parse_flags(invalid_flags);
        assert!(flags.is_err());
    }

    #[test]
    fn class() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::UnicodeShort('L', false),
            TokenKind::OpenBracket,
            TokenKind::Negated,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::CloseBracket,
            TokenKind::Plus,
        ]));

        let class = get_class(p.parse_class());
        assert!(matches!(
            class,
            Class {
                kind: ClassKind::Unicode(_),
                ..
            }
        ));

        let class = get_class(p.parse_class());
        assert!(matches!(
            class,
            Class {
                kind: ClassKind::Specified(_),
                ..
            }
        ));

        let res = p.parse_class();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn unicode_class() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::UnicodeShort('L', false),
            TokenKind::UnicodeLongStart(false),
            TokenKind::UnicodePropValue("Letter".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::Literal('\x5A'),
        ]));

        let class = get_unicode_class(get_class(p.parse_unicode_class()));
        assert_eq!(class.value.value, "L".to_string());
        assert!(class.name.is_none());

        let class = get_unicode_class(get_class(p.parse_unicode_class()));
        assert_eq!(class.value.value, "Letter".to_string());
        assert!(class.name.is_none());

        let res = p.parse_unicode_class();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn unicode_short_class() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::UnicodeShort('L', false),
            TokenKind::UnicodeShort('L', true),
            TokenKind::UnicodeLongStart(false),
            TokenKind::UnicodePropValue("Letter".to_string()),
            TokenKind::UnicodeLongEnd,
        ]));

        let class = get_class(p.parse_unicode_short_class());
        assert_eq!(class.negated, false);
        let u_class = get_unicode_class(class);
        assert_eq!(u_class.value.value, "L".to_string());

        let class = get_class(p.parse_unicode_short_class());
        assert_eq!(class.negated, true);
        let u_class = get_unicode_class(class);
        assert_eq!(u_class.value.value, "L".to_string());

        let res = p.parse_unicode_short_class();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn unicode_long_class() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::UnicodeLongStart(false),
            TokenKind::UnicodePropValue("Letter".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeLongStart(true),
            TokenKind::UnicodePropValue("Digit".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeLongStart(false),
            TokenKind::UnicodePropName("sc".to_string()),
            TokenKind::UnicodeEqual(true),
            TokenKind::UnicodePropValue("Greek".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeLongStart(true),
            TokenKind::UnicodePropName("sc".to_string()),
            TokenKind::UnicodeEqual(false),
            TokenKind::UnicodePropValue("Greek".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeLongStart(true),
            TokenKind::UnicodePropName("sc".to_string()),
            TokenKind::UnicodeEqual(true),
            TokenKind::UnicodePropValue("Greek".to_string()),
            TokenKind::UnicodeLongEnd,
            TokenKind::UnicodeShort('L', true),
        ]));

        let class = get_class(p.parse_unicode_long_class());
        assert_eq!(class.negated, false);
        let u_class = get_unicode_class(class);
        assert!(u_class.name.is_none());
        assert_eq!(u_class.value.value, "Letter".to_string());

        let class = get_class(p.parse_unicode_long_class());
        assert_eq!(class.negated, true);
        let u_class = get_unicode_class(class);
        assert!(u_class.name.is_none());
        assert_eq!(u_class.value.value, "Digit".to_string());

        let class = get_class(p.parse_unicode_long_class());
        assert_eq!(class.negated, true);
        let u_class = get_unicode_class(class);
        assert!(u_class.name.is_some());
        assert_eq!(u_class.name.unwrap().value, "sc".to_string());
        assert_eq!(u_class.value.value, "Greek".to_string());

        let class = get_class(p.parse_unicode_long_class());
        assert_eq!(class.negated, true);
        let u_class = get_unicode_class(class);
        assert!(u_class.name.is_some());
        assert_eq!(u_class.name.unwrap().value, "sc".to_string());
        assert_eq!(u_class.value.value, "Greek".to_string());

        let class = get_class(p.parse_unicode_long_class());
        assert_eq!(class.negated, false);
        let u_class = get_unicode_class(class);
        assert!(u_class.name.is_some());
        assert_eq!(u_class.name.unwrap().value, "sc".to_string());
        assert_eq!(u_class.value.value, "Greek".to_string());

        let res = p.parse_unicode_long_class();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn specified_class() {
        let mut p = Parser::new(span_token_iter(
            0,
            vec![
                (1, TokenKind::OpenBracket),
                (1, TokenKind::Literal('a')),
                (1, TokenKind::Literal('b')),
                (1, TokenKind::Literal('c')),
                (1, TokenKind::CloseBracket),
                (1, TokenKind::OpenBracket),
                (1, TokenKind::Negated),
                (1, TokenKind::Literal('a')),
                (1, TokenKind::Literal('b')),
                (1, TokenKind::Literal('c')),
                (1, TokenKind::CloseBracket),
            ],
        ));

        let class = get_class(p.parse_specified_class());
        assert_eq!(class.negated, false);
        assert_eq!(class.span.start, 0);
        assert_eq!(class.span.end, 5);
        assert!(matches!(class.kind, ClassKind::Specified(_)));
        if let ClassKind::Specified(SpecifiedClass { items, .. }) = class.kind {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].kind, ClassSpecKind::Literal('a')));
            assert!(matches!(items[1].kind, ClassSpecKind::Literal('b')));
            assert!(matches!(items[2].kind, ClassSpecKind::Literal('c')));
        }

        let class = get_class(p.parse_specified_class());
        assert_eq!(class.negated, true);
        assert_eq!(class.span.start, 5);
        assert_eq!(class.span.end, 11);
        assert!(matches!(class.kind, ClassKind::Specified(_)));
        if let ClassKind::Specified(SpecifiedClass { items, .. }) = class.kind {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].kind, ClassSpecKind::Literal('a')));
            assert!(matches!(items[1].kind, ClassSpecKind::Literal('b')));
            assert!(matches!(items[2].kind, ClassSpecKind::Literal('c')));
        }
    }

    #[test]
    fn spec_class_item() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::Intersection,
            TokenKind::Literal('\x63'),
            TokenKind::Symmetrical,
            TokenKind::Literal('Q'),
            TokenKind::CloseBracket,
        ]));
        let spec = get_class_spec(p.parse_specified_class_item());

        assert!(matches!(spec.kind, ClassSpecKind::Symmetrical(_)));
        let ClassSpecKind::Symmetrical(spec) = spec.kind else {
            panic!("symmetrical");
        };

        assert!(matches!((*spec.left).kind, ClassSpecKind::Intersection(_)));
        assert!(matches!((*spec.right).kind, ClassSpecKind::Literal('Q')));

        let ClassSpecKind::Intersection(spec) = (*spec.left).kind else {
            panic!("intersection");
        };

        assert!(matches!((*spec.left).kind, ClassSpecKind::Range('a', 'z')));
        assert!(matches!((*spec.right).kind, ClassSpecKind::Literal('\x63')));
    }

    #[test]
    fn class_term() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::Digit(true),
            TokenKind::Whitespace(false),
            TokenKind::WordChar(false),
            TokenKind::OpenBracket,
            TokenKind::ClassName("alpha".to_string(), false),
            TokenKind::CloseBracket,
            TokenKind::OpenBracket,
            TokenKind::Literal('a'),
            TokenKind::CloseBracket,
            TokenKind::CloseBracket,
        ]));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Literal('a')));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Range('b', 'z')));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Digit(true)));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Whitespace(false)));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::WordChar(false)));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(
            spec.kind,
            ClassSpecKind::Posix(PosixClass {
                kind: PosixKind::Alpha,
                ..
            })
        ));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(
            spec.kind,
            ClassSpecKind::Class(Class {
                kind: ClassKind::Specified(_),
                ..
            })
        ));
    }

    #[test]
    fn class_term_literal() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Range,
            TokenKind::Literal('c'),
            TokenKind::Literal('d'),
            TokenKind::Literal('-'),
            TokenKind::CloseBracket,
        ]));

        let spec = get_class_spec(p.parse_class_term_literal());
        assert!(matches!(spec.kind, ClassSpecKind::Literal('a')));

        let spec = get_class_spec(p.parse_class_term_literal());
        assert!(matches!(spec.kind, ClassSpecKind::Range('b', 'c')));

        let spec = get_class_spec(p.parse_class_term_literal());
        assert!(matches!(spec.kind, ClassSpecKind::Literal('d')));

        let spec = get_class_spec(p.parse_class_term_literal());
        assert!(matches!(spec.kind, ClassSpecKind::Literal('-')));
    }

    #[test]
    fn class_term_bracket() {
        let mut p = Parser::new(token_iter(vec![
            TokenKind::OpenBracket,
            TokenKind::ClassName("xdigit".to_string(), false),
            TokenKind::CloseBracket,
            TokenKind::OpenBracket,
            TokenKind::Literal('a'),
            TokenKind::Literal('b'),
            TokenKind::Literal('c'),
            TokenKind::CloseBracket,
            TokenKind::Literal('a'),
        ]));

        let spec = get_class_spec(p.parse_class_term_bracket());
        assert!(matches!(
            spec.kind,
            ClassSpecKind::Posix(PosixClass {
                kind: PosixKind::XDigit,
                ..
            })
        ));

        let spec = get_class_spec(p.parse_class_term_bracket());
        assert!(matches!(
            spec.kind,
            ClassSpecKind::Class(Class {
                kind: ClassKind::Specified(_),
                ..
            })
        ));

        let res = p.parse_class_term_bracket();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn class_item_set() {
        let lhs = ClassSpec {
            span: Span::new(2, 3),
            kind: ClassSpecKind::Literal('c'),
        };

        let mut p = Parser::new(token_iter(vec![
            TokenKind::Symmetrical,
            TokenKind::Literal('a'),
            TokenKind::Range,
            TokenKind::Literal('z'),
            TokenKind::Difference,
            TokenKind::Literal('z'),
        ]));

        let Ok(spec) = p.parse_class_item_set(lhs) else {
            panic!("parse failed");
        };
        let ClassSpecKind::Difference(set) = spec.kind else {
            panic!("difference set");
        };
        assert!(matches!((*set.left).kind, ClassSpecKind::Symmetrical(_)));
        assert!(matches!((*set.right).kind, ClassSpecKind::Literal('z')));

        let ClassSpecKind::Symmetrical(set) = (*set.left).kind else {
            panic!("symmetrical set");
        };
        assert!(matches!((*set.left).kind, ClassSpecKind::Literal('c')));
        assert!(matches!((*set.right).kind, ClassSpecKind::Range('a', 'z')));
    }
}
