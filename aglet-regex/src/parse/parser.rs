use std::convert::TryFrom;

use aglet_text::Span;

use crate::parse::ast::*;
use crate::parse::error::*;
use crate::parse::input::{expect_tok, match_one, match_tok, matches_one, matches_tok, Input};
use crate::tokenize::{self, Token, TokenKind};

pub struct Parser<'a> {
    input:       Input<'a>,
    group_index: usize,
}

/// Parse a regular expression from a token stream
///
/// Grammar:
///
/// ```grammar
/// expr ->
///     | alternation
///     | \e
/// alternation ->
///     | concatenation '|' alternation
///     | concatenation
/// concatenation ->
///     | repetition concatination
///     | repetition
/// repetition ->
///     | item repetition-spec
///     | item
/// repetition_spec ->
///     | repetition_range
///     | QUESTION
///     | STAR
///     | PLUS
/// repetition_range ->
///     | '{' repetition_range_contents '}'
/// repetition_range_contents ->
///     | NUMBER ',' NUMBER
///     | NUMBER ','
///     | ',' NUMBER
/// item ->
///     | DOT
///     | LITERAL
///     | BOUNDARY
///     | group
///     | class
/// group ->
///     | '(' group_contents ')'
/// group_contents ->
///     | NON_CAPTURING expr
///     | NON_CAPTURING_FLAGS expr
///     | GROUP_NAME expr
///     | FLAGS
///     | \e
/// class ->
///     | unicode_class
///     | '[' specified_class ']'
/// unicode_class ->
///     | UNICODE_SHORT
///     | UNICODE_LONG '{' UNICODE_PROP_VALUE '}'
///     | UNICODE_LONG '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
///     | UNICODE_LONG '{' UNICODE_PROP_NAME '!' '=' UNICODE_PROP_VALUE '}'
/// specified_class ->
///     | NEGATED class_spec
///     | class_spec
/// class_spec ->
///     | spec_item class_spec
///     | spec_item
/// spec_item ->
///     | spec_term spec_set
/// spec_term ->
///     | LITERAL
///     | LITERAL '-' LITERAL
///     | '[' POSIX_NAME ']'
///     | '[' specified_class ']'
/// spec_set ->
///     | '~~' spec_term spec_set
///     | '--' spec_item spec_set
///     | '&&' spec_item spec_set
///     | \e
/// ```
impl<'a> Parser<'a> {
    pub fn new<T>(input: T) -> Self
    where
        T: Iterator<Item = tokenize::Result<Token>> + 'a,
    {
        Parser {
            input:       Input::new(input),
            group_index: 1,
        }
    }

    pub fn next_group_index(&mut self) -> usize {
        let index = self.group_index;
        self.group_index += 1;
        index
    }

    pub fn parse(mut self) -> Result<Ast> {
        Ok(Ast {
            head: self.parse_expr()?,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        unimplemented!()
    }

    /// alternation ->
    ///     | concatenation '|' alternation
    ///     | concatenation
    fn parse_alternation(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// concatenation ->
    ///     | repetition concatination
    ///     | repetition
    fn parse_concatenation(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// repetition ->
    ///     | item repetition-spec
    ///     | item
    fn parse_repetition(&mut self) -> Result<Option<Expr>> {
        let Some(item) = self.parse_item()? else {
            return Ok(None);
        };

        if let Some(kind) = self.parse_repetition_spec()? {
            let span = Span::from(item.span.start, self.input.position());
            let repetition = Repetition {
                span,
                kind,
                item: Box::new(item),
            };

            Ok(Some(Expr {
                span,
                kind: ExprKind::Repetition(repetition),
            }))
        } else {
            Ok(Some(item))
        }
    }

    /// repetition_spec ->
    ///     | QUESTION
    ///     | STAR
    ///     | PLUS
    ///     | repetition_range
    /// repetition_range ->
    ///     | '{' repetition_range_contents '}'
    /// repetition_range_contents ->
    ///     | NUMBER ',' NUMBER
    ///     | NUMBER ','
    ///     | ',' NUMBER
    fn parse_repetition_spec(&mut self) -> Result<Option<RepetitionKind>> {
        let res = parse_alts![
            { self.parse_question() }
            { self.parse_star() }
            { self.parse_plus() }
            { self.parse_repetition_range() }
        ];

        Ok(res)
    }

    fn parse_question(&mut self) -> Result<Option<RepetitionKind>> {
        match_tok!(self.input; _, TokenKind::Question);

        Ok(Some(RepetitionKind::ZeroOrOne))
    }

    fn parse_star(&mut self) -> Result<Option<RepetitionKind>> {
        match_tok!(self.input; _, TokenKind::Star);

        Ok(Some(RepetitionKind::ZeroOrMore))
    }

    fn parse_plus(&mut self) -> Result<Option<RepetitionKind>> {
        match_tok!(self.input; _, TokenKind::Plus);

        Ok(Some(RepetitionKind::OneOrMore))
    }

    fn parse_repetition_range(&mut self) -> Result<Option<RepetitionKind>> {
        match_tok!(self.input; span_start, TokenKind::OpenBrace);

        let mut start: Option<usize> = None;
        let mut end: Option<usize> = None;

        matches_tok!(self.input; TokenKind::Number(number), |_| {
            start = Some(number);
            Ok(())
        });

        expect_tok!(self.input, "comma `,`"; _, TokenKind::Comma);

        matches_tok!(self.input; TokenKind::Number(number), |_| {
            end = Some(number);
            Ok(())
        });

        expect_tok!(self.input, "end of range `}`"; span_end, TokenKind::CloseBrace);

        let span = Span::wrap(span_start, span_end);
        Ok(Some(RepetitionKind::Range(Range { span, start, end })))
    }

    /// item ->
    ///     | DOT
    ///     | LITERAL
    ///     | BOUNDARY
    ///     | group
    ///     | class
    fn parse_item(&mut self) -> Result<Option<Expr>> {
        let res = parse_alts![
            { self.parse_dot() }
            { self.parse_literal() }
            { self.parse_boundary() }
            { self.parse_group() }
            { self.parse_class() }
        ];

        Ok(res)
    }

    fn parse_dot(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span, TokenKind::Dot);

        Ok(Some(Expr {
            span,
            kind: ExprKind::Any,
        }))
    }

    fn parse_literal(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span, TokenKind::Literal(c));

        Ok(Some(Expr {
            span,
            kind: ExprKind::Literal(c),
        }))
    }

    fn parse_boundary(&mut self) -> Result<Option<Expr>> {
        let tok = match_tok!(self.input; TokenKind::is_boundary);

        let span = tok.span;
        let kind = match BoundaryKind::try_from(tok.kind) {
            Ok(kind) => Ok(kind),
            Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
        }?;

        Ok(Some(Expr {
            span,
            kind: ExprKind::Boundary(Boundary { span, kind }),
        }))
    }

    /// group ->
    ///     | '(' group_contents ')'
    fn parse_group(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span_start, TokenKind::OpenGroup);
        let kind = self.parse_group_contents()?;
        expect_tok!(self.input, "end of group `)`"; span_end, TokenKind::CloseGroup);

        let span = Span::wrap(span_start, span_end);

        Ok(Some(Expr {
            span,
            kind: ExprKind::Group(Group { span, kind }),
        }))
    }

    /// group_contents ->
    ///     | NON_CAPTURING expr
    ///     | NON_CAPTURING_FLAGS expr
    ///     | GROUP_NAME expr
    ///     | FLAGS
    ///     | expr
    fn parse_group_contents(&mut self) -> Result<GroupKind> {
        let res = parse_alts![
            { self.parse_non_capturing_group() }
            { self.parse_non_capturing_flags_group() }
            { self.parse_named_group() }
            { self.parse_flags_group() }
            { self.parse_capturing_group() }
        ]
        .expect("group contents should not be None");

        Ok(res)
    }

    fn parse_non_capturing_group(&mut self) -> Result<Option<GroupKind>> {
        match_tok!(self.input; _, TokenKind::NonCapturing);
        let expr = self.parse_expr()?;

        Ok(Some(GroupKind::NonCapturing(NonCapturingGroup {
            flags: None,
            expr:  Box::new(expr),
        })))
    }

    fn parse_non_capturing_flags_group(&mut self) -> Result<Option<GroupKind>> {
        match_tok!(self.input; span, TokenKind::NonCapturingFlags(set_flags, clear_flags));
        let expr = self.parse_expr()?;

        let set_flags = self.parse_flags(set_flags)?;
        let clear_flags = self.parse_flags(clear_flags)?;

        Ok(Some(GroupKind::NonCapturing(NonCapturingGroup {
            flags: Some(Flags {
                span,
                set_flags,
                clear_flags,
            }),
            expr:  Box::new(expr),
        })))
    }

    fn parse_named_group(&mut self) -> Result<Option<GroupKind>> {
        match_tok!(self.input; span, TokenKind::Name(name));
        let expr = self.parse_expr()?;

        let name = StringSpan { span, value: name };

        Ok(Some(GroupKind::Named(NamedGroup {
            name,
            expr: Box::new(expr),
        })))
    }

    fn parse_flags_group(&mut self) -> Result<Option<GroupKind>> {
        match_tok!(self.input; span, TokenKind::Flags(set_flags, clear_flags));

        let set_flags = self.parse_flags(set_flags)?;
        let clear_flags = self.parse_flags(clear_flags)?;

        Ok(Some(GroupKind::Flags(FlagsGroup {
            flags: Flags {
                span,
                set_flags,
                clear_flags,
            },
        })))
    }

    fn parse_capturing_group(&mut self) -> Result<Option<GroupKind>> {
        let expr = self.parse_expr()?;

        Ok(Some(GroupKind::Capturing(CapturingGroup {
            index: self.next_group_index(),
            expr:  Box::new(expr),
        })))
    }

    fn parse_flags(&self, input_flags: Vec<char>) -> Result<Vec<FlagKind>> {
        let res = input_flags
            .into_iter()
            .map(FlagKind::try_from)
            .collect::<std::result::Result<Vec<_>, _>>();

        match res {
            Ok(flags) => Ok(flags),
            Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
        }
    }

    /// class ->
    ///     | unicode_class
    ///     | '[' specified_class ']'
    fn parse_class(&mut self) -> Result<Option<Expr>> {
        let res = parse_alts![
            { self.parse_specified_class() }
            { self.parse_unicode_class() }
        ];

        Ok(res)
    }

    /// unicode_class ->
    ///     | UNICODE_SHORT
    ///     | UNICODE_LONG '{' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
    ///     | UNICODE_LONG '{' UNICODE_PROP_NAME '!' '=' UNICODE_PROP_VALUE '}'
    fn parse_unicode_class(&mut self) -> Result<Option<Expr>> {
        let res = parse_alts![
            { self.parse_unicode_short_class() }
            { self.parse_unicode_long_class() }
        ];

        Ok(res)
    }

    fn parse_unicode_short_class(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span, TokenKind::UnicodeShort(category, negated));

        let unicode_class = UnicodeClass {
            span,
            name: None,
            value: StringSpan {
                span,
                value: format!("{}", category),
            },
        };

        Ok(Some(Expr {
            span,
            kind: ExprKind::Class(Class {
                span,
                negated,
                kind: ClassKind::Unicode(unicode_class),
            }),
        }))
    }

    fn parse_unicode_long_class(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span_begin, TokenKind::UnicodeLongStart(negated));

        let mut negated = negated;
        let mut name: Option<StringSpan> = None;
        let mut value: Option<StringSpan> = None;

        match_one!(self.input; [
            (TokenKind::UnicodePropValue(v), |span| {
                value = Some(StringSpan { span, value: v });

                Ok(())
            }),
            (TokenKind::UnicodePropName(n), |span| {
                name = Some(StringSpan { span, value: n });

                // the long version of a unicode class can be double negated, e.g.:
                //
                // \P{name!=value}
                //  ^     ^
                // here  here
                //
                // the resulting negation is the exclusive or (equivalent to NEQ) of both
                expect_tok!(self.input, "equal sign"; _, TokenKind::UnicodeEqual(negated_eq));
                negated = negated != negated_eq;

                expect_tok!(self.input, "unicode property value"; span, TokenKind::UnicodePropValue(v));
                value = Some(StringSpan { span, value: v });

                Ok(())
            }),
        ]);

        match_tok!(self.input; span_end, TokenKind::UnicodeLongEnd);

        let span = Span::wrap(span_begin, span_end);
        let unicode_class = UnicodeClass {
            span,
            name,
            value: value.expect("property value"),
        };

        Ok(Some(Expr {
            span,
            kind: ExprKind::Class(Class {
                span,
                negated,
                kind: ClassKind::Unicode(unicode_class),
            }),
        }))
    }

    /// class ->
    ///     | '[' specified_class ']'
    /// specified_class ->
    ///     | NEGATED class_spec
    ///     | class_spec
    /// class_spec ->
    ///     | spec_item class_spec
    ///     | spec_item
    fn parse_specified_class(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span_start, TokenKind::OpenBracket);

        let negated = matches_tok!(self.input; TokenKind::Negated);
        let mut items = Vec::new();
        while let Some(item) = self.parse_specified_class_item()? {
            items.push(item);
        }

        expect_tok!(self.input, "end of character class `]`"; span_end, TokenKind::CloseBracket);

        let span = Span::wrap(span_start, span_end);
        let kind = ClassKind::Specified(items);

        Ok(Some(Expr {
            span: span,
            kind: ExprKind::Class(Class {
                span,
                negated,
                kind,
            }),
        }))
    }

    /// spec_item ->
    ///     | spec_term spec_set
    fn parse_specified_class_item(&mut self) -> Result<Option<ClassSpec>> {
        let Some(term) = self.parse_specified_class_term()? else {
            return Ok(None)
        };

        let with_set = self.parse_class_item_set(term)?;
        Ok(Some(with_set))
    }

    /// spec_term ->
    ///     | LITERAL
    ///     | LITERAL '-' LITERAL
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    ///     | unicode_class
    fn parse_specified_class_term(&mut self) -> Result<Option<ClassSpec>> {
        let res = parse_alts![
            { self.parse_class_term_literal() }
            { self.parse_class_term_bracket() }
        ];

        Ok(res)
    }

    /// spec_term ->
    ///     | LITERAL '-' LITERAL
    ///     | LITERAL
    fn parse_class_term_literal(&mut self) -> Result<Option<ClassSpec>> {
        match_tok!(self.input; span_start, TokenKind::Literal(c_start));

        let mut spec: Option<ClassSpec> = None;
        let matched_range = matches_tok!(self.input; TokenKind::Range, |span_end| {
            expect_tok!(self.input, "end of range"; span_end, TokenKind::Literal(c_end));

            let span = Span::wrap(span_start, span_end);
            let kind = ClassSpecKind::Range(c_start, c_end);
            spec = Some(ClassSpec {
                span,
                kind,
            });

            Ok(())
        });

        if !matched_range {
            let span = span_start;
            let kind = ClassSpecKind::Literal(c_start);
            spec = Some(ClassSpec { span, kind });
        }

        Ok(spec)
    }

    /// Parse a specified class item that begins with an opening bracket
    ///
    /// This includes parsing a nested class. [`parse_specified_class`] can't really be reused
    /// here because it needs to consume the first bracket - here the bracket is consumed so that
    /// the beginning of POSIX_NAME can be peeked to disambiguate the two productions; once the
    /// branch has been determined, control can't be passed to [`parse_specified_class`] because
    /// the bracket was already consumed.
    ///
    /// Inverting this dependency and making [`parse_specified_class`] assume the bracket was
    /// already consumed doesn't work that well because that token is needed to compute the
    /// ast node's span.
    ///
    /// spec_term ->
    ///     | '[' POSIX_NAME ']'
    ///     | '[' specified_class ']'
    fn parse_class_term_bracket(&mut self) -> Result<Option<ClassSpec>> {
        match_tok!(self.input; span_start, TokenKind::OpenBracket);

        let mut kind: Option<ClassSpecKind> = None;
        let mut span: Option<Span> = None;
        let matched = matches_tok!(self.input; TokenKind::ClassName(name, negated), |_| {
            expect_tok!(self.input, "end of posix class `]`"; span_end, TokenKind::CloseBracket);

            let posix_kind = match PosixKind::try_from(name.as_ref()) {
                Ok(kind) => Ok(kind),
                Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
            }?;

            let kind_span = Span::wrap(span_start, span_end);
            span = Some(kind_span);
            kind = Some(ClassSpecKind::Posix(PosixClass { span: kind_span, kind: posix_kind }));

            Ok(())
        });

        if !matched {
            let negated = matches_tok!(self.input; TokenKind::Negated);
            let mut items = Vec::new();
            while let Some(item) = self.parse_specified_class_item()? {
                items.push(item);
            }

            expect_tok!(self.input, "end of character class `]`"; span_end, TokenKind::CloseBracket);

            let kind_span = Span::wrap(span_start, span_end);
            let class_kind = ClassKind::Specified(items);
            span = Some(kind_span);
            kind = Some(ClassSpecKind::Class(Class {
                span: kind_span,
                negated,
                kind: class_kind,
            }));
        }

        let kind = kind.expect("posix class or specified class");
        let span = span.expect("class term span");
        Ok(Some(ClassSpec { span, kind }))
    }

    /// spec_set ->
    ///     | '~~' spec_term spec_set
    ///     | '--' spec_term spec_set
    ///     | '&&' spec_term spec_set
    ///     | \e
    fn parse_class_item_set(&mut self, start: ClassSpec) -> Result<ClassSpec> {
        let mut set_kind: Option<TokenKind> = None;
        matches_one!(self.input; [
            (TokenKind::is_symmetrical, |_, kind| {
                set_kind = Some(kind);
                Ok(())
            }),
            (TokenKind::is_difference, |_, kind| {
                set_kind = Some(kind);
                Ok(())
            }),
            (TokenKind::is_intersection, |_, kind| {
                set_kind = Some(kind);
                Ok(())
            }),
        ]);

        let Some(set_kind) = set_kind else {
            return Ok(start);
        };

        let end = self.parse_specified_class_term()?;
        let Some(end) = end else {
            return Err(self.input.error(ErrorKind::UnexpectedToken(set_kind, "end of set".to_string())));
        };

        let span = Span::wrap(start.span, end.span);
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
}

macro_rules! parse_alts {
    ( $parser:block ) => {{
        (|| $parser)()?
    }};
    ( $parser:block $($parser_more:block)* ) => {{
        match (|| $parser)()? {
            res @ Some(_) => res,
            None => parse_alts![ $($parser_more)+ ]
        }
    }};
}

use parse_alts;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::Tokenizer;

    fn get_class(r: Result<Option<Expr>>) -> Class {
        let Ok(Some(expr)) = r else {
            panic!("parse failed");
        };
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
    fn parse_class() {
        let tr = Tokenizer::new(r"\pL[^abc]+");
        let mut p = Parser::new(tr);

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
        let tr = Tokenizer::new(r"\pL\p{Letter}\x5A");
        let mut p = Parser::new(tr);

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
        let tr = Tokenizer::new(r"\pL\PL\p{Letter}");
        let mut p = Parser::new(tr);

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
        let tr = Tokenizer::new(r"\p{Letter}\P{Digit}\p{sc!=Greek}\P{sc=Greek}\PL");
        let mut p = Parser::new(tr);

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

        let res = p.parse_unicode_long_class();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn specified_class() {
        let tr = Tokenizer::new(r"[abc][^abc]");
        let mut p = Parser::new(tr);

        let class = get_class(p.parse_specified_class());
        assert_eq!(class.negated, false);
        assert_eq!(class.span.start.column, 1);
        assert_eq!(class.span.start.offset, 0);
        assert_eq!(class.span.end.column, 6);
        assert_eq!(class.span.end.offset, 5);
        assert!(matches!(class.kind, ClassKind::Specified(_)));
        if let ClassKind::Specified(items) = class.kind {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].kind, ClassSpecKind::Literal('a')));
            assert_eq!(items[0].span.start.column, 2);
            assert_eq!(items[0].span.end.column, 3);
            assert!(matches!(items[1].kind, ClassSpecKind::Literal('b')));
            assert_eq!(items[1].span.start.column, 3);
            assert_eq!(items[1].span.end.column, 4);
            assert!(matches!(items[2].kind, ClassSpecKind::Literal('c')));
            assert_eq!(items[2].span.start.column, 4);
            assert_eq!(items[2].span.end.column, 5);
        }

        let class = get_class(p.parse_specified_class());
        assert_eq!(class.negated, true);
        assert_eq!(class.span.start.column, 6);
        assert_eq!(class.span.start.offset, 5);
        assert_eq!(class.span.end.column, 12);
        assert_eq!(class.span.end.offset, 11);
        assert!(matches!(class.kind, ClassKind::Specified(_)));
        if let ClassKind::Specified(items) = class.kind {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].kind, ClassSpecKind::Literal('a')));
            assert_eq!(items[0].span.start.column, 8);
            assert_eq!(items[0].span.end.column, 9);
            assert!(matches!(items[1].kind, ClassSpecKind::Literal('b')));
            assert_eq!(items[1].span.start.column, 9);
            assert_eq!(items[1].span.end.column, 10);
            assert!(matches!(items[2].kind, ClassSpecKind::Literal('c')));
            assert_eq!(items[2].span.start.column, 10);
            assert_eq!(items[2].span.end.column, 11);
        }
    }

    #[test]
    fn spec_class_item() {
        let tr = Tokenizer::new(r"[a-z&&\x63~~Q]");
        let mut p = Parser::new(tr.skip(1));
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
        let tr = Tokenizer::new(r"[ab-z[:alpha:][abc]\x63-\x6A]");
        let mut p = Parser::new(tr.skip(1));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Literal('a')));

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Range('b', 'z')));

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

        let spec = get_class_spec(p.parse_specified_class_term());
        assert!(matches!(spec.kind, ClassSpecKind::Range('\x63', '\x6A')));
    }

    #[test]
    fn class_term_literal() {
        let tr = Tokenizer::new(r"[ab-cd-]");
        let mut p = Parser::new(tr.skip(1));

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
        let tr = Tokenizer::new(r"[[:xdigit:][abc]a]");
        let mut p = Parser::new(tr.skip(1));

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
            span: Span::new(),
            kind: ClassSpecKind::Literal('c'),
        };

        let tr = Tokenizer::new(r"[a~~a-z--z]");
        let mut p = Parser::new(tr.skip(2));

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
