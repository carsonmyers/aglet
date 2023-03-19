use std::convert::TryFrom;
use std::iter::Peekable;

use aglet_text::Span;

use crate::parse::ast::*;
use crate::parse::error::*;
use crate::parse::input::{expect_tok, if_match_tok, match_one, match_tok, Input};
use crate::tokenize::{self, Token, TokenKind, Tokenizer};

pub struct Parser<'a> {
    input: Input<'a>,
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
///     | specified_class
///     | unicode_class
///     | posix_class
/// specified_class ->
///     | '[' spec_contents ']'
/// spec_contents ->
///     | NEGATED class_spec
///     | class_spec
///     | \e
/// class_spec ->
///     | spec_item class_spec
///     | spec_item
/// spec_item ->
///     | LITERAL
///     | class_range_symmetrical
///     | class_range_difference
///     | class_range_intersection
///     | class_range
///     | class
/// class_range_symmetrical ->
///     | spec_item '~~' spec_item
/// class_range_difference ->
///     | spec_item '--' spec_item
/// class_range_intersection
///     | spec_item '&&' spec_item
/// class_range ->
///     | LITERAL '-' LITERAL
/// unicode_class ->
///     | UNICODE_SHORT
///     | '{' UNICODE_PROP_VALUE '}'
///     | '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
///     | '{' UNICODE_PROP_NAME '!' '=' UNICODE_PROP_VALUE '}'
/// posix_class ->
///     | '[' POSIX_NAME ']'
/// ```
impl<'a> Parser<'a> {
    pub fn new<T>(input: T) -> Self
    where
        T: Iterator<Item = tokenize::Result<Token>> + 'a,
    {
        Parser {
            input: Input::new(input),
        }
    }

    pub fn parse(mut self) -> Result<Ast> {
        Ok(Ast {
            head: self.parse_expr()?,
        })
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>> {
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
        unimplemented!()
    }

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
    fn parse_repetition_spec(&mut self) -> Result<Option<RepetitionKind>> {
        unimplemented!()
    }

    /// item ->
    ///     | DOT
    ///     | LITERAL
    ///     | BOUNDARY
    ///     | group
    ///     | class
    fn parse_item(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// group ->
    ///     | '(' group_contents ')'
    fn parse_group(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// group_contents ->
    ///     | NON_CAPTURING expr
    ///     | NON_CAPTURING_FLAGS expr
    ///     | GROUP_NAME expr
    ///     | FLAGS
    ///     | \e
    fn parse_group_contents(&mut self) -> Result<Option<GroupKind>> {
        unimplemented!()
    }

    /// class ->
    ///     | specified_class
    ///     | unicode_class
    ///     | posix_class
    fn parse_class(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// specified_class ->
    ///     | '[' spec_contents ']'
    /// spec_contents ->
    ///     | NEGATED class_spec
    ///     | class_spec
    ///     | \e
    /// class_spec ->
    ///     | spec_item class_spec
    ///     | spec_item
    /// spec_item ->
    ///     | LITERAL
    ///     | class_range_symmetrical
    ///     | class_range_difference
    ///     | class_range_intersection
    ///     | class_range
    ///     | class
    fn parse_specified_class(&mut self) -> Result<Option<Expr>> {
        unimplemented!()
    }

    /// class_range_symmetrical ->
    ///     | LITERAL '~~' LITERAL
    /// class_range_difference ->
    ///     | LITERAL '--' LITERAL
    /// class_range_intersection
    ///     | LITERAL '&&' LITERAL
    /// class_range ->
    ///     | LITERAL '-' LITERAL
    fn parse_class_range(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span_start, TokenKind::Literal(c_start));

        let mut range_tok: Option<TokenKind> = None;
        match_one!(self.input; [
            (TokenKind::is_range_symmetrical, |_, kind| {
                range_tok = Some(kind);
                Ok(())
            }),
            (TokenKind::is_range_difference, |_, kind| {
                range_tok = Some(kind);
                Ok(())
            }),
            (TokenKind::is_range_intersection, |_, kind| {
                range_tok = Some(kind);
                Ok(())
            }),
            (TokenKind::is_range, |_, kind| {
                range_tok = Some(kind);
                Ok(())
            }),
        ]);

        expect_tok!(self.input, "end literal"; span_end, TokenKind::Literal(c_end));

        unimplemented!()
    }

    /// unicode_class ->
    ///     | UNICODE_SHORT
    ///     | '{' UNICODE_PROP_VALUE '}'
    ///     | '{' UNICODE_PROP_NAME '=' UNICODE_PROP_VALUE '}'
    ///     | '{' UNICODE_PROP_NAME '!' '=' UNICODE_PROP_VALUE '}'
    fn parse_unicode_class(&mut self) -> Result<Option<Expr>> {
        let res = parse_alts![
            self.parse_unicode_short_class();
            self.parse_unicode_long_class();
        ];

        Ok(res)
    }

    fn parse_unicode_short_class(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; span, TokenKind::UnicodeShort(category, negated));

        let class = UnicodeClass {
            span,
            name: None,
            value: StringSpan {
                span,
                value: format!("{}", category),
            },
        };

        self.input.class_res(negated, ClassKind::Unicode(class))
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

        let class = UnicodeClass {
            span:  Span::wrap(span_begin, span_end),
            name:  name,
            value: value.expect("property value"),
        };

        self.input.class_res(negated, ClassKind::Unicode(class))
    }

    /// posix_class ->
    ///     | '[' POSIX_NAME ']'
    fn parse_posix_class(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input; _, TokenKind::OpenBracket);
        expect_tok!(self.input, "posix class name"; span, TokenKind::ClassName(name, negated));
        expect_tok!(self.input, "end of class `]`"; _, TokenKind::CloseBracket);

        let kind = match PosixKind::try_from(name.as_ref()) {
            Ok(kind) => Ok(kind),
            Err(err) => Err(self.input.error(ErrorKind::TokenConvertError(err))),
        }?;

        let class = self
            .input
            .class(negated, ClassKind::Posix(PosixClass { span, kind }));

        Ok(Some(class))
    }

    /*
    fn parse_item(&mut self) -> Result<Option<Expr>> {
        let res = parse_alts![
            self.parse_literal();
            self.parse_boundary();
        ];

        Ok(res)
    }

    fn parse_literal(&mut self) -> Result<Option<Expr>> {
        match_tok!(self.input, TokenKind::Literal(c));

        self.input.expr_res(ExprKind::Literal(c))
    }

    fn parse_boundary(&mut self) -> Result<Option<Expr>> {
        let tok = match_tok!(self.input, |kind: &TokenKind| kind.is_boundary());
        match BoundaryKind::try_from(tok.kind) {
            Ok(kind) => self.input.boundary_res(kind),
            Err(err) => self.input.error_res(ErrorKind::TokenConvertError(err)),
        }
    }
    */
}

macro_rules! parse_alts {
    ( $p:expr ; ) => {{
        $p?
    }};
    ( $p:expr; $($ps:expr);* ; ) => {{
        match parse_alts![ $p; ] {
            res @ Some(_) => res,
            None => parse_alts![ $($ps;)+ ],
        }
    }};
}

use parse_alts;

#[cfg(test)]
mod tests {
    use super::*;

    fn get_class(r: Result<Option<Expr>>) -> Class {
        let Ok(Some(expr)) = r else {
            panic!("parse failed");
        };
        let ExprKind::Class(class) = expr.kind else {
            panic!("expression is not a class");
        };

        class
    }

    fn get_unicode_class(class: Class) -> UnicodeClass {
        let ClassKind::Unicode(class) = class.kind else {
            panic!("class is not unicode class");
        };

        class
    }

    #[test]
    fn unicode_short_class() {
        let mut tr = Tokenizer::new(r"\pL\PLl");
        tr.set_debug();

        let mut p = Parser::new(tr);

        let class = get_class(p.parse_unicode_short_class());
        assert_eq!(class.negated, false);
        let u_class = get_unicode_class(class);
        assert_eq!(u_class.value.value, "L".to_string());

        let class = get_class(p.parse_unicode_short_class());
        assert_eq!(class.negated, true);
        let u_class = get_unicode_class(class);
        assert_eq!(u_class.value.value, "L".to_string())
    }

    #[test]
    fn unicode_long_class() {
        let mut tr = Tokenizer::new(r"\p{Letter}\P{Digit}\p{sc!=Greek}\P{sc=Greek}");
        tr.set_debug();

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
    }

    #[test]
    fn posix_class() {
        let mut tr = Tokenizer::new(r"[[:alpha:]");
        tr.set_debug();

        // skip the first '[' as it's not part of the POSIX class
        let mut p = Parser::new(tr.skip(1));

        let Ok(Some(expr)) = p.parse_posix_class() else {
            panic!("parse failed");
        };
        let ExprKind::Class(class) = expr.kind else {
            panic!("expression is not a class");
        };

        assert!(matches!(
            class.kind,
            ClassKind::Posix(PosixClass {
                kind: PosixKind::Alpha,
                ..
            })
        ));
    }
}
