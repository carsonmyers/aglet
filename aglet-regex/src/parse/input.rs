use std::iter::Peekable;

use aglet_text::Span;

use crate::parse::ast::*;
use crate::parse::error::*;
use crate::tokenize::{self, Token, TokenKind};

pub(crate) struct Input<'a> {
    data: Peekable<Box<dyn Iterator<Item = tokenize::Result<Token>> + 'a>>,
    span: Span,
}

impl<'a> Input<'a> {
    pub fn new<T>(data: T) -> Self
    where
        T: Iterator<Item = tokenize::Result<Token>> + 'a,
    {
        let data = Box::new(data) as Box<dyn Iterator<Item = tokenize::Result<Token>>>;
        Self {
            data: data.peekable(),
            span: Span::new(),
        }
    }

    pub fn peek(&mut self) -> Option<Result<&Token>> {
        match self.data.peek() {
            None => None,
            Some(Ok(tok)) => Some(Ok(tok)),
            Some(Err(err)) => Some(Err(Error::from(err.clone()))),
        }
    }

    pub fn next(&mut self) -> Option<Result<Token>> {
        let item = self
            .data
            .next()
            .map(|res| res.map_err(|err| Error::from(err)));

        if let Some(Ok(tok)) = &item {
            self.span = tok.span;
        }

        item
    }

    pub fn error(&self, kind: ErrorKind) -> Error {
        Error {
            span: self.span,
            kind,
        }
    }

    pub fn error_res<T>(&self, kind: ErrorKind) -> Result<Option<T>> {
        Err(self.error(kind))
    }

    pub fn expr(&self, kind: ExprKind) -> Expr {
        Expr {
            span: self.span,
            kind,
        }
    }

    pub fn expr_res(&self, kind: ExprKind) -> Result<Option<Expr>> {
        Ok(Some(self.expr(kind)))
    }

    pub fn boundary(&self, kind: BoundaryKind) -> Expr {
        self.expr(ExprKind::Boundary(Boundary {
            span: self.span,
            kind,
        }))
    }

    pub fn boundary_res(&self, kind: BoundaryKind) -> Result<Option<Expr>> {
        Ok(Some(self.boundary(kind)))
    }

    pub fn class(&self, negated: bool, kind: ClassKind) -> Expr {
        self.expr(ExprKind::Class(Class {
            span: self.span,
            negated,
            kind,
        }))
    }

    pub fn class_res(&self, negated: bool, kind: ClassKind) -> Result<Option<Expr>> {
        Ok(Some(self.class(negated, kind)))
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! expect_tok {
    ($input:expr, $expect:literal; $span:pat, $kind:pat) => {
        let Some(res) = $input.next() else {
            let expected = $expect.into();
            return Err($input.error(ErrorKind::UnexpectedEOF(expected)));
        };

        let tok = res?;
        let $kind = tok.kind else {
            let expected = $expect.into();
            return Err($input.error(ErrorKind::UnexpectedToken(tok.kind, expected)));
        };

        let $span = tok.span;
    };
    ($input:expr, $expect:literal; $guard:expr) => {{
        let Some(res) = $input.next() else {
            let expected = $expect.into();
            return Err($input.error(ErrorKind::UnexpectedEOF(expected)));
        };

        let tok = res?;
        if !$guard(&tok.kind) {
            let expected = $expect.into();
            return Err($input.error(ErrorKind::UnexpectedToken(tok.kind, expected)));
        }

        tok
    }}
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! match_tok {
    ($input:expr; $span:pat, $kind:pat) => {
        let Some(res) = $input.peek() else {
            return Ok(None)
        };

        #[allow(unused_variables)]
        if !matches!(&res?.kind, $kind) {
            return Ok(None)
        };

        let tok = $input.next().expect("next token").expect("successful token");

        let $kind = tok.kind else {
            panic!("token should still match");
        };

        let $span = tok.span;
    };
    ($input:expr; $guard:expr) => {{
        let Some(res) = $input.peek() else {
            return Ok(None);
        };

        if !$guard(&res?.kind) {
            return Ok(None);
        }

        $input.next().expect("next token").expect("successful token")
    }}
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! if_match_tok {
    ($input:expr; $kind:pat, |$span:pat| $block:block) => {{
        let mut matched = false;
        if let Some(res) = $input.peek() {
            #[allow(unused_variables)]
            if matches!(&res?.kind, $kind) {
                let tok = $input.next().expect("next token").expect("successful token");
                let $kind = tok.kind else {
                    panic!("token should still match");
                };
                (|$span: Span| -> std::result::Result<(), Error> {
                    $block
                })(tok.span)?;
                matched = true;
            }
        }

        matched
    }};
    ($input:expr; $guard:expr, |$span:pat, $kind:pat| $block:block) => {{
        let mut matched = false;
        if let Some(res) = $input.peek() {
            if $guard(&res?.kind) {
                let tok = $input.next().expect("next token").expect("successful token");
                (|$span, $kind| -> std::result::Result<(), Error> {$block})(tok.span, tok.kind)?;
                matched = true;
            }
        }

        matched
    }};
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! match_one {
    ($input:expr; [ ($kind:pat, |$span: pat| $block:block) $(,)* ]) => {{
        if !if_match_tok!($input; $kind, |$span| $block) {
            return Ok(None);
        }

        true
    }};
    ($input: expr; [ ($guard:expr, |$span:pat, $kind:pat| $block:block) $(,)* ]) => {{
        if !if_match_tok!($input; $guard, |$span, $kind| $block) {
            return Ok(None);
        }

        true
    }};
    ($input:expr; [
        ($kind:pat, |$span:pat| $block:block),
        $(($kind_more:pat, |$span_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if if_match_tok!($input; $kind, |$span| $block) {
            true
        } else {
            match_one!($input; [ $(($kind_more, |$span_more| $block_more),)+ ])
        }
    }};
    ($input: expr; [
        ($guard:expr, |$span:pat, $kind:pat| $block:block),
        $(($guard_more:expr, |$span_more:pat, $kind_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if if_match_tok!($input; $guard, |$span, $kind| $block) {
            true
        } else {
            match_one!($input; [ $(($guard_more, |$span_more, $kind_more| $block_more),)+ ])
        }
    }};
}

pub(crate) use expect_tok;
use if_match_tok;
pub(crate) use match_one;
pub(crate) use match_tok;
