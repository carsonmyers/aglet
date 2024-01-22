use std::collections::HashSet;

use aglet_text::Span;

use crate::lowering::error::*;
use crate::lowering::ir::*;
use crate::parse::ast;

pub struct Lower {
    flag_stack: Vec<HashSet<Flag>>,
    errors:     Vec<Error>,
}

impl Lower {
    pub fn lower(input: ast::ParseResult) -> LowerResult {
        let mut state = Lower {
            flag_stack: vec![HashSet::new()],
            errors:     Vec::new(),
        };

        let res = state.lower_expr(input.ast);

        let mut errors = state.errors;
        let ir = match res {
            Ok(expr) => expr,
            Err(err) => {
                errors.push(err);
                Default::default()
            },
        };

        errors.extend(input.errors.into_iter().map(|err| err.into()));
        errors.reverse();

        LowerResult { ir, errors }
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> Result<Expr> {
        let kind = match expr.kind {
            ast::ExprKind::Alternation(alternation) => {
                ExprKind::Alternation(self.lower_alternation(alternation))
            },
            ast::ExprKind::Concatenation(concatenation) => {
                ExprKind::Concatenation(self.lower_concatenation(concatenation))
            },
            ast::ExprKind::Repetition(repetition) => {
                ExprKind::Repetition(self.lower_repetition(repetition))
            },
            ast::ExprKind::Any => ExprKind::Class(self.lower_any(expr.span)),
            ast::ExprKind::Literal(char) => ExprKind::Class(self.lower_literal(char, expr.span)),
            ast::ExprKind::Digit(negated) => ExprKind::Class(self.lower_digit(negated)?),
            ast::ExprKind::Whitespace(negated) => ExprKind::Class(self.lower_whitespace(negated)?),
            ast::ExprKind::WordChar(negated) => ExprKind::Class(self.lower_word_char(negated)?),
            ast::ExprKind::Boundary(_boundary) => todo!(),
            ast::ExprKind::Group(group) => ExprKind::Group(self.lower_group(group)?),
            ast::ExprKind::Class(class) => ExprKind::Class(self.lower_class(class)?),
            ast::ExprKind::Empty => ExprKind::Empty,
        };

        let span = expr.span;
        let flags = self.flag_stack.last().cloned().unwrap_or_default();

        Ok(Expr { span, flags, kind })
    }

    fn lower_alternation(&mut self, alternation: ast::Alternation) -> Alternation {
        Alternation {
            span:  alternation.span,
            items: self.lower_items(alternation.items),
        }
    }

    fn lower_concatenation(&mut self, concatenation: ast::Concatenation) -> Concatenation {
        Concatenation {
            span:  concatenation.span,
            items: self.lower_items(concatenation.items),
        }
    }

    fn lower_repetition(&mut self, repetition: ast::Repetition) -> Repetition {
        let res = self.lower_expr(*repetition.item);
        if let Err(err) = &res {
            self.errors.push(err.clone());
        }

        let span = repetition.span;
        let greedy = repetition.greedy;
        let item = Box::new(res.unwrap_or_default());
        let (lower, upper) = match repetition.kind {
            ast::RepetitionKind::ZeroOrOne => (0, Some(1)),
            ast::RepetitionKind::ZeroOrMore => (0, None),
            ast::RepetitionKind::OneOrMore => (1, None),
            ast::RepetitionKind::Range(range) => match range {
                ast::Range {
                    start: Some(start),
                    end,
                    ..
                } => (start, end),
                ast::Range { end, .. } => (0, end),
            },
        };

        Repetition {
            span,
            lower,
            upper,
            greedy,
            item,
        }
    }

    fn lower_items(&mut self, items: Vec<ast::Expr>) -> Vec<Expr> {
        let mut lowered_items = Vec::new();
        for item in items {
            match self.lower_expr(item) {
                Ok(lowered_item) => lowered_items.push(lowered_item),
                Err(err) => self.errors.push(err),
            }
        }

        lowered_items
    }

    fn lower_any(&mut self, span: Span) -> Class {
        #[allow(clippy::single_range_in_vec_init)]
        let ranges = if !self.flag_is_set(&Flag::DotMatchesNewline) {
            vec![
                '\0'..'\n',
                char::from_u32('\n' as u32 + 1).unwrap()..char::MAX,
            ]
        } else {
            vec!['\0'..char::MAX]
        };

        let ranges = ranges
            .into_iter()
            .map(|range| ClassRange { span, range })
            .collect();

        Class { span, ranges }
    }

    fn lower_literal(&mut self, literal: char, span: Span) -> Class {
        Class {
            span,
            ranges: vec![ClassRange {
                span,
                range: literal..literal,
            }],
        }
    }

    fn lower_digit(&mut self, negated: bool) -> Result<Class> {
        todo!();
    }

    fn lower_whitespace(&mut self, negated: bool) -> Result<Class> {
        todo!();
    }

    fn lower_word_char(&mut self, negated: bool) -> Result<Class> {
        todo!();
    }

    fn lower_class(&mut self, class: ast::Class) -> Result<Class> {
        todo!();
    }

    fn lower_group(&mut self, group: ast::Group) -> Result<Group> {
        todo!();
    }

    fn flag_is_set(&self, flag: &Flag) -> bool {
        self.flag_stack
            .last()
            .map(|flag_set| flag_set.contains(flag))
            .unwrap_or(false)
    }
}
