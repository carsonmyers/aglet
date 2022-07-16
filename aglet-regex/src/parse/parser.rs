use crate::parse::ast::*;
use crate::parse::error::*;
use crate::parse::state::*;
use crate::tokenize::{Token, TokenKind, TokenizeError, Tokenizer};

pub struct Parser<'a> {
    input: Tokenizer<'a>,
    state: StateStack,
    ast:   Ast,
}

impl Parser {
    pub fn new(input: Tokenizer) -> Self {
        Parser {
            input,
            state: StateStack::new(),
            ast: Ast::default(),
        }
    }

    pub fn parse(mut self) -> Result<Ast> {
        let mut items = Vec::new();
        loop {
            if let Some(Ok(tok)) = self.input.next() {
                items.push(self.parse_tok(tok)?);
            }
        }
    }

    fn parse_tok(&mut self, tok: Token) -> Result<AstKind> {
        match tok.kind {
            TokenKind::Literal(c) => Ok(AstKind::Literal {
                span:  tok.span,
                value: c,
            }),
            _ => panic!(),
        }
    }
}
