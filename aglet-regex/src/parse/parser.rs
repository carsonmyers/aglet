use crate::parse::ast::*;
use crate::parse::error::*;
use crate::tokenize::{Token, TokenizeError, Tokenizer};

pub struct Parser<'a> {
    input: Tokenizer<'a>,
    ast:   Ast,
}

impl Parser {
    pub fn new(input: Tokenizer) -> Self {
        Parser {
            input,
            ast: Ast::
        }
    }

    pub fn parse() -> Result<Ast> { Err(TokenizeError::NotImplemented) }
}
