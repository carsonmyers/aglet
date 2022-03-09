use crate::parse::ast::*;
use crate::parse::error::*;

pub struct Parser {
    input: String,

    next: usize,
    peek: usize,
}

impl Parser {
    pub fn new(input: String) -> Self {
        Parser {
            input,

            next: 0,
            peek: 0,
        }
    }

    pub fn parse() -> Result<Ast> {

        Err(TokenizeError::NotImplemented)
    }
}