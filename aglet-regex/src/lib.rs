pub mod parse;
pub mod tokenize;

use parse::{ast::Ast, Parser};
use tokenize::{Token, Tokenizer};

pub fn tokenize<S: AsRef<str>>(input: &str) -> Result<Vec<Token>, tokenize::Error> {
    let tr = Tokenizer::new(input);
    tr.collect::<Result<Vec<_>, _>>()
}

pub fn parse<S: AsRef<str>>(input: &str) -> Result<Ast, parse::Error> {
    let tr = Tokenizer::new(input);
    let p = Parser::new(tr);
    p.parse()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
