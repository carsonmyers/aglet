use color_eyre::Report;

pub trait Ingest<'a> {
    type Unit;

    fn next(&mut self) -> Result<Self::Unit, Report>;
    fn next_while<P, I>(&'a mut self, p: P) -> Result<I, Report> where
        P: Fn(&'a Self::Unit) -> bool,
        I: Iterator<Item = Self::Unit>;

    fn peek(&mut self) -> Result<Self::Unit, Report>;
    fn peek_while<P, I>(&'a mut self, p: P) -> Result<&'a I, Report> where
        P: Fn(&'a Self::Unit) -> bool,
        I: Iterator<Item = Self::Unit>;

    fn skip(&'a mut self) -> Result<(), Report>;
    fn skip_while<P>(&'a mut self, p: P) -> Result<(), Report> where
        P: Fn(&'a Self::Unit) -> bool;

    fn take<I>(&'a mut self) -> Result<I, Report> where
        I: Iterator<Item = Self::Unit>;
    fn discard(&'a mut self);
}

mod string;
mod error;

pub use string::*;
pub use error::*;