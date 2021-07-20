use color_eyre::Report;
use std::str::Chars;
use crate::ingest::{Ingest, IngestError};

pub struct StringIngest {
    data: Vec<char>,
    n: usize,
    p: usize,
    l: usize,
    e: usize,
}

impl StringIngest {
    pub fn new<T>(data: T) -> StringIngest
        where T: AsRef<String>
    {
        let string: String = data.as_ref().into();
        let data = string.chars().collect();

        StringIngest {
            data,
            n: 0,
            p: 0,
            l: 0,
            e: data.len(),
        }
    }
}

impl <'a> Ingest<'a> for StringIngest {
    type Unit = char;

    fn next(&mut self) -> Result<Self::Unit, Report> {
        if self.n >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        let c = self.data[self.n];
        self.n += 1;
        self.p = self.n;
        Ok(c)
    }

    fn next_while<P, I>(&'a mut self, p: P) -> Result<I, Report> where
        P: Fn(&'a Self::Unit) -> bool,
        I: Iterator<Item = Self::Unit>
    {
        if self.n >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        let mut original_n = self.n;
        while self.n < self.e {
            if !p(&self.data[self.n]) {
                break;
            }

            self.n += 1;
        }

        self.p = self.n;
        Ok(self.data[original_n..self.n].into_iter())
    }

    fn peek(&mut self) -> Result<Self::Unit, Report> {
        if self.p >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        let c = self.data[self.p];
        self.p += 1;
        Ok(c)
    }

    fn peek_while<P, I>(&mut self, p: P) -> Result<&I, Report> where
        P: Fn(&'a Self::Unit) -> bool,
        I: Iterator<Item = Self::Unit>
    {
        if self.p >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        let original_p = self.p;
        while self.p < self.e {
            if !p(&self.data[self.p]) {
                break;
            }

            self.p += 1;
        }

        Ok(self.data[original_p..self.p].iter())
    }

    fn skip(&mut self) -> Result<(), Report> {
        if self.n >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        self.n += 1;
        self.p = self.n;
        Ok(())
    }

    fn skip_while<P>(&mut self, p: P) -> Result<(), Report> where
        P: Fn(&'a Self::Unit) -> bool
    {
        if self.n >= self.e {
            return Err(IngestError::EndOfFile.into());
        }

        while self.n < self.e {
            if !p(&self.data[self.n]) {
                break;
            }

            self.n += 1;
        }

        self.p = self.n;
        Ok(())
    }

    fn take<I>(&mut self) -> Result<I, Report> where
        I: Iterator<Item = Self::Unit>
    {
        let original_l = self.l;
        self.l = self.n;
        Ok(self.data[original_l..self.n].into_iter())
    }

    fn discard(&mut self) {
        self.l = self.n;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut ingest: dyn Ingest = StringIngest::new("abc");
        let unit = ingest.next();
        assert!(unit.is_ok());
        let c = unit.unwrap();
        assert_eq!(c, 'a');
    }
}