use std::fmt::{self, Write};

use thiserror;

pub struct PrettyPrinter {
    indent: String,
}

impl PrettyPrinter {
    pub fn new(indent: &'_ str) -> Self {
        Self {
            indent: indent.to_string(),
        }
    }

    pub fn print(&self, item: &impl Pretty) -> Result {
        let out = self.print_buf(item)?;
        println!("{}", out);
        Ok(())
    }

    fn print_buf(&self, item: &impl Pretty) -> std::result::Result<String, Error> {
        let mut buf = String::new();
        let mut writer = Writer::new(&mut buf).with_indent(&self.indent);
        item.print(&mut writer)?;

        Ok(buf)
    }
}

pub struct Writer<'a> {
    buf:        &'a mut (dyn Write + 'a),
    on_newline: bool,
    indent:     String,
    level:      i32,
}

impl<'a> Writer<'a> {
    pub fn new(buf: &'a mut (dyn Write + 'a)) -> Self {
        Self {
            buf,
            on_newline: true,
            indent: "\t".to_string(),
            level: 0,
        }
    }

    pub fn with_indent(mut self, indent: &str) -> Self {
        self.indent = indent.to_string();
        self
    }

    pub fn print_indent(&mut self) -> fmt::Result {
        for _ in 0..self.level {
            self.buf.write_str(self.indent.as_str())?;
        }

        Ok(())
    }

    pub fn print_ast<'b>(&'b mut self, name: &str) -> AstPrinter<'b, 'a> {
        AstPrinter::new(self, name)
    }

    pub fn print(&mut self, item: &impl Pretty) -> Result {
        item.print(self)
    }
}

impl Write for Writer<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for line in s.split_inclusive('\n') {
            if self.on_newline {
                self.print_indent()?;
            }

            self.on_newline = line.ends_with('\n');
            self.buf.write_str(line)?;
        }

        Ok(())
    }
}

pub struct AstPrinter<'a, 'b: 'a> {
    writer: &'a mut Writer<'b>,
    result: Result,
}

impl<'a, 'b: 'a> AstPrinter<'a, 'b> {
    pub fn new(writer: &'a mut Writer<'b>, name: &str) -> Self {
        let result = write!(writer, "({}", name).map_err(|err| err.into());
        AstPrinter { writer, result }
    }

    pub fn property(&mut self, name: Option<&str>, value: &impl fmt::Debug) -> &mut Self {
        self.result = self.result.and_then(|_| {
            if let Some(name) = name {
                write!(self.writer, " {}={:?}", name, value)?;
            } else {
                write!(self.writer, " {:?}", value)?;
            }
            Ok(())
        });

        self
    }

    pub fn child(&mut self, name: Option<&str>, item: &impl Pretty) -> &mut Self {
        self.result = self.result.and_then(|_| {
            write!(self.writer, "\n")?;

            self.writer.level += 1;
            if let Some(name) = name {
                write!(self.writer, "{}: ", name)?;
            }

            item.print(self.writer)?;
            self.writer.level -= 1;

            Ok(())
        });

        self
    }

    pub fn finish(&mut self) -> Result {
        self.result.and_then(|_| {
            write!(self.writer, ")")?;
            Ok(())
        })
    }
}

pub trait Pretty {
    fn print(&self, w: &mut Writer<'_>) -> Result;
}

impl<T: fmt::Debug> Pretty for Vec<T> {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        write!(w, "{:?}", self).map_err(|err| err.into())
    }
}

pub type Result = std::result::Result<(), Error>;

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum Error {
    #[error("format error: {0}")]
    FormatError(#[from] fmt::Error),

    #[error("children have already been written")]
    ChildrenAlreadyWritten,
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Expr {
        kind: ExprKind,
    }

    impl Pretty for Expr {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            w.print(&self.kind)
        }
    }

    enum ExprKind {
        Add(Add),
        Number(Number),
    }

    impl Pretty for ExprKind {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            match self {
                Self::Add(add) => w.print(add),
                Self::Number(number) => w.print(number),
            }
        }
    }

    struct Add {
        left:  Box<Expr>,
        right: Box<Expr>,
    }

    impl Pretty for Add {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            w.print_ast("Add")
                .child(None, &*self.left)
                .child(None, &*self.right)
                .finish()
        }
    }

    struct Number {
        value: i32,
    }

    impl Pretty for Number {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            w.print_ast("Number")
                .property(Some("value"), &self.value)
                .finish()
        }
    }

    #[test]
    fn print_ast() {
        let expr = Expr {
            kind: ExprKind::Add(Add {
                left:  Box::new(Expr {
                    kind: ExprKind::Add(Add {
                        left:  Box::new(Expr {
                            kind: ExprKind::Number(Number { value: 10 }),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Number(Number { value: -1 }),
                        }),
                    }),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Number(Number { value: 6 }),
                }),
            }),
        };

        let mut printer = PrettyPrinter::new("  ");

        let expected = concat!(
            "(Add\n",
            "  (Add\n",
            "    (Number value=10)\n",
            "    (Number value=-1))\n",
            "  (Number value = 6))\n"
        );
        let out = printer.print_buf(&expr);
        assert!(out.is_ok());
        let out = out.unwrap();
        assert_eq!(out, expected);
    }
}
