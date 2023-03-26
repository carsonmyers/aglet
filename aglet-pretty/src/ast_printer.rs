use std::fmt::{self, Write};

use aglet_text::Span;
use colored::{Color, Colorize};

use crate::Pretty;
use crate::Result;
use crate::Writer;

const DEFAULT_COLOR: Color = Color::White;

pub struct AstPrinter<'a, 'b: 'a> {
    writer: &'a mut Writer<'b>,
    result: Result,
}

impl<'a, 'b: 'a> AstPrinter<'a, 'b> {
    pub fn new(
        writer: &'a mut Writer<'b>,
        name: &str,
        span: Option<Span>,
        color: Option<Color>,
    ) -> Self {
        let result = Ok(()).and_then(|_| {
            if writer.use_color {
                let color = color.unwrap_or(DEFAULT_COLOR);
                write!(writer, "({}", name.color(color))?;
            } else {
                write!(writer, "({}", name)?;
            }

            Ok(())
        });

        writer.add_span(span);

        AstPrinter { writer, result }
    }

    pub fn property(
        &mut self,
        name: Option<&str>,
        value: &impl fmt::Debug,
        color: Option<Color>,
    ) -> &mut Self {
        self.result = self.result.and_then(|_| {
            let name_text = if let Some(name) = name {
                format!("{}=", name)
            } else {
                String::new()
            };

            if self.writer.use_color {
                let color = color.unwrap_or(DEFAULT_COLOR);
                write!(
                    self.writer,
                    " {}{}",
                    name_text.truecolor(150, 150, 150).italic(),
                    format!("{:?}", value).color(color)
                )?;
            } else {
                write!(self.writer, " {}{:?}", name_text, value)?;
            }

            Ok(())
        });

        self
    }

    pub fn maybe_property(
        &mut self,
        name: Option<&str>,
        value: Option<&impl fmt::Debug>,
        color: Option<Color>,
    ) -> &mut Self {
        if let Some(value) = value {
            self.property(name, value, color)
        } else {
            self
        }
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

    pub fn children(&mut self, items: &Vec<impl Pretty>) -> &mut Self {
        for item in items {
            self.child(None, item);
        }

        self
    }

    pub fn finish(&mut self) -> Result {
        self.result.and_then(|_| {
            write!(self.writer, ")")?;
            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use aglet_text::Span;

    use super::*;
    use crate::ColorWhen;
    use crate::PrettyPrintSettings;
    use crate::PrettyPrinter;

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
        span:  Span,
        left:  Box<Expr>,
        right: Box<Expr>,
    }

    impl Pretty for Add {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            w.print_ast("Add", Some(self.span), None)
                .child(None, &*self.left)
                .child(None, &*self.right)
                .finish()
        }
    }

    struct Number {
        span:  Span,
        value: i32,
    }

    impl Pretty for Number {
        fn print(&self, w: &mut Writer<'_>) -> Result {
            w.print_ast("Number", Some(self.span), None)
                .property(Some("value"), &self.value, None)
                .finish()
        }
    }

    #[test]
    fn print_ast() {
        // 10 + -1 + 6
        // 0   4   8

        let expr = Expr {
            kind: ExprKind::Add(Add {
                span:  Span::from_offsets(0, 11),
                left:  Box::new(Expr {
                    kind: ExprKind::Add(Add {
                        span:  Span::from_offsets(0, 7),
                        left:  Box::new(Expr {
                            kind: ExprKind::Number(Number {
                                span:  Span::from_offsets(0, 2),
                                value: 10,
                            }),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Number(Number {
                                span:  Span::from_offsets(5, 7),
                                value: -1,
                            }),
                        }),
                    }),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Number(Number {
                        span:  Span::from_offsets(10, 11),
                        value: 6,
                    }),
                }),
            }),
        };

        let mut printer = PrettyPrinter::new(
            PrettyPrintSettings::default()
                .align(false)
                .include_meta(false)
                .color_when(ColorWhen::Never),
        );

        let expected = concat!(
            "1:1[0] - 1:12[11]:\t(Add\n",
            "1:1[0] - 1:8[7]:\t  (Add\n",
            "1:1[0] - 1:3[2]:\t    (Number value=10)\n",
            "1:6[5] - 1:8[7]:\t    (Number value=-1))\n",
            "1:11[10] - 1:12[11]:\t  (Number value=6))"
        );

        let out = printer.print(&expr).expect("print failed").finish();
        assert!(out.is_ok());
        let out = out.unwrap();
        assert_eq!(out, expected);
    }
}
