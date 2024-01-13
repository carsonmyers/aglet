use std::fmt::{self, Write};

use aglet_text::Span;
use colored::{Color, Colorize};

use crate::{Pretty, Result, Writer};

const DEFAULT_COLOR: Color = Color::White;
const PARAMETER_COLOR: Color = Color::TrueColor {
    r: 150,
    g: 150,
    b: 150,
};

/// Printer for an abstract syntax tree
///
/// Tree nodes can be printed with properties and children, with children being
/// printed indented as another node.
///
/// # Example
///
/// An alternation node with two literal children might be printed like this:
///
/// ```ast
/// (Alternation
///     (Literal value='a')
///     (Literal value='b'))
/// ```
pub struct AstPrinter<'a, 'b: 'a> {
    writer: &'a mut Writer<'b>,
    result: Result,
}

impl<'a, 'b: 'a> AstPrinter<'a, 'b> {
    /// Begin printing an AST node - its name will be printed, and its span will be added
    /// to the output at this stage.
    ///
    /// To keep spans aligned with their nodes, all children should either also be printed
    /// with this printer, or the caller that adds the children needs to supply a span
    /// (or `None`) for each child printed which isn't a node.
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

    /// Add a property to the node.
    ///
    /// Properties are printed on the same line as the node, and can optionally be
    /// prefixed with a name.
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
                    name_text.color(PARAMETER_COLOR).italic(),
                    format!("{:?}", value).color(color)
                )?;
            } else {
                write!(self.writer, " {}{:?}", name_text, value)?;
            }

            Ok(())
        });

        self
    }

    /// Maybe add a property to the node.
    ///
    /// The property will be added if `value` is not `None`. The property is printed
    /// on the same line as the node and can be optionally prefixed with a name.
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

    /// Add a child to the node.
    ///
    /// The child will be recursively pretty printed at a higher level of indentation.
    /// Children are printed on a new line and so should add a value to `w.spans`
    /// and to `w.meta` if applicable.
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

    /// Add multiple children to the node
    ///
    /// Children will be recursively printed at a higher level of indentation.
    /// Children are printed on a new line and so should use [`Writer.add_span()`][1]
    /// and [`Writer.add_meta()`][2] if appliccable
    ///
    /// [1]: crate::writer::Writer::add_span
    /// [2]: crate::writer::Writer::add_meta
    pub fn children(&mut self, items: &Vec<impl Pretty>) -> &mut Self {
        for item in items {
            self.child(None, item);
        }

        self
    }

    /// Finish the AST node.
    ///
    /// Writes the closing `)` of the node and returns the result of printing all of its
    /// parts. If errors occurred, only the first will be returned (an no printing will have
    /// taken place since it occurred).
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
    use crate::{ColorWhen, PrettyPrintSettings, PrettyPrinter};

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
                span:  Span::new(0, 11),
                left:  Box::new(Expr {
                    kind: ExprKind::Add(Add {
                        span:  Span::new(0, 7),
                        left:  Box::new(Expr {
                            kind: ExprKind::Number(Number {
                                span:  Span::new(0, 2),
                                value: 10,
                            }),
                        }),
                        right: Box::new(Expr {
                            kind: ExprKind::Number(Number {
                                span:  Span::new(5, 7),
                                value: -1,
                            }),
                        }),
                    }),
                }),
                right: Box::new(Expr {
                    kind: ExprKind::Number(Number {
                        span:  Span::new(10, 11),
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
            "0 - 11:\t(Add\n",
            "0 - 7:\t  (Add\n",
            "0 - 2:\t    (Number value=10)\n",
            "5 - 7:\t    (Number value=-1))\n",
            "10 - 11:\t  (Number value=6))"
        );

        let out = printer.print(&expr).expect("print failed").finish();
        assert!(out.is_ok());
        let out = out.unwrap();
        assert_eq!(expected, out);
    }
}
