use aglet_pretty::{Pretty, Result, Writer};
use colored::Color;

use crate::parse::ast::*;

const COLOR_BOUNDARY: Option<Color> = Some(Color::BrightRed);
const COLOR_REPETITION: Option<Color> = Some(Color::Cyan);
const COLOR_MATCH_ONE: Option<Color> = Some(Color::Green);
const COLOR_CLASS_ITEM: Option<Color> = Some(Color::BrightYellow);

impl Pretty for Ast {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.head)
    }
}

impl Pretty for Expr {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match &self.kind {
            ExprKind::Any => w.print_ast(".", Some(self.span), COLOR_MATCH_ONE).finish(),
            ExprKind::Literal(c) => w
                .print_ast("Literal", Some(self.span), COLOR_MATCH_ONE)
                .property(None, c, None)
                .finish(),
            ExprKind::Digit(negated) => w
                .print_ast("Digit", Some(self.span), COLOR_MATCH_ONE)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ExprKind::Whitespace(negated) => w
                .print_ast("Whitespace", Some(self.span), COLOR_MATCH_ONE)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ExprKind::WordChar(negated) => w
                .print_ast("WordChar", Some(self.span), COLOR_MATCH_ONE)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ExprKind::Empty => w.print_ast("Expr", Some(self.span), None).finish(),
            _ => w.print(&self.kind),
        }
    }
}

impl Pretty for ExprKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Alternation(alt) => w.print(alt),
            Self::Concatenation(concat) => w.print(concat),
            Self::Repetition(rep) => w.print(rep),
            Self::Any => w.print_ast(".", None, None).finish(),
            Self::Literal(c) => w
                .print_ast("Literal", None, COLOR_MATCH_ONE)
                .property(None, c, None)
                .finish(),
            Self::Digit(negated) => w
                .print_ast("Digit", None, COLOR_MATCH_ONE)
                .maybe_property(None, if *negated { Some(&"negated") } else { None }, None)
                .finish(),
            Self::Whitespace(negated) => w
                .print_ast("Whitespace", None, COLOR_MATCH_ONE)
                .maybe_property(None, if *negated { Some(&"negated") } else { None }, None)
                .finish(),
            Self::WordChar(negated) => w
                .print_ast("WordChar", None, COLOR_MATCH_ONE)
                .maybe_property(None, if *negated { Some(&"negated") } else { None }, None)
                .finish(),
            Self::Boundary(boundary) => w.print(boundary),
            Self::Group(group) => w.print(group),
            Self::Class(class) => w.print(class),
            Self::Empty => w.print_ast("Expr", None, None).finish(),
        }
    }
}

impl Pretty for Alternation {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Alt", Some(self.span), None);
        for expr in &self.items {
            printer.child(None, expr);
        }
        printer.finish()
    }
}

impl Pretty for Concatenation {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Concat", Some(self.span), None);
        for expr in &self.items {
            printer.child(None, expr);
        }
        printer.finish()
    }
}

impl Pretty for Repetition {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match &self.kind {
            RepetitionKind::ZeroOrOne => w
                .print_ast("ZeroOrOne", Some(self.span), COLOR_REPETITION)
                .child(None, &*self.item)
                .finish(),
            RepetitionKind::ZeroOrMore => w
                .print_ast("ZeroOrMore", Some(self.span), COLOR_REPETITION)
                .child(None, &*self.item)
                .finish(),
            RepetitionKind::OneOrMore => w
                .print_ast("OneOrMore", Some(self.span), COLOR_REPETITION)
                .child(None, &*self.item)
                .finish(),
            RepetitionKind::Range(range) => w
                .print_ast("Range", Some(self.span), COLOR_REPETITION)
                .property(Some("start"), &range.start.unwrap_or(0), None)
                .maybe_property(Some("end"), range.end.as_ref(), None)
                .child(None, &*self.item)
                .finish(),
        }
    }
}

impl Pretty for Boundary {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Boundary", Some(self.span), COLOR_BOUNDARY)
            .property(Some("kind"), &self.kind, None)
            .finish()
    }
}

impl Pretty for Group {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.kind)
    }
}

impl Pretty for GroupKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Capturing(capturing) => w.print(capturing),
            Self::Named(named) => w.print(named),
            Self::NonCapturing(non_capturing) => w.print(non_capturing),
            Self::Flags(flags) => w.print(flags),
        }
    }
}

impl Pretty for CapturingGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Group", Some(self.span), None)
            .property(Some("index"), &self.index, None)
            .child(None, &*self.expr)
            .finish()
    }
}

impl Pretty for NamedGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Group", Some(self.span), None)
            .property(Some("name"), &self.name.value, None)
            .child(None, &*self.expr)
            .finish()
    }
}

impl Pretty for NonCapturingGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Group", Some(self.span), None);
        if let Some(flags) = &self.flags {
            printer.child(None, flags);
        }

        printer.child(None, &*self.expr).finish()
    }
}

impl Pretty for FlagGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.flags)
    }
}

impl Pretty for Flags {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Flags", Some(self.span), None)
            .property(Some("set"), &self.set_flags, None)
            .property(Some("clear"), &self.clear_flags, None)
            .finish()
    }
}

impl Pretty for Class {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match &self.kind {
            ClassKind::Unicode(class) => w
                .print_ast("UnicodeClass", Some(self.span), COLOR_MATCH_ONE)
                .maybe_property(
                    Some("negated"),
                    if self.negated { Some(&true) } else { None },
                    None,
                )
                .maybe_property(
                    Some("name"),
                    class.name.as_ref().map(|name| &name.value).as_ref(),
                    None,
                )
                .property(Some("value"), &class.value.value, None)
                .finish(),
            ClassKind::Specified(class) => w
                .print_ast("SpecifiedClass", Some(self.span), COLOR_MATCH_ONE)
                .maybe_property(
                    Some("negated"),
                    if self.negated { Some(&true) } else { None },
                    None,
                )
                .children(&class.items)
                .finish(),
        }
    }
}

impl Pretty for ClassSpec {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match &self.kind {
            ClassSpecKind::Literal(c) => w
                .print_ast("Literal", Some(self.span), COLOR_CLASS_ITEM)
                .property(None, c, None)
                .finish(),
            ClassSpecKind::Digit(negated) => w
                .print_ast("Digit", Some(self.span), COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ClassSpecKind::Whitespace(negated) => w
                .print_ast("Whitespace", Some(self.span), COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ClassSpecKind::WordChar(negated) => w
                .print_ast("WordChar", Some(self.span), COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            ClassSpecKind::Range(start, end) => w
                .print_ast("Range", Some(self.span), COLOR_CLASS_ITEM)
                .property(Some("start"), start, None)
                .property(Some("end"), end, None)
                .finish(),
            _ => w.print(&self.kind),
        }
    }
}

impl Pretty for ClassSpecKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Intersection(intersection) => w.print(intersection),
            Self::Difference(difference) => w.print(difference),
            Self::Symmetrical(symmetrical) => w.print(symmetrical),
            Self::Literal(c) => w
                .print_ast("Literal", None, COLOR_CLASS_ITEM)
                .property(None, c, None)
                .finish(),
            Self::Digit(negated) => w
                .print_ast("Digit", None, COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            Self::Whitespace(negated) => w
                .print_ast("Whitespace", None, COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            Self::WordChar(negated) => w
                .print_ast("WordChar", None, COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            Self::Range(start, end) => w
                .print_ast("Range", None, COLOR_CLASS_ITEM)
                .property(Some("start"), start, None)
                .property(Some("end"), end, None)
                .finish(),
            Self::Posix(posix) => w.print(posix),
            Self::Class(class) => w.print(class),
        }
    }
}

impl Pretty for Intersection {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Intersection", Some(self.span), COLOR_CLASS_ITEM)
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for Difference {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Difference", Some(self.span), COLOR_CLASS_ITEM)
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for Symmetrical {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("SymmectricalDifference", Some(self.span), COLOR_CLASS_ITEM)
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for PosixClass {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("PosixClass", Some(self.span), COLOR_CLASS_ITEM)
            .property(None, &self.kind, None)
            .finish()
    }
}
