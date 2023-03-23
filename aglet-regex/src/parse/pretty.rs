use aglet_pretty::{Pretty, Result, Writer};

use crate::parse::ast::*;

impl Pretty for Ast {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.head)
    }
}

impl Pretty for Expr {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.kind)
    }
}

impl Pretty for ExprKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Alternation(alt) => w.print(alt),
            Self::Concatenation(concat) => w.print(concat),
            Self::Repetition(rep) => w.print(rep),
            Self::Any => w.print_ast(".").finish(),
            Self::Literal(c) => w.print_ast("Literal").property(None, c).finish(),
            Self::Boundary(boundary) => w.print(boundary),
            Self::Group(group) => w.print(group),
            Self::Class(class) => w.print(class),
            Self::Empty => w.print_ast("Expr").finish(),
        }
    }
}

impl Pretty for Alternation {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Alt");
        for expr in &self.items {
            printer.child(None, expr);
        }
        printer.finish()
    }
}

impl Pretty for Concatenation {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Concat");
        for expr in &self.items {
            printer.child(None, expr);
        }
        printer.finish()
    }
}

impl Pretty for Repetition {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Repeat")
            .property(Some("kind"), &self.kind)
            .child(None, &*self.item)
            .finish()
    }
}

impl Pretty for Boundary {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Boundary")
            .property(Some("kind"), &self.kind)
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
        w.print_ast("Group")
            .property(Some("index"), &self.index)
            .child(None, &*self.expr)
            .finish()
    }
}

impl Pretty for NamedGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Group")
            .property(Some("name"), &self.name.value)
            .child(None, &*self.expr)
            .finish()
    }
}

impl Pretty for NonCapturingGroup {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Group");
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
        w.print_ast("Flags")
            .child(Some("set"), &self.set_flags)
            .child(Some("clear"), &self.clear_flags)
            .finish()
    }
}

impl Pretty for Class {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("Class");
        if self.negated {
            printer.property(None, &"negated");
        }

        printer.child(None, &self.kind).finish()
    }
}

impl Pretty for ClassKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Unicode(unicode) => w.print(unicode),
            Self::Specified(items) => {
                let mut printer = w.print_ast("ClassSpec");
                for item in items {
                    printer.child(None, item);
                }

                printer.finish()
            },
        }
    }
}

impl Pretty for UnicodeClass {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let mut printer = w.print_ast("UnicodeClass");
        if let Some(name) = &self.name {
            printer.property(Some("name"), &name.value);
        }
        printer.property(Some("value"), &self.value.value).finish()
    }
}

impl Pretty for ClassSpec {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print(&self.kind)
    }
}

impl Pretty for ClassSpecKind {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        match self {
            Self::Intersection(intersection) => w.print(intersection),
            Self::Difference(difference) => w.print(difference),
            Self::Symmetrical(symmetrical) => w.print(symmetrical),
            Self::Literal(c) => w.print_ast("Literal").property(None, c).finish(),
            Self::Range(start, end) => w
                .print_ast("Range")
                .property(Some("start"), start)
                .property(Some("end"), end)
                .finish(),
            Self::Posix(posix) => w.print(posix),
            Self::Class(class) => w.print(class),
        }
    }
}

impl Pretty for Intersection {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Intersection")
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for Difference {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("Difference")
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for Symmetrical {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("SymmectricalDifference")
            .child(None, &*self.left)
            .child(None, &*self.right)
            .finish()
    }
}

impl Pretty for PosixClass {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        w.print_ast("PosixClass")
            .property(None, &self.kind)
            .finish()
    }
}
