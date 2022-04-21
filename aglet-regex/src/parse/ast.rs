use aglet_text::Span;

pub struct Ast {
    pub head: Option<AstKind>,
}

impl Default for Ast {
    fn default() -> Self { Ast { head: None } }
}

pub enum AstKind {
    Literal {
        span:  Span,
        value: char,
    },
    Any {
        span: Span,
    },
    Boundary {
        span: Span,
        kind: BoundaryKind,
    },
    Repetition {
        span: Span,
        kind: RepetitionKind,
    },
    Alternation {
        span:    Span,
        options: Vec<Ast>,
    },
    Concatenation {
        span:  Span,
        items: Vec<Ast>,
    },
    Group {
        span:     Span,
        kind:     GroupKind,
        contents: Box<Ast>,
    },
    Class {
        span: Span,
        kind: ClassKind,
    },
}

pub enum BoundaryKind {
    StartOfLine,
    EndOfLine,
    StartOfText,
    EndOfText,
    WordBoundary,
    NonWordBoundary,
}

pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Range(Option<usize>, Option<usize>),
}

pub enum GroupKind {
    Capturing(usize),
    Named(StringSpan),
    NonCapturing,
}

pub enum ClassKind {
    Posix {
        negated: bool,
        kind:    PosixKind,
    },
    Unicode {
        negated: bool,
        name:    Option<StringSpan>,
        value:   StringSpan,
    },
    Specified {
        negated: bool,
        spec:    ClassSpec,
    },
}

pub enum PosixKind {
    Upper(Span),
    Lower(Span),
    Alpha(Span),
    Digit(Span),
    XDigit(Span),
    AlNum(Span),
    Punct(Span),
    Blank(Span),
    Space(Span),
    Cntrl(Span),
    Graph(Span),
    Print(Span),
    Word(Span),
}

pub struct ClassSpec {
    pub span:  Span,
    pub items: Vec<ClassSpecKind>,
}

pub enum ClassSpecKind {
    Intersection(ClassSpec, ClassSpec),
    Difference(ClassSpec, ClassSpec),
    Symmetric(ClassSpec, ClassSpec),
    Literal(char),
    Range(char, char),
    Class(ClassKind),
    Union(ClassSpec),
}

pub struct StringSpan {
    span:  Span,
    value: String,
}
