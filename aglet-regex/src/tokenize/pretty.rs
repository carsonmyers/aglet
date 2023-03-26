use aglet_pretty::{Pretty, Result, Writer};
use colored::Color;

use crate::tokenize::state::State;
use crate::tokenize::token::*;

const COLOR_BOUNDARY: Option<Color> = Some(Color::BrightRed);
const COLOR_REPETITION: Option<Color> = Some(Color::Cyan);
const COLOR_MATCH_ONE: Option<Color> = Some(Color::Green);
const COLOR_CLASS_ITEM: Option<Color> = Some(Color::BrightYellow);

impl Pretty for TokenStack {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        let stack = Some(format!("\t\u{2192} {:?}", self.stack));
        let in_class = self
            .stack
            .get()
            .map(|state| *state == State::Class)
            .unwrap_or(false);
        self.token.pretty_print(stack, in_class, w)
    }
}

impl Pretty for Token {
    fn print(&self, w: &mut Writer<'_>) -> Result {
        self.pretty_print(None, false, w)
    }
}

impl Token {
    fn pretty_print(&self, stack: Option<String>, in_class: bool, w: &mut Writer<'_>) -> Result {
        match &self.kind {
            TokenKind::Literal(c) => w
                .print_token(
                    "Literal",
                    Some(self.span),
                    stack,
                    if in_class {
                        COLOR_CLASS_ITEM
                    } else {
                        COLOR_MATCH_ONE
                    },
                )
                .property(None, c, None)
                .finish(),
            TokenKind::Digit(negated) => w
                .print_token(
                    "Digit",
                    Some(self.span),
                    stack,
                    if in_class {
                        COLOR_CLASS_ITEM
                    } else {
                        COLOR_MATCH_ONE
                    },
                )
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::Whitespace(negated) => w
                .print_token(
                    "Whitespace",
                    Some(self.span),
                    stack,
                    if in_class {
                        COLOR_CLASS_ITEM
                    } else {
                        COLOR_MATCH_ONE
                    },
                )
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::WordChar(negated) => w
                .print_token(
                    "WordChar",
                    Some(self.span),
                    stack,
                    if in_class {
                        COLOR_CLASS_ITEM
                    } else {
                        COLOR_MATCH_ONE
                    },
                )
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::Dot => w
                .print_token("Dot", Some(self.span), stack, COLOR_MATCH_ONE)
                .finish(),
            TokenKind::Alternate => w
                .print_token("Alternate", Some(self.span), stack, None)
                .finish(),
            TokenKind::StartOfLine => w
                .print_token("StartOfLine", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::EndOfLine => w
                .print_token("EndOfLine", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::StartOfText => w
                .print_token("StartOfText", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::EndOfText => w
                .print_token("EndOfText", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::WordBoundary => w
                .print_token("WordBoundary", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::NonWordBoundary => w
                .print_token("NonWordBoundary", Some(self.span), stack, COLOR_BOUNDARY)
                .finish(),
            TokenKind::OpenBrace => w
                .print_token("OpenBrace", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::CloseBrace => w
                .print_token("CloseBrace", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::Number(value) => w
                .print_token("Number", Some(self.span), stack, COLOR_REPETITION)
                .property(None, value, None)
                .finish(),
            TokenKind::Comma => w
                .print_token("Comma", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::Question => w
                .print_token("Question", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::Star => w
                .print_token("Star", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::Plus => w
                .print_token("Plus", Some(self.span), stack, COLOR_REPETITION)
                .finish(),
            TokenKind::OpenGroup => w
                .print_token("OpenGroup", Some(self.span), stack, None)
                .finish(),
            TokenKind::CloseGroup => w
                .print_token("CloseGroup", Some(self.span), stack, None)
                .finish(),
            TokenKind::NonCapturing => w
                .print_token("NonCapturing", Some(self.span), stack, None)
                .finish(),
            TokenKind::Name(name) => w
                .print_token("Name", Some(self.span), stack, None)
                .property(None, name, None)
                .finish(),
            TokenKind::Flags(set, clear) => w
                .print_token("Flags", Some(self.span), stack, None)
                .property(Some("set"), set, None)
                .property(Some("clear"), clear, None)
                .finish(),
            TokenKind::NonCapturingFlags(set, clear) => w
                .print_token("NonCapturingFlags", Some(self.span), stack, None)
                .property(Some("set"), set, None)
                .property(Some("clear"), clear, None)
                .finish(),
            TokenKind::OpenBracket => w
                .print_token("OpenBracket", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::CloseBracket => w
                .print_token("CloseBracket", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::Negated => w
                .print_token("Negated", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::Range => w
                .print_token("Range", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::Symmetrical => w
                .print_token("Symmetrical", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::Difference => w
                .print_token("Difference", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::Intersection => w
                .print_token("Intersection", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::UnicodeShort(category, negated) => w
                .print_token("UnicodeShort", Some(self.span), stack, COLOR_CLASS_ITEM)
                .property(None, category, None)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::UnicodeLongStart(negated) => w
                .print_token("UnicodeLongStart", Some(self.span), stack, COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::UnicodeLongEnd => w
                .print_token("UnicodeLongEnd", Some(self.span), stack, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::UnicodePropName(name) => w
                .print_token("UnicodePropName", Some(self.span), stack, COLOR_CLASS_ITEM)
                .property(None, name, None)
                .finish(),
            TokenKind::UnicodeEqual(negated) => w
                .print_token("UnicodeEqual", Some(self.span), stack, COLOR_CLASS_ITEM)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
            TokenKind::UnicodePropValue(value) => w
                .print_token("UnicodePropValue", Some(self.span), stack, COLOR_CLASS_ITEM)
                .property(None, value, COLOR_CLASS_ITEM)
                .finish(),
            TokenKind::ClassName(name, negated) => w
                .print_token("ClassName", Some(self.span), stack, COLOR_CLASS_ITEM)
                .property(Some("name"), name, None)
                .maybe_property(
                    Some("negated"),
                    if *negated { Some(&true) } else { None },
                    None,
                )
                .finish(),
        }
    }
}