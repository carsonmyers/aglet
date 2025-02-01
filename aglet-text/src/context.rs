use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use crate::Error;
use crate::UnicodeContextKind::{AfterI, AfterSoftDotted, BeforeDot, Final, FinalSigma, MoreAbove};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum UnicodeContextKind {
    AfterI,
    AfterSoftDotted,
    BeforeDot,
    Final,
    FinalSigma,
    MoreAbove,
}

impl fmt::Debug for UnicodeContextKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use UnicodeContextKind::*;
        match self {
            AfterI => write!(f, "After_I"),
            AfterSoftDotted => write!(f, "After_Soft_Dotted"),
            BeforeDot => write!(f, "Before_Dot"),
            Final => write!(f, "Final"),
            FinalSigma => write!(f, "Final_Sigma"),
            MoreAbove => write!(f, "More_Above"),
        }
    }
}

impl Display for UnicodeContextKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl FromStr for UnicodeContextKind {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "after_i" => Ok(AfterI),
            "after-i" => Ok(AfterI),
            "after_soft_dotted" => Ok(AfterSoftDotted),
            "after-soft-dotted" => Ok(AfterSoftDotted),
            "before_dot" => Ok(BeforeDot),
            "before-dot" => Ok(BeforeDot),
            "final" => Ok(Final),
            "final_sigma" => Ok(FinalSigma),
            "final-sigma" => Ok(FinalSigma),
            "more_above" => Ok(MoreAbove),
            "more-above" => Ok(MoreAbove),
            _ => Err(Error::UnsupportedUnicodeContext(s.into())),
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct UnicodeContext {
    pub kind: UnicodeContextKind,
    pub negated: bool,
}

impl UnicodeContext {
    pub fn new(kind: UnicodeContextKind, negated: bool) -> Self {
        Self { kind, negated }
    }
}

impl fmt::Debug for UnicodeContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.negated {
            write!(f, "Not_{}", self.kind)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl Display for UnicodeContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
