use std::hash::Hash;

use aglet_derive::BorrowKey;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, BorrowKey)]
pub(crate) struct Key2(pub(crate) String, pub(crate) String);
