mod borrow_key;
mod default_with_span;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(BorrowKey)]
#[proc_macro_error]
pub fn borrow_key(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    borrow_key::impl_borrow_key(input)
}

#[proc_macro_derive(DefaultWithSpan)]
pub fn default_with_span(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    default_with_span::impl_default_with_span(input)
}
