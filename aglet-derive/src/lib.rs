mod default_with_span;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(DefaultWithSpan)]
pub fn default_with_span(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    default_with_span::impl_default_with_span(input)
}
