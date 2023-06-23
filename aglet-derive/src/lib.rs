use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(SpanPrinter)]
pub fn span_printer_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_span_printer_macro(&ast)
}

fn impl_span_printer_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl SpanPrinter for #name {
            fn print_with_span(&self, source: &str) {
                println!("hello, world!");
            }
        }
    };

    gen.into()
}
