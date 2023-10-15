use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DataStruct, DeriveInput, Ident, Type};

pub fn impl_default_with_span(input: DeriveInput) -> TokenStream {
    let type_name = input.ident;
    if let Data::Struct(data) = input.data {
        impl_for_struct(type_name, data)
    } else if let Data::Enum(_) = input.data {
        impl_with_no_span(type_name)
    } else {
        panic!("can only derive DefaultAst for structs and enums");
    }
}

fn impl_for_struct(type_name: Ident, data: DataStruct) -> TokenStream {
    if let Some((index, field)) = find_span_field(data.fields) {
        if let Some(span_name) = field.ident {
            impl_with_named_span(type_name, span_name)
        } else {
            impl_with_numbered_span(type_name, index)
        }
    } else {
        impl_with_no_span(type_name)
    }
}

fn find_span_field(fields: syn::Fields) -> Option<(usize, syn::Field)> {
    fields
        .into_iter()
        .enumerate()
        .filter(|(_, field)| {
            let is_named_span = field
                .ident
                .as_ref()
                .map(|name| name == "span")
                .unwrap_or_default();
            let is_span_type = type_is_span(&field.ty);

            is_named_span || is_span_type
        })
        .next()
}

fn type_is_span(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };

    if path.path.is_ident("Span") {
        return true;
    }

    let Some(last_segment) = path.path.segments.last() else {
        return false;
    };

    return last_segment.ident == "Span";
}

fn impl_with_named_span(type_name: Ident, span_name: Ident) -> TokenStream {
    let s = quote! {
        impl aglet_text::DefaultWithSpan for #type_name {
            fn default_with_span(span: aglet_text::Span) -> Self {
                let mut result: Self = core::default::Default::default();
                result.#span_name = span;
                result
            }
        }
    };

    s.into()
}

fn impl_with_numbered_span(type_name: Ident, index: usize) -> TokenStream {
    let s = quote! {
        impl aglet_text::DefaultWithSpan for #type_name {
            fn default_with_span(span: aglet_text::Span) -> Self {
                let mut result: Self = core::default::Default::default();
                result.#index = span;
                result
            }
        }
    };

    s.into()
}

fn impl_with_no_span(type_name: Ident) -> TokenStream {
    let s = quote! {
        impl aglet_text::DefaultWithSpan for #type_name {
            fn default_with_span(_: aglet_text::Span) -> Self {
                core::default::Default::default()
            }
        }
    };

    s.into()
}
