use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_error::abort;
use quote::ToTokens;
use syn::punctuated::{Pair, Punctuated};
use syn::token::Comma;
use syn::{
    parse_quote, Attribute, Data, DeriveInput, Expr, ExprCall, ExprStruct, FieldValue, Fields,
    FieldsNamed, FieldsUnnamed, Ident, ItemImpl, ItemTrait, Type,
};

pub fn impl_borrow_key(mut input: DeriveInput) -> TokenStream {
    let owned_struct_name = input.ident.clone();

    to_borrowed_input(&mut input);
    to_borrowed_fields(&mut input);

    let mut tokens = proc_macro2::TokenStream::new();
    input.to_tokens(&mut tokens);

    let trait_name = Ident::new(&format!("Borrow{}", &owned_struct_name), Span::call_site());
    write_borrow_trait(&trait_name, &input).to_tokens(&mut tokens);
    write_owned_trait_impl(&trait_name, &owned_struct_name, &input).to_tokens(&mut tokens);
    write_borrowed_trait_impl(&trait_name, &input).to_tokens(&mut tokens);
    for std_impl in write_std_impls(&trait_name, &owned_struct_name) {
        std_impl.to_tokens(&mut tokens);
    }

    tokens.into()
}

fn to_borrowed_input(input: &mut DeriveInput) {
    let borrowed_name = Ident::new(&format!("Borrowed{}", input.ident), Span::call_site());
    let derives: Attribute = parse_quote! {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
    };

    input.ident = borrowed_name;
    input.generics.params.push(parse_quote!('_k));
    input.attrs.push(derives);
}

fn to_borrowed_fields(input: &mut DeriveInput) {
    let Data::Struct(ref mut data) = input.data else {
        abort!(input, "can only derive BorrowKey for structs");
    };

    match data.fields {
        Fields::Named(ref mut named) => {
            to_named_borrowed_fields(named);
        },
        Fields::Unnamed(ref mut unnamed) => {
            to_unnamed_borrowed_fields(unnamed);
        },
        Fields::Unit => abort!(data.fields, "key type must have fields"),
    }
}

fn to_named_borrowed_fields(owned_fields: &mut FieldsNamed) {
    owned_fields.named.iter_mut().for_each(to_borrowed_field);
}

fn to_unnamed_borrowed_fields(owned_fields: &mut FieldsUnnamed) {
    owned_fields.unnamed.iter_mut().for_each(to_borrowed_field);
}

fn to_borrowed_field(field: &mut syn::Field) {
    let Type::Path(ref ty) = field.ty else {
        abort!(field.ty, "unsupported field type {:?} in key", field.ty);
    };

    let borrowed_type: Type = parse_quote!(&'_k <#ty as ::core::ops::Deref>::Target);
    field.ty = borrowed_type;
}

fn write_borrow_trait(trait_name: &Ident, borrowed_struct: &DeriveInput) -> ItemTrait {
    let struct_name = &borrowed_struct.ident;
    let mut borrow_trait: ItemTrait = parse_quote! {
        trait #trait_name {
            fn key<'k>(&'k self) -> #struct_name<'k>;
        }
    };

    borrow_trait.vis = borrowed_struct.vis.clone();
    borrow_trait
}

fn write_owned_trait_impl(
    trait_name: &Ident,
    owned_struct_name: &Ident,
    borrowed_struct: &DeriveInput,
) -> ItemImpl {
    let struct_name = &borrowed_struct.ident;
    let value = write_borrowed_value(borrowed_struct);

    parse_quote! {
        impl #trait_name for #owned_struct_name {
            fn key<'k>(&'k self) -> #struct_name<'k> {
                #value
            }
        }
    }
}

fn write_borrowed_trait_impl(trait_name: &Ident, borrowed_struct: &DeriveInput) -> ItemImpl {
    let struct_name = &borrowed_struct.ident;

    parse_quote! {
        impl<'a> #trait_name for #struct_name<'a> {
            fn key<'k>(&'k self) -> #struct_name<'k> {
                *self
            }
        }
    }
}

fn write_borrowed_value(borrowed_struct: &DeriveInput) -> Expr {
    let struct_name = &borrowed_struct.ident;

    let Data::Struct(ref data) = borrowed_struct.data else {
        unreachable!()
    };

    match &data.fields {
        Fields::Named(fields) => {
            let fields = write_named_borrow_fields(fields);
            let val: ExprStruct = parse_quote!(#struct_name { #fields } );
            Expr::Struct(val)
        },
        Fields::Unnamed(fields) => {
            let fields = write_unnamed_borrow_fields(fields);
            let val: ExprCall = parse_quote!(#struct_name(#fields));
            Expr::Call(val)
        },
        Fields::Unit => unreachable!(),
    }
}

fn write_named_borrow_fields(fields: &FieldsNamed) -> Punctuated<FieldValue, Comma> {
    fields
        .named
        .pairs()
        .map(|p| {
            let field = p.value();
            let name = field.ident.as_ref().unwrap();
            let assigned: FieldValue = parse_quote!(#name: &self.name);

            match p {
                Pair::Punctuated(_, comma) => Pair::Punctuated(assigned, *comma),
                Pair::End(..) => Pair::End(assigned),
            }
        })
        .collect()
}

fn write_unnamed_borrow_fields(fields: &FieldsUnnamed) -> Punctuated<Expr, Comma> {
    fields
        .unnamed
        .pairs()
        .enumerate()
        .map(|(i, p)| {
            let index = proc_macro2::Literal::u32_unsuffixed(i as u32);
            let expr: Expr = parse_quote!(&self.#index);

            match p {
                Pair::Punctuated(_, comma) => Pair::Punctuated(expr, *comma),
                Pair::End(..) => Pair::End(expr),
            }
        })
        .collect()
}

fn write_std_impls(trait_name: &Ident, owned_struct_name: &Ident) -> Vec<ItemImpl> {
    vec![
        parse_quote! {
            impl<'a> ::core::borrow::Borrow<dyn #trait_name + 'a> for #owned_struct_name {
                fn borrow(&self) -> &(dyn #trait_name + 'a) {
                    self
                }
            }
        },
        parse_quote! {
            impl<'a> ::core::cmp::PartialEq for (dyn #trait_name + 'a) {
                fn eq(&self, other: &Self) -> bool {
                    self.key().eq(&other.key())
                }
            }
        },
        parse_quote! {
            impl<'a> ::core::cmp::Eq for (dyn #trait_name + 'a) {}
        },
        parse_quote! {
            impl<'a> ::core::cmp::PartialOrd for (dyn #trait_name + 'a) {
                fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering> {
                    #[allow(clippy::non_canonical_partial_ord_impl)]
                    self.key().partial_cmp(&other.key())
                }
            }
        },
        parse_quote! {
            impl<'a> ::core::cmp::Ord for (dyn #trait_name + 'a) {
                fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                    self.key().cmp(&other.key())
                }
            }
        },
        parse_quote! {
            impl<'a> core::hash::Hash for (dyn #trait_name + 'a) {
                fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                    self.key().hash(state)
                }
            }
        },
    ]
}
