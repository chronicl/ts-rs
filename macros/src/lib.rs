#![macro_use]
#![deny(unused)]

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse_quote, spanned::Spanned, ConstParam, GenericParam, Generics, Item, LifetimeDef, Result,
    TypeParam, WhereClause,
};

use crate::deps::Dependencies;

#[macro_use]
mod utils;
mod attr;
mod deps;
mod types;

struct DerivedTS {
    name: String,
    inline: TokenStream,
    decl: TokenStream,
    inline_flattened: Option<TokenStream>,
    dependencies: Dependencies,

    export: bool,
    export_to: Option<String>,
}

impl DerivedTS {
    fn generate_export_test(&self, rust_ty: &Ident, generics: &Generics) -> Option<TokenStream> {
        let test_fn = format_ident!("export_bindings_{}", &self.name.to_lowercase());
        let generic_params = generics
            .params
            .iter()
            .filter(|param| matches!(param, GenericParam::Type(_)))
            .map(|_| quote! { () });
        let ty = quote!(<#rust_ty<#(#generic_params),*> as ts_rs::TS>);

        Some(quote! {
            #[cfg(test)]
            #[test]
            fn #test_fn() {
                #ty::export().expect("could not export type");
            }
        })
    }

    fn into_impl(self, rust_ty: Ident, generics: Generics) -> TokenStream {
        let export_to = match &self.export_to {
            Some(dirname) if dirname.ends_with('/') => {
                format!("{}{}.ts", dirname, self.name)
            }
            Some(filename) => filename.clone(),
            None => {
                format!("bindings/{}.ts", self.name)
            }
        };

        let export = match self.export {
            true => Some(self.generate_export_test(&rust_ty, &generics)),
            false => None,
        };

        let DerivedTS {
            name,
            inline,
            decl,
            inline_flattened,
            mut dependencies,
            ..
        } = self;
        let inline_flattened = inline_flattened
            .map(|t| {
                quote! {
                    fn inline_flattened() -> String {
                        #t
                    }
                }
            })
            .unwrap_or_else(TokenStream::new);

        // generating function body of `fn generics() -> Option<String>`
        let fn_generics = if !generics.params.is_empty() {
            let format_values = generics.params.iter().flat_map(|param| {
                if let GenericParam::Type(TypeParam { ident, .. }) = param {
                    Some(quote!(#ident::name_with_generics()))
                } else {
                    None
                }
            });

            quote! {
               fn generics() -> Option<String> {
                   Some(format!("{}", vec![#(#format_values),*].join(", ")))
               }
            }
        } else {
            quote!()
        };

        // Currently specified generics are sometimes added to the dependencies,
        // sometimes not, depending on the rust type. This is inconsistent and
        // should be fixed. However as a workaround, we add all generics as
        // dependencies here. This leads to duplicate dependencies (which can
        // happen even without this), so be careful.
        // Todo: Fix this.
        for param in &generics.params {
            if let GenericParam::Type(TypeParam { ident, .. }) = param {
                dependencies.add_generic(ident);
            }
        }

        let id = rand::random::<u64>();
        let dependencies = dependencies.0;
        let impl_start = generate_impl(&rust_ty, &generics);
        quote! {
            #impl_start {
                const EXPORT_TO: Option<&'static str> = Some(#export_to);

                fn id() -> ts_rs::Id {
                    #id
                }

                fn decl() -> Option<String> {
                    Some(#decl)
                }
                fn name() -> String {
                    #name.to_owned()
                }
                #fn_generics
                fn inline() -> String {
                    #inline
                }
                #inline_flattened
                fn dependencies_inner(dependencies: &mut ts_rs::Dependencies) {
                    #(dependencies.add::<#dependencies>();)*
                }
                fn transparent() -> bool {
                    false
                }
            }

            #export
        }
    }
}

// Todo: use crate_name instead of hardcoding the crate name

// generate start of the `impl TS for #ty` block, up to (excluding) the open brace
fn generate_impl(ty: &Ident, generics: &Generics) -> TokenStream {
    use GenericParam::*;

    let bounds = generics.params.iter().map(|param| match param {
        Type(TypeParam {
            ident,
            colon_token,
            bounds,
            ..
        }) => quote!(#ident #colon_token #bounds),
        Lifetime(LifetimeDef {
            lifetime,
            colon_token,
            bounds,
            ..
        }) => quote!(#lifetime #colon_token #bounds),
        Const(ConstParam {
            const_token,
            ident,
            colon_token,
            ty,
            ..
        }) => quote!(#const_token #ident #colon_token #ty),
    });
    let type_args = generics.params.iter().map(|param| match param {
        Type(TypeParam { ident, .. }) | Const(ConstParam { ident, .. }) => quote!(#ident),
        Lifetime(LifetimeDef { lifetime, .. }) => quote!(#lifetime),
    });

    let where_bound = add_ts_to_where_clause(generics);
    quote!(impl <#(#bounds),*> ts_rs::TS for #ty <#(#type_args),*> #where_bound)
}

fn add_ts_to_where_clause(generics: &Generics) -> Option<WhereClause> {
    let generic_types = generics
        .params
        .iter()
        .filter_map(|gp| match gp {
            GenericParam::Type(ty) => Some(ty.ident.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    if generic_types.is_empty() {
        return generics.where_clause.clone();
    }
    match generics.where_clause {
        None => Some(parse_quote! { where #( #generic_types : ts_rs::TS ),* }),
        Some(ref w) => {
            let bounds = w.predicates.iter();
            Some(parse_quote! { where #(#bounds,)* #( #generic_types : ts_rs::TS ),* })
        }
    }
}

/// Derives [TS](./trait.TS.html) for a struct or enum.
/// Please take a look at [TS](./trait.TS.html) for documentation.
#[proc_macro_derive(TS, attributes(ts))]
pub fn typescript(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match entry(input) {
        Err(err) => err.to_compile_error(),
        Ok(result) => result,
    }
    .into()
}

fn entry(input: proc_macro::TokenStream) -> Result<TokenStream> {
    let input = syn::parse::<Item>(input)?;
    let (ts, ident, generics) = match input {
        Item::Struct(s) => (types::struct_def(&s)?, s.ident, s.generics),
        Item::Enum(e) => (types::enum_def(&e)?, e.ident, e.generics),
        _ => syn_err!(input.span(); "unsupported item"),
    };

    Ok(ts.into_impl(ident, generics))
}
