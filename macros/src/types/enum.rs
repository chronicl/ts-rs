use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{Fields, Generics, ItemEnum, Variant};

use crate::{
    attr::{EnumAttr, FieldAttr, StructAttr, Tagged, VariantAttr},
    deps::Dependencies,
    types,
    types::generics::{format_generics, format_type},
    DerivedTS,
};

pub(crate) fn r#enum_def(s: &ItemEnum) -> syn::Result<DerivedTS> {
    let enum_attr: EnumAttr = EnumAttr::from_attrs(&s.attrs)?;

    let name = match &enum_attr.rename {
        Some(existing) => existing.clone(),
        None => s.ident.to_string(),
    };

    if s.variants.is_empty() {
        return Ok(empty_enum(name, enum_attr));
    }

    let is_enum = match enum_attr.r#type.as_deref() {
        Some("enum" | "const enum") => true,
        None | Some("type") => false,
        Some(x) => {
            syn_err!(
                "Either `const enum`, `enum` or `type` accepted; was: {:?}",
                x
            );
        }
    };

    let mut formatted_variants = vec![];
    let mut dependencies = Dependencies::default();
    if is_enum {
        let any_renamed = enum_attr.rename_all.is_some()
            || s.variants.iter().any(|v| {
                let FieldAttr { rename, .. } = FieldAttr::from_attrs(&v.attrs).unwrap();
                rename.is_some()
            });

        for variant in &s.variants {
            format_enum_variant(&mut formatted_variants, &enum_attr, variant, any_renamed)?;
        }
    } else {
        for variant in &s.variants {
            format_variant(
                &mut formatted_variants,
                &mut dependencies,
                &enum_attr,
                variant,
                &s.generics,
            )?;
        }
    }

    let inline = if is_enum {
        quote!([#(#formatted_variants),*].join(", "))
    } else {
        quote!([#(#formatted_variants),*].join(" | "))
    };

    let overwrite_type = enum_attr.r#type.unwrap_or(String::from("type"));

    let generic_args = format_generics(&mut dependencies, &s.generics);

    let decl = if is_enum {
        quote!(format!("{} {} {{ {} }}", #overwrite_type, #name, Self::inline()))
    } else {
        quote!(format!("{} {}{} = {};", #overwrite_type, #name, #generic_args, Self::inline()))
    };

    Ok(DerivedTS {
        inline,
        decl,
        inline_flattened: None,
        dependencies,
        name,
        export: enum_attr.export,
        export_to: enum_attr.export_to,
    })
}

fn format_variant(
    formatted_variants: &mut Vec<TokenStream>,
    dependencies: &mut Dependencies,
    enum_attr: &EnumAttr,
    variant: &Variant,
    generics: &Generics,
) -> syn::Result<()> {
    let variant_attr = VariantAttr::from_attrs(&variant.attrs)?;

    if variant_attr.skip {
        return Ok(());
    }

    let name = match (variant_attr.rename.clone(), &enum_attr.rename_all) {
        (Some(rn), _) => rn,
        (None, None) => variant.ident.to_string(),
        (None, Some(rn)) => rn.apply(&variant.ident.to_string()),
    };

    let variant_type = types::type_def(
        &StructAttr::from(variant_attr),
        // since we are generating the variant as a struct, it doesn't have a name
        &format_ident!("_"),
        &variant.fields,
        generics,
    )?;
    let variant_dependencies = variant_type.dependencies;
    let inline_type = variant_type.inline;

    let formatted = match enum_attr.tagged()? {
        Tagged::Untagged => quote!(#inline_type),
        Tagged::Externally => match &variant.fields {
            Fields::Unit => quote!(format!("\"{}\"", #name)),
            _ => quote!(format!("{{ {}: {} }}", #name, #inline_type)),
        },
        Tagged::Adjacently { tag, content } => match &variant.fields {
            Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                let ty = format_type(&unnamed.unnamed[0].ty, dependencies, generics);
                quote!(format!("{{ {}: \"{}\", {}: {} }}", #tag, #name, #content, #ty))
            }
            Fields::Unit => quote!(format!("{{ {}: \"{}\" }}", #tag, #name)),
            _ => quote!(format!("{{ {}: \"{}\", {}: {} }}", #tag, #name, #content, #inline_type)),
        },
        Tagged::Internally { tag } => match variant_type.inline_flattened {
            Some(inline_flattened) => quote! {
                format!(
                    "{{ {}: \"{}\", {} }}",
                    #tag,
                    #name,
                    #inline_flattened
                )
            },
            None => match &variant.fields {
                Fields::Unnamed(unnamed) if unnamed.unnamed.len() == 1 => {
                    let ty = format_type(&unnamed.unnamed[0].ty, dependencies, generics);
                    quote!(format!("{{ {}: \"{}\" }} & {}", #tag, #name, #ty))
                }
                Fields::Unit => quote!(format!("{{ {}: \"{}\" }}", #tag, #name)),
                _ => {
                    quote!(format!("{{ {}: \"{}\" }} & {}", #tag, #name, #inline_type))
                }
            },
        },
    };

    dependencies.extend(variant_dependencies);
    formatted_variants.push(formatted);
    Ok(())
}

// bindings for an empty enum (`never` in TS)
fn empty_enum(name: impl Into<String>, enum_attr: EnumAttr) -> DerivedTS {
    let name = name.into();
    DerivedTS {
        inline: quote!("never".to_owned()),
        decl: quote!(format!("type {} = never;", #name)),
        name,
        inline_flattened: None,
        dependencies: Dependencies::default(),
        export: enum_attr.export,
        export_to: enum_attr.export_to,
    }
}

/// If any have been renamed then we want to rename all enum variants
fn format_enum_variant(
    formatted_variants: &mut Vec<TokenStream>,
    enum_attr: &EnumAttr,
    variant: &Variant,
    any_renamed: bool,
) -> syn::Result<()> {
    let VariantAttr {
        rename,
        inline,
        skip,
        ..
    } = VariantAttr::from_attrs(&variant.attrs)?;

    match (skip, inline) {
        (true, ..) => return Ok(()),
        (_, true) => syn_err!("`inline` is not applicable to enum variants when type enum"),
        _ => {}
    };

    let name = variant.ident.to_string();
    let renamed = match (rename, enum_attr.rename_all, any_renamed) {
        (Some(rn), _, _) => Some(rn),
        (_, Some(rn), _) => Some(rn.apply(&variant.ident.to_string())),
        (_, _, true) => Some(name.to_owned()),
        (_, _, false) => None,
    };

    for (forbidden_attr_name, forbidden_attr_val) in [
        ("tag", &enum_attr.tag.as_deref()),
        ("content", &enum_attr.content.as_deref()),
        ("untagged", &enum_attr.untagged.then_some("true")),
    ] {
        if forbidden_attr_val.is_some() {
            syn_err!(
                "Invalid enum attribute {:?} when type is enum.",
                forbidden_attr_name
            )
        }
    }

    formatted_variants.push(if let Some((_, expr)) = &variant.discriminant {
        let str = format!("{} = {}", name, expr.to_token_stream());
        quote!(#str)
    } else if let Some(renamed) = renamed {
        if let Some((_, e)) = &variant.discriminant {
            if !any_renamed {
                syn_err!(
                    "{:?} Can't be both renamed and have a discriminant {:?}",
                    name,
                    e.to_token_stream()
                );
            }
        }
        let str = format!("{name} = \"{renamed}\"");
        quote!(#str)
    } else {
        quote!(#name)
    });
    Ok(())
}
