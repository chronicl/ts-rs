use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Type};

#[derive(Default)]
pub struct Dependencies(pub Vec<TokenStream>);

impl Dependencies {
    /// Adds all dependencies from the given type
    pub fn add_type(&mut self, ty: &Type) {
        self.0.push(quote!(#ty));
    }

    pub fn add_generic(&mut self, generic: &Ident) {
        self.0.push(quote!(#generic));
    }

    pub fn extend(&mut self, other: Dependencies) {
        self.0.extend(other.0);
    }
}
