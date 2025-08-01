use syn::{Ident, parse};
use syn::parse::{Result, Error};
use syn::spanned::Spanned;

use macrospace::generate_item_macro;
use macrospace_autotransform_core::Autotransform;

fn try_define_autotransform_impl (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let mut autotransform: Autotransform = parse (input)?;

	autotransform . validate ()?;

	let tokens = generate_item_macro
	(
		&autotransform . ident,
		&Ident::new ("autotransform", autotransform . autotransform_token . span ()),
		&autotransform . vis,
		&autotransform
	);

	Ok (tokens)
}

pub fn define_autotransform_impl (input: proc_macro::TokenStream)
-> proc_macro::TokenStream
{
	try_define_autotransform_impl (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}
