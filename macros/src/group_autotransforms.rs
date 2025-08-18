use macrospace::{
	generate_macrospace_invokation,
	parse_args,
	generate_item_macro
};
use syn::{Visibility, Ident, Path, Token, parse, parse_quote};
use syn::parse::{Result, Error};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use macrospace_autotransform_core::{AutotransformGroup, AutotransformInputs, kw};

#[derive (Clone, Debug, Parse, ToTokens)]
struct GroupAutotransformsHeader
{
	vis: Visibility,
	autotransform_token: kw::autotransform,
	group_ident: Ident
}

#[derive (Clone, Debug, Parse, ToTokens)]
struct GroupAutotransformsInput
{
	header: GroupAutotransformsHeader,
	#[syn (bracketed)]
	bracket_token: syn::token::Bracket,
	#[syn (in = bracket_token)]
	#[parse (Punctuated::parse_terminated)]
	autotransform_paths: Punctuated <Path, Token! [,]>
}

fn group_autotransforms
(
	header: GroupAutotransformsHeader,
	autotransform_paths: Punctuated <Path, Token! [,]>
)
-> proc_macro2::TokenStream
{
	generate_macrospace_invokation
	(
		parse_quote! (macrospace_autotransform::__group_autotransforms_inner__),
		autotransform_paths
			. into_iter ()
			. map (|path| parse_quote! (#path: autotransform)),
		header . into_token_stream ()
	)
}

fn try_group_autotransforms (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let GroupAutotransformsInput
	{
		header,
		autotransform_paths,
		..
	}
		= parse (input)?;

	Ok (group_autotransforms (header, autotransform_paths))
}

pub fn group_autotransforms_impl (input: proc_macro::TokenStream)
-> proc_macro::TokenStream
{
	try_group_autotransforms (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}

fn group_autotransforms_inner
(
	vis: Visibility,
	autotransform_token: kw::autotransform,
	group_ident: Ident,
	autotransforms: AutotransformInputs
)
-> proc_macro2::TokenStream
{
	let autotransform_group = AutotransformGroup
	{
		vis: vis . clone (),
		autotransform_token,
		ident: group_ident . clone (),
		brace_token: Default::default (),
		autotransforms: autotransforms . into_flattened ()
	};

	generate_item_macro
	(
		&group_ident,
		&Ident::new ("autotransform", autotransform_token . span ()),
		&vis,
		&autotransform_group
	)
}

fn try_group_autotransforms_inner (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let
	(
		autotransforms,
		GroupAutotransformsHeader {vis, autotransform_token, group_ident}
	) = parse_args! (1, input)?;

	Ok
	(
		group_autotransforms_inner
		(
			vis,
			autotransform_token,
			group_ident,
			autotransforms
		)
	)
}

pub fn group_autotransforms_inner_impl (input: proc_macro::TokenStream)
-> proc_macro::TokenStream
{
	try_group_autotransforms_inner (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}
