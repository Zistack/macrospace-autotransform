use macrospace::pattern::{DummyTokens, TypedParameter};
use proc_macro2::TokenStream;
use syn::Lifetime;
use quote::{ToTokens, quote};

use super::{
	AutotransformBindingType,
	ArgParameter,
	SubvalueParameter,
	ExprParameter
};

pub struct ValidationTokens;

impl DummyTokens <TypedParameter <AutotransformBindingType>>
for ValidationTokens
{
	fn dummy_tokens (p: &TypedParameter <AutotransformBindingType>)
	-> TokenStream
	{
		let ident = &p . ident;
		let lifetime = Lifetime::new (&ident . to_string (), ident . span ());

		match p . ty
		{
			AutotransformBindingType::OptionalLifetime (_) => quote! (#lifetime),
			AutotransformBindingType::Type (_) => quote! (#ident),
			AutotransformBindingType::Lifetime (_) => quote! (#lifetime),
			AutotransformBindingType::Const (_) => quote! (#ident),
			AutotransformBindingType::InnerType (_) => quote! (#ident)
		}
	}
}

impl DummyTokens <SubvalueParameter> for ValidationTokens
{
	fn dummy_tokens (p: &SubvalueParameter) -> TokenStream
	{
		p . to_token_stream ()
	}
}

impl DummyTokens <ArgParameter> for ValidationTokens
{
	fn dummy_tokens (p: &ArgParameter) -> TokenStream
	{
		p . arg_token . to_token_stream ()
	}
}

impl DummyTokens <ExprParameter> for ValidationTokens
{
	fn dummy_tokens (p: &ExprParameter) -> TokenStream
	{
		match p
		{
			ExprParameter::Arg (arg_p) => Self::dummy_tokens (arg_p),
			ExprParameter::Subvalue (subvalue_p) =>
				Self::dummy_tokens (subvalue_p)
		}
	}
}
