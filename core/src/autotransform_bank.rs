use proc_macro2::TokenStream;

use syn::parse::{Parse, ParseStream};
use quote::ToTokens;

use crate::{Autotransform, TransformDirection, Forward, Backward, ApplicationError};

#[derive (Clone, Debug)]
pub struct AutotransformBank
{
	transforms: Vec <Autotransform>
}

impl AutotransformBank
{
	pub fn try_apply <D> (&self, ty_tokens: TokenStream)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	where D: TransformDirection
	{
		for transform in &self . transforms
		{
			match transform . try_apply::<D, _>
			(
				ty_tokens . clone (),
				|ty_tokens| self . try_apply::<D> (ty_tokens)
			)
			{
				Ok ((transformed_ty, transformed_closure)) =>
					return Ok ((transformed_ty, transformed_closure)),
				e @ Err (ApplicationError::Substitute (_)) => return e,
				_ => continue
			}
		}

		Err
		(
			ApplicationError::Match
			(
				syn::parse::Error::new_spanned
				(
					ty_tokens,
					"type does not match any autotransform pattern"
				)
			)
		)
	}

	pub fn try_apply_forward (&self, ty_tokens: TokenStream)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	{
		self . try_apply::<Forward> (ty_tokens)
	}

	pub fn try_apply_backward (&self, ty_tokens: TokenStream)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	{
		self . try_apply::<Backward> (ty_tokens)
	}
}

impl Parse for AutotransformBank
{
	fn parse (input: ParseStream <'_>) -> syn::Result <Self>
	{
		let mut transforms = Vec::new ();

		while let Some (autotransform) = Autotransform::try_parse (input)?
		{
			transforms . push (autotransform);
		}

		Ok (Self {transforms})
	}
}

impl ToTokens for AutotransformBank
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		for transform in &self . transforms
		{
			transform . to_tokens (tokens)
		}
	}
}
