use proc_macro2::TokenStream;

use syn::{Lifetime, Type, Token, parse2};
use syn::parse::{Parse, ParseStream};
use syn_derive::{Parse, ToTokens};
use quote::{ToTokens, quote};

use crate::{
	Autotransform,
	TransformDirection,
	Forward,
	Backward,
	ApplicationError
};

#[derive (Clone, Debug, Parse, ToTokens)]
struct OwnedSelf
{
	self_token: Token! [Self],
}

#[derive (Clone, Debug, Parse, ToTokens)]
struct RefSelf
{
	ampersand_token: Token! [&],
	lifetime: Option <Lifetime>,
	self_token: Token! [Self]
}

#[derive (Clone, Debug, Parse, ToTokens)]
struct RefMutSelf
{
	ampersand_token: Token! [&],
	lifetime: Option <Lifetime>,
	mut_token: Token! [mut],
	self_token: Token! [Self]
}

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

	pub fn try_apply_with_receiver <D>
	(
		&self,
		ty_tokens: TokenStream,
		receiver_ty: &Type
	)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	where D: TransformDirection
	{
		let mut preprocessed_ty_tokens = ty_tokens . clone ();

		if let Ok (OwnedSelf {..}) = parse2 (ty_tokens . clone ())
		{
			preprocessed_ty_tokens = receiver_ty . to_token_stream ();
		}

		if let Ok (RefSelf {ampersand_token, lifetime, ..}) =
			parse2 (ty_tokens . clone ())
		{
			preprocessed_ty_tokens = quote!
			(
				#ampersand_token #lifetime #receiver_ty
			);
		}

		if let Ok (RefMutSelf {ampersand_token, lifetime, mut_token, ..}) =
			parse2 (ty_tokens . clone ())
		{
			preprocessed_ty_tokens = quote!
			(
				#ampersand_token #lifetime #mut_token #receiver_ty
			);
		}

		for transform in &self . transforms
		{
			match transform . try_apply::<D, _>
			(
				preprocessed_ty_tokens . clone (),
				|ty_tokens|
				self . try_apply_with_receiver::<D> (ty_tokens, receiver_ty)
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

	pub fn try_apply_forward_with_receiver
	(
		&self,
		ty_tokens: TokenStream,
		receiver_ty: &Type
	)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	{
		self . try_apply_with_receiver::<Forward> (ty_tokens, receiver_ty)
	}

	pub fn try_apply_backward_with_receiver
	(
		&self,
		ty_tokens: TokenStream,
		receiver_ty: &Type
	)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	{
		self . try_apply_with_receiver::<Backward> (ty_tokens, receiver_ty)
	}
}

impl From <Vec <Autotransform>> for AutotransformBank
{
	fn from (transforms: Vec <Autotransform>) -> Self
	{
		Self {transforms}
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
