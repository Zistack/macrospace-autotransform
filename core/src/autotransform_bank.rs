use proc_macro2::TokenStream;

use syn::{Lifetime, Token};
use syn::parse::{Parse, ParseStream};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::{
	Autotransform,
	TransformDirection,
	Forward,
	Backward,
	ApplicationError
};

use crate::translation::GetTranslation;

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
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where D: TransformDirection
	{
		for transform in &self . transforms
		{
			match transform . try_apply::<D, _>
			(
				ty_tokens . clone (),
				|ty_tokens| self . try_apply::<D> (ty_tokens)
			)?
			{
				Some ((transformed_ty, transformed_closure)) =>
					return Ok (Some ((transformed_ty, transformed_closure))),
				None => continue
			}
		}

		Ok (None)
	}

	pub fn try_apply_forward (&self, ty_tokens: TokenStream)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	{
		self . try_apply::<Forward> (ty_tokens)
	}

	pub fn try_apply_backward (&self, ty_tokens: TokenStream)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	{
		self . try_apply::<Backward> (ty_tokens)
	}

	pub fn try_apply_with_translator <D, T>
	(
		&self,
		ty_tokens: TokenStream,
		translator: T
	)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where
		D: TransformDirection,
		T: Copy + GetTranslation
	{
		let preprocessed_ty_tokens =
			match translator . get_translation (ty_tokens . clone ())
		{
			Some (translated_ty) => translated_ty . to_token_stream (),
			None => ty_tokens . clone ()
		};

		for transform in &self . transforms
		{
			match transform . try_apply::<D, _>
			(
				preprocessed_ty_tokens . clone (),
				|ty_tokens|
				self . try_apply_with_translator::<D, T> (ty_tokens, translator)
			)?
			{
				Some ((transformed_ty, transformed_closure)) =>
					return Ok (Some ((transformed_ty, transformed_closure))),
				None => continue
			}
		}

		Ok (None)
	}

	pub fn try_apply_forward_with_translator <T>
	(
		&self,
		ty_tokens: TokenStream,
		translator: T
	)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where T: Copy + GetTranslation
	{
		self . try_apply_with_translator::<Forward, T> (ty_tokens, translator)
	}

	pub fn try_apply_backward_with_translator <T>
	(
		&self,
		ty_tokens: TokenStream,
		translator: T
	)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where T: Copy + GetTranslation
	{
		self . try_apply_with_translator::<Backward, T> (ty_tokens, translator)
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
