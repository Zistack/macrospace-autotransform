use std::error::Error;
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	Pattern,
	TypedParameter,
	UntypedParameter,
	ParameterNotFound
};
use proc_macro2::TokenStream;
use syn::{Token, parse2};
use syn::token::{Brace, Bracket};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::autotransform_bindings::{
	AutotransformBindings,
	AutotransformBindingType
};
use crate::closure_bindings::ClosureBindings;

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct Autotransform
{
	#[syn (braced)]
	transform_braces: Brace,
	#[syn (in = transform_braces)]
	transform_closure: Pattern <UntypedParameter>,
	colon_token: Token! [:],

	#[syn (bracketed)]
	from_brackets: Bracket,
	#[syn (in = from_brackets)]
	from_type: Pattern <TypedParameter <AutotransformBindingType>>,

	arrow_token: Token! [->],

	#[syn (bracketed)]
	to_brackets: Bracket,
	#[syn (in = to_brackets)]
	to_type: Pattern <UntypedParameter>,
}

impl Autotransform
{
	/*
	fn validate () -> Result <(), >
	{
		// We might need a new visitor for this one.
	}
	*/

	pub fn try_apply <F> (&self, ty_tokens: TokenStream, mut apply_inner: F)
	-> Result <(TokenStream, TokenStream), ApplicationError>
	where F: FnMut (TokenStream) -> Result <(TokenStream, TokenStream), ApplicationError>
	{
		let mut autotransform_bindings: AutotransformBindings =
			self . from_type . match_tokens (ty_tokens)?;

		let mut closure_bindings = ClosureBindings::new ();

		for (transformable_ident, transformable_ty)
		in autotransform_bindings . inner_types_mut ()
		{
			let (transformed_ty, transformed_closure) =
				apply_inner (transformable_ty . to_token_stream ())?;

			*transformable_ty = parse2 (transformed_ty)?;

			closure_bindings . insert
			(
				transformable_ident . clone (),
				parse2 (transformed_closure)?
			);
		}

		let transformed_ty =
			self . to_type . substitute (autotransform_bindings)?;

		let transformed_closure = self
			. transform_closure
			. substitute (closure_bindings)
			// This process is infallible.
			. unwrap ();

		Ok ((transformed_ty, transformed_closure))
	}
}

#[derive (Clone, Debug)]
pub enum ApplicationError
{
	Match (syn::parse::Error),
	Substitute (ParameterNotFound <UntypedParameter>)
}

impl From <syn::parse::Error> for ApplicationError
{
	fn from (m: syn::parse::Error) -> Self
	{
		Self::Match (m)
	}
}

impl From <ParameterNotFound <UntypedParameter>> for ApplicationError
{
	fn from (s: ParameterNotFound <UntypedParameter>) -> Self
	{
		Self::Substitute (s)
	}
}

impl Display for ApplicationError
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		match self
		{
			Self::Match (m) => Display::fmt (m, f),
			Self::Substitute (s) => Display::fmt (s, f)
		}
	}
}

impl Error for ApplicationError
{
}

impl Into <syn::parse::Error> for ApplicationError
{
	fn into (self) -> syn::parse::Error
	{
		match self
		{
			Self::Match (m) => m,
			Self::Substitute (s) => s . into ()
		}
	}
}
