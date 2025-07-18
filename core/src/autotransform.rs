use std::error::Error;
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	Pattern,
	TypedParameter,
	UntypedParameter,
	SubstitutionError
};
use proc_macro2::TokenStream;
use syn::{Visibility, Ident, Token, parse2};
use syn::parse::ParseStream;
use syn::token::{Brace, Bracket};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::bindings::{
	AutotransformBindings,
	AutotransformBindingType,
	ClosureBindings
};

mod kw
{
	syn::custom_keyword! (autotransform);
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct Autotransform
{
	pub vis: Visibility,
	pub autotransform_token: kw::autotransform,
	pub ident: Ident,

	#[syn (bracketed)]
	pub from_brackets: Bracket,
	#[syn (in = from_brackets)]
	pub from_type: Pattern <TypedParameter <AutotransformBindingType>>,

	pub arrow_token: Token! [->],

	#[syn (bracketed)]
	pub to_brackets: Bracket,
	#[syn (in = to_brackets)]
	pub to_type: Pattern <TypedParameter <AutotransformBindingType>>,

	#[syn (braced)]
	pub transform_braces: Brace,
	#[syn (in = transform_braces)]
	pub transform_closure: Pattern <UntypedParameter>,
}

impl Autotransform
{
	/*
	fn validate () -> Result <(), >
	{
		// We might need a new visitor for this one.
	}
	*/

	pub fn try_parse (input: ParseStream) -> syn::Result <Option <Self>>
	{
		let speculative = input . fork ();

		if speculative . parse::<Visibility> () . is_err ()
		{
			return Ok (None);
		}
		if speculative . parse::<kw::autotransform> () . is_err ()
		{
			return Ok (None);
		}

		Ok (Some (input . parse ()?))
	}

	pub fn try_apply <D, F> (&self, ty_tokens: TokenStream, mut apply_inner: F)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where
		D: TransformDirection,
		F: FnMut (TokenStream)
			-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	{
		let mut autotransform_bindings: AutotransformBindings =
			match D::from_type (self) . match_tokens (ty_tokens)
			{
				Ok (bindings) => bindings,
				Err (_)=> return Ok (None)
			};

		let mut closure_bindings = ClosureBindings::new ();

		for (transformable_ident, transformable_ty)
		in autotransform_bindings . inner_types_mut ()
		{
			match apply_inner (transformable_ty . to_token_stream ())?
			{
				Some ((transformed_ty, transformed_closure)) =>
				{
					*transformable_ty = parse2 (transformed_ty)?;

					closure_bindings . insert
					(
						transformable_ident . clone (),
						parse2 (transformed_closure)?
					);
				},
				None => continue
			}
		}

		let transformed_ty =
			D::to_type (self) . substitute (autotransform_bindings)?;

		let transformed_closure = self
			. transform_closure
			. substitute (closure_bindings)
			// This process is infallible.
			. unwrap ();

		Ok (Some ((transformed_ty, transformed_closure)))
	}
}

pub trait TransformDirection
{
	fn from_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>;

	fn to_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>;
}

pub struct Forward;

impl TransformDirection for Forward
{
	fn from_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>
	{
		&autotransform . from_type
	}

	fn to_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>
	{
		&autotransform . to_type
	}
}

pub struct Backward;

impl TransformDirection for Backward
{
	fn from_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>
	{
		&autotransform . to_type
	}

	fn to_type (autotransform: &Autotransform)
	-> &Pattern <TypedParameter <AutotransformBindingType>>
	{
		&autotransform . from_type
	}
}

#[derive (Clone, Debug)]
pub enum ApplicationError
{
	Transform (syn::parse::Error),
	Substitute (SubstitutionError <AutotransformBindingType>)
}

impl From <syn::parse::Error> for ApplicationError
{
	fn from (m: syn::parse::Error) -> Self
	{
		Self::Transform (m)
	}
}

impl From <SubstitutionError <AutotransformBindingType>> for ApplicationError
{
	fn from (s: SubstitutionError <AutotransformBindingType>) -> Self
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
			Self::Transform (m) => Display::fmt (m, f),
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
			Self::Transform (m) => m,
			Self::Substitute (s) => s . into ()
		}
	}
}
