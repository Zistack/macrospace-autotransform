use std::error::Error;
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	TokenizeBinding,
	ParameterBindingTypeMismatch,
	Pattern
};
use proc_macro2::TokenStream;
use syn::Ident;
use syn::token::Paren;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::{TransformBindingType, ExprBinding, SpecializationBinding};

mod kw
{
	syn::custom_keyword! (arg);
}

#[derive (Clone, Debug)]
pub enum Infallible {}

impl Display for Infallible
{
	fn fmt (&self, _f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		unreachable! ();
	}
}

impl Error for Infallible
{
}

impl Into <syn::Error> for Infallible
{
	fn into (self) -> syn::Error
	{
		unreachable! ();
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct SubexprAnnotation
{
	#[syn (parenthesized)]
	pub paren_token: Paren,
	#[syn (in = paren_token)]
	pub arg_expr: TokenStream
}

impl Display for SubexprAnnotation
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		f . write_fmt
		(
			format_args! ("({})", self . arg_expr)
		)
	}
}

impl TokenizeBinding <ExprBinding> for SubexprAnnotation
{
	type Error = Infallible;

	fn tokenize
	(
		&self,
		_ident: &Ident,
		binding: &ExprBinding,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		Ok (binding . substitute_arg (&self . arg_expr, tokens))
	}
}

impl TokenizeBinding <SpecializationBinding> for SubexprAnnotation
{
	type Error = ParameterBindingTypeMismatch
	<
		SpecializationBinding,
		TransformBindingType
	>;

	fn tokenize
	(
		&self,
		ident: &Ident,
		binding: &SpecializationBinding,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		if let SpecializationBinding::Type (_) = binding
		{
			// We pass through the value unmodified.
			Ok (self . arg_expr . to_tokens (tokens))
		}
		else
		{
			Err
			(
				ParameterBindingTypeMismatch::new
				(
					ident . clone (),
					binding . clone (),
					binding . ty (),
					TransformBindingType::Type (Default::default ())
				)
			)
		}
	}
}

pub type ExprPattern = Pattern <SubexprAnnotation>;
