use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	ParseBinding,
	TokenizeBinding,
	ParameterBindingTypeMismatch,
	Pattern,
	TypeAnnotation
};
use proc_macro2::TokenStream;
use syn::{Ident, Token};
use syn::parse::ParseStream;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::{TypePatternBinding, SpecializationBinding};

mod kw
{
	syn::custom_keyword! (opt_lifetime);
	syn::custom_keyword! (lifetime);
	syn::custom_keyword! (inner_type);
}

#[derive (Copy, Clone, Debug, PartialEq, Eq, Parse, ToTokens)]
pub enum TransformBindingType
{
	#[parse (peek = Token! [type])]
	Type (Token! [type]),
	#[parse (peek = kw::lifetime)]
	Lifetime (kw::lifetime),
	#[parse (peek = Token! [const])]
	Const (Token! [const]),
	#[parse (peek = kw::inner_type)]
	InnerType (kw::inner_type)
}

impl Display for TransformBindingType
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		match self
		{
			Self::Type (_) => f . write_str ("type"),
			Self::Lifetime (_) => f . write_str ("lifetime"),
			Self::Const (_) => f . write_str ("const"),
			Self::InnerType (_) => f . write_str ("inner_type")
		}
	}
}

impl ParseBinding <TypePatternBinding> for TransformBindingType
{
	fn parse (&self, input: ParseStream <'_>)
	-> syn::Result <TypePatternBinding>
	{
		match self
		{
			Self::Type (_) =>
				Ok (TypePatternBinding::Type (input . parse ()?)),
			Self::Lifetime (_) =>
				Ok (TypePatternBinding::Lifetime (input . parse ()?)),
			Self::Const (_) =>
				Ok (TypePatternBinding::Const (input . parse ()?)),
			Self::InnerType (_) =>
				Ok (TypePatternBinding::InnerType (input . parse ()?))
		}
	}
}

impl TokenizeBinding <TypePatternBinding> for TransformBindingType
{
	type Error = ParameterBindingTypeMismatch <TypePatternBinding, Self>;

	fn tokenize
	(
		&self,
		ident: &Ident,
		binding: &TypePatternBinding,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		if binding . ty () != *self
		{
			return Err
			(
				ParameterBindingTypeMismatch::new
				(
					ident . clone (),
					binding . clone (),
					binding . ty (),
					self . clone ()
				)
			)
		}

		binding . to_tokens (tokens);

		Ok (())
	}
}

impl TokenizeBinding <SpecializationBinding> for TransformBindingType
{
	type Error = ParameterBindingTypeMismatch <SpecializationBinding, Self>;

	fn tokenize
	(
		&self,
		ident: &Ident,
		binding: &SpecializationBinding,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match (self, binding)
		{
			(TransformBindingType::Type (_), SpecializationBinding::Type (ty)) =>
				Ok (ty . to_tokens (tokens)),
			(TransformBindingType::Lifetime (_), SpecializationBinding::Lifetime (lifetime)) =>
				Ok (lifetime . to_tokens (tokens)),
			(TransformBindingType::Const (_), SpecializationBinding::Const (expr)) =>
				Ok (expr . to_tokens (tokens)),
			(TransformBindingType::InnerType (_), SpecializationBinding::Type (ty)) =>
				Ok (ty . to_tokens (tokens)),
			_ => Err
			(
				ParameterBindingTypeMismatch::new
				(
					ident . clone (),
					binding . clone (),
					binding . ty (),
					self . clone ()
				)
			)
		}
	}
}

pub type TypePattern = Pattern <TypeAnnotation <TransformBindingType>>;
