use std::fmt::{Debug, Display, Formatter};

use macrospace::pattern::StructuredBindings;
use syn::{Type, Lifetime, Expr};
use syn::parse::{Parse, ParseStream};
use syn::parse::discouraged::Speculative;
use syn_derive::ToTokens;
use quote::ToTokens;

use crate::TransformBindingType;

#[derive (Clone, Debug, ToTokens, PartialEq, Eq)]
pub enum SpecializationBinding
{
	Type (Type),
	Lifetime (Lifetime),
	Const (Expr)
}

impl Parse for SpecializationBinding
{
	fn parse (input: ParseStream <'_>) -> syn::Result <Self>
	{
		if input . peek (Lifetime)
		{
			return Ok (Self::Lifetime (input . parse ()?));
		}

		let speculative = input . fork ();

		if let Ok (ty) = speculative . parse ()
		{
			input . advance_to (&speculative);

			return Ok (Self::Type (ty));
		}

		Ok (Self::Const (input . parse ()?))
	}
}

impl SpecializationBinding
{
	pub fn ty (&self) -> TransformBindingType
	{
		match self
		{
			Self::Type (_) =>
				TransformBindingType::Type (Default::default ()),
			Self::Lifetime (_) =>
				TransformBindingType::Lifetime (Default::default ()),
			Self::Const (_) =>
				TransformBindingType::Const (Default::default ())
		}
	}
}

impl Display for SpecializationBinding
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		Display::fmt (&self . to_token_stream (), f)
	}
}

pub type SpecializationBindings = StructuredBindings <SpecializationBinding>;
