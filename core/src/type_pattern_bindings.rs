use std::fmt::{Display, Formatter};

use macrospace::pattern::StructuredBindings;
use syn::{Lifetime, Type, Expr};
use syn_derive::ToTokens;
use quote::ToTokens;

use crate::TransformBindingType;

#[derive (Clone, Debug, ToTokens, PartialEq, Eq)]
pub enum TypePatternBinding
{
	Type (Type),
	Lifetime (Lifetime),
	Const (Expr),
	InnerType (Type)
}

impl TypePatternBinding
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
				TransformBindingType::Const (Default::default ()),
			Self::InnerType (_) =>
				TransformBindingType::InnerType (Default::default ())
		}
	}
}

impl Display for TypePatternBinding
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		Display::fmt (&self . to_token_stream (), f)
	}
}

pub type TypePatternBindings = StructuredBindings <TypePatternBinding>;
