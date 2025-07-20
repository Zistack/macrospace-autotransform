use std::collections::hash_map::{HashMap, Entry};
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	CursorParse,
	MatchBindings,
	MergeableBindings,
	SubstitutionBindings,
	TypedParameter,
	UntypedParameter,
	ParameterBindingMismatch,
	ParameterNotFound,
	ParameterTypeMismatch,
	SubstitutionError
};
use proc_macro2::TokenStream;
use syn::{Lifetime, Type, Expr, Ident, Token};
use syn::buffer::Cursor;
use syn::parse::ParseStream;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

mod kw
{
	syn::custom_keyword! (opt_lifetime);
	syn::custom_keyword! (lifetime);
	syn::custom_keyword! (inner_type);
}

#[derive (Copy, Clone, Debug, PartialEq, Eq, Parse, ToTokens)]
pub enum AutotransformBindingType
{
	#[parse (peek = kw::opt_lifetime)]
	OptionalLifetime (kw::opt_lifetime),
	#[parse (peek = Token! [type])]
	Type (Token! [type]),
	#[parse (peek = kw::lifetime)]
	Lifetime (kw::lifetime),
	#[parse (peek = Token! [const])]
	Const (Token! [const]),
	#[parse (peek = kw::inner_type)]
	InnerType (kw::inner_type)
}

impl CursorParse for AutotransformBindingType
{
	fn parse_from_cursor (cursor: Cursor <'_>) -> Option <(Self, Cursor <'_>)>
	{
		if let Some ((type_ident, next_cursor)) = cursor . ident ()
		{
			let autotransform_binding_type = match
				type_ident . to_string () . as_str ()
			{
				"opt_lifetime" => AutotransformBindingType::OptionalLifetime
				(
					kw::opt_lifetime {span: type_ident . span ()}
				),
				"type" => AutotransformBindingType::Type
				(
					syn::token::Type {span: type_ident . span ()}
				),
				"lifetime" => AutotransformBindingType::Lifetime
				(
					kw::lifetime {span: type_ident . span ()}
				),
				"const" => AutotransformBindingType::Const
				(
					syn::token::Const {span: type_ident . span ()}
				),
				"inner_type" => AutotransformBindingType::InnerType
				(
					kw::inner_type {span: type_ident . span ()}
				),
				_ => return None
			};

			Some ((autotransform_binding_type, next_cursor))
		}
		else
		{
			None
		}
	}
}

impl Display for AutotransformBindingType
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		match self
		{
			Self::OptionalLifetime (_) => f . write_str ("opt_lifetime"),
			Self::Type (_) => f . write_str ("type"),
			Self::Lifetime (_) => f . write_str ("lifetime"),
			Self::Const (_) => f . write_str ("const"),
			Self::InnerType (_) => f . write_str ("inner_type")
		}
	}
}

#[derive (Clone, Debug, ToTokens, PartialEq, Eq)]
pub enum AutotransformBindingValue
{
	OptionalLifetime (Option <Lifetime>),
	Type (Type),
	Lifetime (Lifetime),
	Const (Expr),
	InnerType (Type)
}

impl AutotransformBindingValue
{
	fn get_type (&self) -> AutotransformBindingType
	{
		match self
		{
			Self::OptionalLifetime (_) =>
				AutotransformBindingType::OptionalLifetime (Default::default ()),
			Self::Type (_) =>
				AutotransformBindingType::Type (Default::default ()),
			Self::Lifetime (_) =>
				AutotransformBindingType::Lifetime (Default::default ()),
			Self::Const (_) =>
				AutotransformBindingType::Const (Default::default ()),
			Self::InnerType (_) =>
				AutotransformBindingType::InnerType (Default::default ())
		}
	}
}

impl Display for AutotransformBindingValue
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		Display::fmt (&self . to_token_stream (), f)
	}
}

pub struct AutotransformBindings
{
	map: HashMap <Ident, AutotransformBindingValue>
}

impl AutotransformBindings
{
	pub fn new () -> Self
	{
		Self {map: HashMap::new ()}
	}

	pub fn inner_types_mut (&mut self)
	-> impl Iterator <Item = (&Ident, &mut Type)>
	{
		self
			. map
			. iter_mut ()
			. filter_map
		(
			|(ident, value)|
			{
				match value
				{
					AutotransformBindingValue::InnerType (inner_type) =>
						Some ((&*ident, inner_type)),
					_ => None
				}
			}
		)
	}
}

impl Default for AutotransformBindings
{
	fn default () -> Self
	{
		Self::new ()
	}
}

impl MatchBindings <TypedParameter <AutotransformBindingType>>
for AutotransformBindings
{
	fn parse_parameter_binding
	(
		&mut self,
		parameter: TypedParameter <AutotransformBindingType>,
		input: ParseStream <'_>
	)
	-> syn::parse::Result <()>
	{
		let value = match parameter . ty
		{
			AutotransformBindingType::OptionalLifetime (_) =>
				AutotransformBindingValue::OptionalLifetime (input . parse ()?),
			AutotransformBindingType::Type (_) =>
				AutotransformBindingValue::Type (input . parse ()?),
			AutotransformBindingType::Lifetime (_) =>
				AutotransformBindingValue::Lifetime (input . parse ()?),
			AutotransformBindingType::Const (_) =>
				AutotransformBindingValue::Const (input . parse ()?),
			AutotransformBindingType::InnerType (_) =>
				AutotransformBindingValue::InnerType (input . parse ()?)
		};

		self . map . insert (parameter . ident, value);

		Ok (())
	}
}

impl MergeableBindings for AutotransformBindings
{
	type Error = ParameterBindingMismatch <AutotransformBindingValue>;

	fn try_merge (&mut self, other: Self) -> Result <(), Self::Error>
	{
		for (other_ident, other_value) in other . map
		{
			match self . map . entry (other_ident . clone ())
			{
				Entry::Occupied (occupied) => if *occupied . get () != other_value
				{
					return Err
					(
						ParameterBindingMismatch::new
						(
							other_ident,
							occupied . get () . clone (),
							other_value
						)
					);
				},
				Entry::Vacant (vacant) =>
				{
					vacant . insert (other_value);
				}
			}
		}

		Ok (())
	}
}

impl SubstitutionBindings <TypedParameter <AutotransformBindingType>>
for AutotransformBindings
{
	type Error = SubstitutionError <AutotransformBindingType>;

	fn write_parameter_tokens
	(
		&self,
		parameter: TypedParameter <AutotransformBindingType>,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match self . map . get (&parameter . ident)
		{
			Some (value) =>
			{
				let value_type = value . get_type ();

				if value_type == parameter . ty
				{
					value . to_tokens (tokens);
					Ok (())
				}
				else
				{
					Err
					(
						ParameterTypeMismatch::new (value_type, parameter)
							. into ()
					)
				}
			},
			None => Err (ParameterNotFound::new (parameter) . into ())
		}
	}
}

impl SubstitutionBindings <UntypedParameter> for AutotransformBindings
{
	type Error = ParameterNotFound <UntypedParameter>;

	fn write_parameter_tokens
	(
		&self,
		parameter: UntypedParameter,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match self . map . get (&parameter . ident)
		{
			Some (value) => value . to_tokens (tokens),
			None => return Err (ParameterNotFound::new (parameter))
		}

		Ok (())
	}
}
