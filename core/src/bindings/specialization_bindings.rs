use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use macrospace::pattern::{
	TypedParameter,
	SubstitutionBindings,
	ParameterTypeMismatch
};
use macrospace::substitute::Argument;
use proc_macro2::TokenStream;
use syn::Ident;
use quote::ToTokens;

use super::{AutotransformBindingType, SubvalueParameter, ExprParameter};

pub struct SpecializationBindings
{
	map: HashMap <Ident, Argument>
}

impl FromIterator <(Ident, Argument)> for SpecializationBindings
{
	fn from_iter <I> (iter: I) -> Self
	where I: IntoIterator <Item = (Ident, Argument)>
	{
		Self {map: HashMap::from_iter (iter)}
	}
}

impl SubstitutionBindings <TypedParameter <AutotransformBindingType>>
for SpecializationBindings
{
	type Error = ParameterTypeMismatch <AutotransformBindingType>;

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
				match (value, parameter . ty)
				{
					(
						Argument::Lifetime (lifetime),
						AutotransformBindingType::OptionalLifetime (_)
					) => Ok
					(
						lifetime . to_tokens (tokens)
					),
					(
						Argument::Lifetime (lifetime),
						AutotransformBindingType::Lifetime (_)
					) => Ok
					(
						lifetime . to_tokens (tokens)
					),
					(Argument::Lifetime (_), _) => Err
					(
						ParameterTypeMismatch::new
						(
							AutotransformBindingType::Lifetime
							(
								Default::default ()
							),
							parameter
						)
					),
					(
						Argument::Type (ty),
						AutotransformBindingType::Type (_)
					) => Ok
					(
						ty . to_tokens (tokens)
					),
					(
						Argument::Type (ty),
						AutotransformBindingType::InnerType (_)
					) => Ok
					(
						ty . to_tokens (tokens)
					),
					(Argument::Type (_), _) => Err
					(
						ParameterTypeMismatch::new
						(
							AutotransformBindingType::Type
							(
								Default::default ()
							),
							parameter
						)
					),
					(
						Argument::Const (expr),
						AutotransformBindingType::Const (_)) => Ok
					(
						expr . to_tokens (tokens)
					),
					(Argument::Const (_), _) => Err
					(
						ParameterTypeMismatch::new
						(
							AutotransformBindingType::Const
							(
								Default::default ()
							),
							parameter
						)
					)
				}
			},
			None => Ok (parameter . to_tokens (tokens))
		}
	}
}

#[derive (Clone, Debug)]
pub struct SpecializationTypeMismatch <T>
{
	value: Argument,
	value_type: T,
	parameter: SubvalueParameter
}

impl <T> SpecializationTypeMismatch <T>
{
	fn new (value: Argument, value_type: T, parameter: SubvalueParameter)
	-> Self
	{
		Self {value, value_type, parameter}
	}
}

impl <T> Display for SpecializationTypeMismatch <T>
where T: Display
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		f . write_fmt
		(
			format_args!
			(
				"parameter {} has value of type {}, but expression parameters may only be specialized with types",
				&self . parameter . ident,
				self . value_type
			)
		)
	}
}

impl <T> Error for SpecializationTypeMismatch <T>
where T: Debug + Display
{
}

impl <T> Into <syn::parse::Error> for SpecializationTypeMismatch <T>
where T: Display
{
	fn into (self) -> syn::parse::Error
	{
		syn::parse::Error::new_spanned
		(
			&self . value,
			self . to_string ()
		)
	}
}

impl SubstitutionBindings <ExprParameter> for SpecializationBindings
{
	type Error = SpecializationTypeMismatch <&'static str>;

	fn write_parameter_tokens
	(
		&self,
		parameter: ExprParameter,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match parameter
		{
			ExprParameter::Arg (arg_parameter) =>
				Ok (arg_parameter . to_tokens (tokens)),
			ExprParameter::Subvalue (subvalue_parameter) => match self
				. map
				. get (&subvalue_parameter . ident)
			{
				Some (value) => match value
				{
					Argument::Lifetime (_) => Err
					(
						SpecializationTypeMismatch::new
						(
							value . clone (),
							"lifetime",
							subvalue_parameter
						)
					),
					Argument::Type (_) => Ok
					(
						subvalue_parameter . arg_expr . to_tokens (tokens)
					),
					Argument::Const (_) => Err
					(
						SpecializationTypeMismatch::new
						(
							value . clone (),
							"const",
							subvalue_parameter
						)
					)
				},
				None => Ok (subvalue_parameter . to_tokens (tokens))
			}
		}
	}
}
