use std::error::Error;
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	Pattern,
	TypedParameter,
	SubstitutionError,
	ParameterBindingMismatch
};
use proc_macro2::TokenStream;
use syn::{Visibility, Ident, Expr, Type, Token, parse2};
use syn::parse::ParseStream;
use syn::token::{Brace, Bracket};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use crate::bindings::{
	AutotransformBindings,
	AutotransformBindingType,
	AutotransformParameters,
	ExprParameter,
	ExprBindings,
	ExprParameters,
	ValidationTokens
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
	pub transform_expr: Pattern <ExprParameter>,
}

impl Autotransform
{
	pub fn try_parse (input: ParseStream <'_>) -> syn::Result <Option <Self>>
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

	pub fn parse_all (input: ParseStream <'_>) -> syn::Result <Vec <Self>>
	{
		let mut autotransforms = Vec::new ();

		while let Some (autotransform) = Autotransform::try_parse (input)?
		{
			autotransforms . push (autotransform);
		}

		Ok (autotransforms)
	}

	pub fn validate (&mut self) -> syn::Result <()>
	{
		let to_params = self
			. to_type
			. validate_as_and_collect::<Type, AutotransformParameters, ValidationTokens> ()?;
		let from_params = self
			. from_type
			. validate_as_and_collect::<Type, AutotransformParameters, ValidationTokens> ()?;

		let expr_params = self
			. transform_expr
			. validate_as_and_collect::<Expr, ExprParameters, ValidationTokens> ()?;

		to_params . assert_superset (&from_params)?;
		from_params . assert_superset (&to_params)?;

		to_params . assert_expr_superset (&expr_params)?;

		Ok (())
	}

	pub fn try_apply <D, F>
	(
		&self,
		ty_tokens: TokenStream,
		arg_expr: Expr,
		mut apply_inner: F
	)
	-> Result <Option <(TokenStream, TokenStream)>, ApplicationError>
	where
		D: TransformDirection,
		F: FnMut (TokenStream, Option <&Expr>)
			-> Result <Option <(TokenStream, Option <TokenStream>)>, ApplicationError>
	{
		let mut autotransform_bindings: AutotransformBindings =
			match D::from_type (self) . match_tokens (ty_tokens)
		{
			Ok (bindings) => bindings,
			Err (_) => return Ok (None)
		};

		let expr_parameters: ExprParameters = self
			. transform_expr
			. collect_parameters ()?;

		let mut expr_bindings = ExprBindings::new (arg_expr);

		for (transformable_ident, transformable_ty)
		in autotransform_bindings . inner_types_mut ()
		{
			match apply_inner
			(
				transformable_ty . to_token_stream (),
				expr_parameters . get_arg_expr (transformable_ident)
			)?
			{
				Some ((transformed_ty, maybe_transformed_expr)) =>
				{
					*transformable_ty = parse2 (transformed_ty)?;

					if let Some (transformed_expr) = maybe_transformed_expr
					{
						expr_bindings . add_subvalue
						(
							transformable_ident . clone (),
							parse2 (transformed_expr)?
						);
					}
				},
				None => continue
			}
		}

		let transformed_ty =
			D::to_type (self) . substitute (autotransform_bindings)?;

		let transformed_expr = self
			. transform_expr
			. substitute (expr_bindings)
			// This process is infallible.
			. unwrap ();

		Ok (Some ((transformed_ty, transformed_expr)))
	}

	// This cannot fail in as many ways as the first version, and the return
	// type should reflect that.
	pub fn try_apply_type <D, F>
	(
		&self,
		ty_tokens: TokenStream,
		mut apply_inner: F
	)
	-> Result <Option <TokenStream>, ApplicationError>
	where
		D: TransformDirection,
		F: FnMut (TokenStream)
			-> Result <Option <TokenStream>, ApplicationError>
	{
		let mut autotransform_bindings: AutotransformBindings =
			match D::from_type (self) . match_tokens (ty_tokens)
		{
			Ok (bindings) => bindings,
			Err (_) => return Ok (None)
		};

		for (_transformable_ident, transformable_ty)
		in autotransform_bindings . inner_types_mut ()
		{
			match apply_inner (transformable_ty . to_token_stream ())?
			{
				Some (transformed_ty) =>
					*transformable_ty = parse2 (transformed_ty)?,
				None => continue
			}
		}

		let transformed_ty =
			D::to_type (self) . substitute (autotransform_bindings)?;

		Ok (Some (transformed_ty))
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
	Collect (ParameterBindingMismatch <TokenStream>),
	Substitute (SubstitutionError <AutotransformBindingType>)
}

impl From <syn::parse::Error> for ApplicationError
{
	fn from (m: syn::parse::Error) -> Self
	{
		Self::Transform (m)
	}
}

impl From <ParameterBindingMismatch <TokenStream>> for ApplicationError
{
	fn from (c: ParameterBindingMismatch <TokenStream>) -> Self
	{
		Self::Collect (c)
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
			Self::Collect (c) => Display::fmt (c, f),
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
			Self::Collect (c) => c . into (),
			Self::Substitute (s) => s . into ()
		}
	}
}
