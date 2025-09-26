use std::error::Error;
use std::fmt::{Display, Formatter};

use macrospace::pattern::{
	TokenizeBinding,
	SubstitutionError,
	ParameterBindingTypeMismatch,
	VisitationError,
	SpecializationError
};
use proc_macro2::TokenStream;
use syn::{Visibility, Ident, Token, parse2};
use syn::parse::ParseStream;
use syn::token::{Brace, Bracket};
use syn_derive::{Parse, ToTokens};

use crate::{
	TypePatternBinding,
	TypePatternBindings,
	TransformBindingType,
	TypePattern,
	ExprBinding,
	ExprBindings,
	SubexprAnnotation,
	ExprPattern,
	SpecializationBinding,
	SpecializationBindings,
	transform_binding,
	transform_binding_type,
	kw
};

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct Autotransform
{
	pub vis: Visibility,
	pub autotransform_token: kw::autotransform,
	pub ident: Ident,

	#[syn (bracketed)]
	pub from_brackets: Bracket,
	#[syn (in = from_brackets)]
	pub from_type: TypePattern,

	pub arrow_token: Token! [->],

	#[syn (bracketed)]
	pub to_brackets: Bracket,
	#[syn (in = to_brackets)]
	pub to_type: TypePattern,

	#[syn (braced)]
	pub transform_braces: Brace,
	#[syn (in = transform_braces)]
	pub transform_expr: ExprPattern
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

		while let Some (autotransform) = Self::try_parse (input)?
		{
			autotransforms . push (autotransform);
		}

		Ok (autotransforms)
	}


	pub fn weak_validate (&self) -> syn::Result <()>
	{
		self . from_type . assert_parameters_superset (&self . to_type) . map_err
		(
			|ident|
			syn::Error::new_spanned
			(
				&ident,
				format!
				(
					"Parameter `{}` exists in to-pattern but not in from-pattern",
					ident
				)
			)
		)?;

		self . from_type . assert_parameters_superset (&self . transform_expr) . map_err
		(
			|ident|
			syn::Error::new_spanned
			(
				&ident,
				format!
				(
					"Parameter `{}` exists in expression-pattern but not in from-pattern",
					ident
				)
			)
		)?;

		Ok (())
	}

	pub fn specialize
	(
		&mut self,
		specialization_bindings: &SpecializationBindings
	)
	-> Result
	<
		(),
		SpecializationError
		<
			ParameterBindingTypeMismatch
			<
				SpecializationBinding,
				TransformBindingType
			>
		>
	>
	{
		self . from_type = self
			. from_type
			. specialize (specialization_bindings)?;
		self . to_type = self
			. to_type
			. specialize (specialization_bindings)?;
		self . transform_expr = self
			. transform_expr
			. specialize (specialization_bindings)?;

		Ok (())
	}

	pub fn strong_validate (&self) -> syn::Result <()>
	{
		self . weak_validate ()?;

		self . to_type . assert_parameters_superset (&self . from_type) . map_err
		(
			|ident|
			syn::Error::new_spanned
			(
				&ident,
				format!
				(
					"Parameter `{}` exists in from-pattern but not in to-pattern",
					ident
				)
			)
		)?;

		Ok (())
	}

	pub fn try_apply <D, F> (&self, ty_tokens: TokenStream, mut apply_inner: F)
	-> Result <Option <(TokenStream, ExprBinding)>, ApplicationError>
	where
		D: TransformDirection,
		F: FnMut (TokenStream)
			-> Result <Option <(TokenStream, ExprBinding)>, ApplicationError>
	{
		let mut autotransform_bindings: TypePatternBindings =
			match D::from_type (self) . match_tokens (ty_tokens)
		{
			Ok (bindings) => bindings,
			Err (_) => return Ok (None)
		};

		let mut expr_bindings = ExprBindings::new ();

		for (parameter_ident, autotransform_binding)
		in &mut autotransform_bindings
		{
			let expr_binding = transform_binding::<&mut F, F>
			(
				autotransform_binding,
				&mut apply_inner
			)?;

			// This will never fail on account of the fact that we will never
			// see the same identifier twice.
			expr_bindings
				. add_binding (parameter_ident . clone (), expr_binding)
				. unwrap ();
		}

		let transformed_ty =
			D::to_type (self) . substitute (&autotransform_bindings)?;

		let transformed_expr_pattern =
			parse2 (self . transform_expr . substitute (&expr_bindings)?)?;

		Ok (Some ((transformed_ty, transformed_expr_pattern)))
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
		let mut autotransform_bindings: TypePatternBindings =
			match D::from_type (self) . match_tokens (ty_tokens)
		{
			Ok (bindings) => bindings,
			Err (_) => return Ok (None)
		};

		for (_parameter_ident, autotransform_binding)
		in &mut autotransform_bindings
		{
			transform_binding_type::<&mut F, F>
			(
				autotransform_binding,
				&mut apply_inner
			)?;
		}

		let transformed_ty =
			D::to_type (self) . substitute (&autotransform_bindings)?;

		Ok (Some (transformed_ty))
	}
}

pub trait TransformDirection
{
	fn from_type (autotransform: &Autotransform) -> &TypePattern;

	fn to_type (autotransform: &Autotransform) -> &TypePattern;
}

pub struct Forward;

impl TransformDirection for Forward
{
	fn from_type (autotransform: &Autotransform) -> &TypePattern
	{
		&autotransform . from_type
	}

	fn to_type (autotransform: &Autotransform) -> &TypePattern
	{
		&autotransform . to_type
	}
}

pub struct Backward;

impl TransformDirection for Backward
{
	fn from_type (autotransform: &Autotransform) -> &TypePattern
	{
		&autotransform . to_type
	}

	fn to_type (autotransform: &Autotransform) -> &TypePattern
	{
		&autotransform . from_type
	}
}

pub type AutotransformTokenizeError =
	<TransformBindingType as TokenizeBinding <TypePatternBinding>>::Error;

pub type SubstituteTypeError =
	VisitationError <SubstitutionError <AutotransformTokenizeError>>;

pub type ExprTokenizeError =
	<SubexprAnnotation as TokenizeBinding <ExprBinding>>::Error;

pub type SubstituteExprError =
	VisitationError <SubstitutionError <ExprTokenizeError>>;

#[derive (Clone, Debug)]
pub enum ApplicationError
{
	Transform (syn::parse::Error),
	SubstituteType (SubstituteTypeError),
	SubstituteExpr (SubstituteExprError)
}

impl From <syn::parse::Error> for ApplicationError
{
	fn from (e: syn::parse::Error) -> Self
	{
		Self::Transform (e)
	}
}

impl From <SubstituteTypeError> for ApplicationError
{
	fn from (e: SubstituteTypeError) -> Self
	{
		Self::SubstituteType (e)
	}
}

impl From <SubstituteExprError> for ApplicationError
{
	fn from (e: SubstituteExprError) -> Self
	{
		Self::SubstituteExpr (e)
	}
}

impl Display for ApplicationError
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		match self
		{
			Self::Transform (e) => Display::fmt (e, f),
			Self::SubstituteType (e) => Display::fmt (e, f),
			Self::SubstituteExpr (e) => Display::fmt (e, f)
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
			Self::Transform (e) => e,
			Self::SubstituteType (e) => e . into (),
			Self::SubstituteExpr (e) => e . into ()
		}
	}
}
