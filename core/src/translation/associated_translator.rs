use std::collections::HashMap;
use std::marker::PhantomData;

use macrospace::substitute::Substitutions;
use syn::{
	Ident,
	Type,
	Generics,
	AngleBracketedGenericArguments,
	Token
};
use syn::fold::Fold;
use syn::parse::{Parser, ParseStream, Result};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use super::util::{TranslationTarget, Base, Delegated};

fn parse_generic_arguments (input: ParseStream <'_>)
-> Result <Option <AngleBracketedGenericArguments>>
{
	if input . peek (Token! [::]) || input . peek (Token! [<])
	{
		Ok (Some (input . parse ()?))
	}
	else
	{
		Ok (None)
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
struct SimpleAssocType
{
	self_token: Token! [Self],
	double_colon_token: Token! [::],
	assoc_type_ident: Ident,
	#[parse (parse_generic_arguments)]
	generic_arguments: Option <AngleBracketedGenericArguments>
}

#[derive (Clone, Debug, Parse, ToTokens)]
struct QualifiedAssocType
{
	l_angle_token: Token! [<],
	self_token: Token! [Self],
	as_token: Token! [as],
	trait_ident: Ident,
	r_angle_token: Token! [>],
	double_colon_token: Token! [::],
	assoc_type_ident: Ident,
	#[parse (parse_generic_arguments)]
	generic_arguments: Option <AngleBracketedGenericArguments>
}

#[derive (Clone, Debug)]
pub struct AssociatedTypes
{
	trait_ident: Ident,
	map: HashMap <Ident, (Generics, Type, Type)>
}

impl AssociatedTypes
{
	pub fn new (trait_ident: Ident) -> Self
	{
		Self {trait_ident, map: HashMap::new ()}
	}

	pub fn add_associated_type
	(
		&mut self,
		ident: Ident,
		generics: Generics,
		ty: Type,
		delegated_ty: Type
	)
	{
		self . map . insert (ident, (generics, ty, delegated_ty));
	}

	pub fn translate_base (&self) -> AssociatedTranslator <'_, Base>
	{
		AssociatedTranslator::new (self)
	}

	pub fn translate_delegated (&self) -> AssociatedTranslator <'_, Delegated>
	{
		AssociatedTranslator::new (self)
	}
}

#[derive (Debug)]
pub struct AssociatedTranslator <'a, T>
{
	associated_types: &'a AssociatedTypes,
	_d: PhantomData <T>
}

impl <'a, T> AssociatedTranslator <'a, T>
{
	fn new (associated_types: &'a AssociatedTypes) -> Self
	{
		Self {associated_types, _d: PhantomData::default ()}
	}
}

impl <'a, T> Copy for AssociatedTranslator <'a, T>
{
}

impl <'a, T> Clone for AssociatedTranslator <'a, T>
{
	fn clone (&self) -> Self
	{
		*self
	}
}

impl <'a, T> Fold for AssociatedTranslator <'a, T>
where T: TranslationTarget
{
	fn fold_type (&mut self, ty: Type) -> Type
	{
		let parser = |input: ParseStream <'_>|
		{
			let (generics, ty_arguments, translated_ty) = if input . peek (Token! [Self])
			{
				let simple_assoc_type: SimpleAssocType = input . parse ()?;

				match self
					. associated_types
					. map
					. get (&simple_assoc_type . assoc_type_ident)
				{
					Some ((generics, ty, delegated_ty)) =>
					(
						generics,
						simple_assoc_type . generic_arguments,
						T::select (ty, delegated_ty)
					),
					None => return Ok (None)
				}
			}
			else if input . peek (Token! [<])
			{
				let qualified_assoc_type: QualifiedAssocType = input . parse ()?;

				if qualified_assoc_type . trait_ident
					== self . associated_types . trait_ident
				{
					match self
						. associated_types
						. map
						. get (&qualified_assoc_type . assoc_type_ident)
					{
						Some ((generics, ty, delegated_ty)) =>
						(
							generics,
							qualified_assoc_type . generic_arguments,
							T::select (ty, delegated_ty)
						),
						None => return Ok (None)
					}
				}
				else
				{
					return Ok (None);
				}
			}
			else
			{
				return Ok (None);
			};

			let translated_ty = match ty_arguments
			{
				Some (ty_arguments) =>
				{
					let mut substitutions = Substitutions::try_from_generics
					(
						&generics . params,
						&ty_arguments . args
					)?;

					substitutions . fold_type (translated_ty . clone ())
				},
				None => translated_ty . clone ()
			};

			Ok (Some (translated_ty))
		};

		match parser . parse2 (ty . to_token_stream ())
		{
			Ok (Some (translated_ty)) => translated_ty,
			_ => ty
		}
	}
}
