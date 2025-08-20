use std::collections::HashMap;

use macrospace::substitute::Substitutions;
use macrospace::pattern::expect_tokens;
use syn::{
	Ident,
	AngleBracketedGenericArguments,
	Type,
	Path,
	PathSegment,
	Generics,
	Token,
	parse_quote
};
use syn::fold::{Fold, fold_type};
use syn::parse::{Parser, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn_derive::Parse;
use quote::{ToTokens, quote};

#[derive (Clone, Debug, Parse)]
struct AssocBindings
{
	assoc_ident: Ident,
	#[parse (parse_maybe_args)]
	args: Option <AngleBracketedGenericArguments>
}

fn parse_maybe_args (input: ParseStream <'_>)
-> Result <Option <AngleBracketedGenericArguments>>
{
	if input . peek (Token! [<])
	{
		Ok (Some (input . parse ()?))
	}
	else
	{
		Ok (None)
	}
}

impl AssocBindings
{
	fn match_self_assoc_pattern (ty: &Type) -> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (Self::))?;

			Ok (input . parse ()?)
		};

		parser . parse2 (ty . to_token_stream ())
	}

	fn match_self_as_trait_assoc_pattern
	(
		ty: &Type,
		trait_segment: &PathSegment
	)
	-> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (<Self as #trait_segment>::))?;

			Ok (input . parse ()?)
		};

		parser . parse2 (ty . to_token_stream ())
	}

	fn match_receiver_assoc_pattern
	(
		ty: &Type,
		receiver_type: &Type
	)
	-> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (#receiver_type::))?;

			Ok (input . parse ()?)
		};

		parser . parse2 (ty . to_token_stream ())
	}

	fn match_receiver_type_assoc_pattern
	(
		ty: &Type,
		receiver_type: &Type
	)
	-> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (<#receiver_type>::))?;

			Ok (input . parse ()?)
		};

		parser . parse2 (ty . to_token_stream ())
	}

	fn match_receiver_as_trait_assoc_pattern
	(
		ty: &Type,
		receiver_type: &Type,
		trait_path: &Path
	)
	-> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (<#receiver_type as #trait_path>::))?;

			Ok (input . parse ()?)
		};

		parser . parse2 (ty . to_token_stream ())
	}
}

pub struct TypeTransformer
{
	receiver_type: Type,
	trait_path: Path,
	pub associated_types: HashMap <Ident, (Generics, Type)>,
}

impl TypeTransformer
{
	pub fn new (receiver_type: Type, trait_path: Path) -> Self
	{
		Self {receiver_type, trait_path, associated_types: HashMap::new ()}
	}

	pub fn add_associated_type
	(
		&mut self,
		ident: Ident,
		generics: Generics,
		ty: Type
	)
	{
		self . associated_types . insert (ident, (generics, ty));
	}

	fn get_assoc_bindings (&self, ty: &Type) -> Option <AssocBindings>
	{
		if let Ok (assoc_bindings) =
			AssocBindings::match_self_assoc_pattern (ty)
		{
			return Some (assoc_bindings);
		}

		if let Some (last_segment) = self . trait_path . segments . last ()
		{
			if let Ok (assoc_bindings) =
				AssocBindings::match_self_as_trait_assoc_pattern
			(
				ty,
				last_segment
			)
			{
				return Some (assoc_bindings);
			}
		}

		if let Ok (assoc_bindings) = AssocBindings::match_receiver_assoc_pattern
		(
			ty,
			&self . receiver_type
		)
		{
			return Some (assoc_bindings);
		}

		if let Ok (assoc_bindings) =
			AssocBindings::match_receiver_type_assoc_pattern
		(
			ty,
			&self . receiver_type
		)
		{
			return Some (assoc_bindings);
		}

		if let Ok (assoc_bindings) =
			AssocBindings::match_receiver_as_trait_assoc_pattern
		(
			ty,
			&self . receiver_type,
			&self . trait_path
		)
		{
			return Some (assoc_bindings);
		}

		None
	}

	fn get_assoc_replacement_type (&self, ty: &Type)
	-> Option <Type>
	{
		let assoc_bindings = match self . get_assoc_bindings (ty)
		{
			Some (assoc_bindings) => assoc_bindings,
			None => return None
		};

		let (assoc_generics, assoc_ty) = match self
			. associated_types
			. get (&assoc_bindings . assoc_ident)
		{
			Some ((assoc_generics, assoc_ty)) => (assoc_generics, assoc_ty),
			None => return None
		};

		let args = match assoc_bindings . args
		{
			Some (angle_bracketed) => angle_bracketed . args,
			None => Punctuated::new ()
		};

		let mut substitutions = match Substitutions::try_from_path_arguments
		(
			&assoc_generics . params,
			&args
		)
		{
			Ok (substitutions) => substitutions,
			Err (_) => return None
		};

		Some (substitutions . fold_type (assoc_ty . clone ()))
	}
}

impl Fold for TypeTransformer
{
	fn fold_type (&mut self, ty: Type) -> Type
	{
		if let Some (ty) = self . get_assoc_replacement_type (&ty)
		{
			return ty;
		}

		if ty == parse_quote! (Self)
		{
			return self . receiver_type . clone ();
		}

		return fold_type (self, ty);
	}
}
