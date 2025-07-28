use std::collections::HashMap;

use macrospace::substitute::Substitutions;
use proc_macro2::{TokenStream, TokenTree, Delimiter};
use syn::{Ident, AngleBracketedGenericArguments, Type, Path, Generics, Token, parse_quote};
use syn::fold::{Fold, fold_type};
use syn::parse::{Parser, ParseStream, Result, Error};
use syn::punctuated::Punctuated;
use syn_derive::Parse;
use quote::{ToTokens, quote};

fn expect_token_tree (input: ParseStream <'_>, expected_token_tree: TokenTree)
-> Result <()>
{
	let input_token_tree = input . parse ()?;

	match (input_token_tree, expected_token_tree)
	{
		(TokenTree::Group (input_group), TokenTree::Group (expected_group)) =>
		{
			let input_delimiter = input_group . delimiter ();
			let expected_delimiter = expected_group . delimiter ();

			if input_delimiter != expected_delimiter
			{
				let expected_char = match expected_delimiter
				{
					Delimiter::Parenthesis => "(",
					Delimiter::Brace => "{",
					Delimiter::Bracket => "[",
					Delimiter::None => "∅"
				};

				return Err
				(
					Error::new
					(
						input_group . span_open (),
						format! ("expected `{}`", expected_char)
					)
				);
			}

			let parser = |input: ParseStream <'_>|
			{
				expect_tokens (input, expected_group . stream ())
			};

			return parser . parse2 (input_group . stream ());
		},
		(TokenTree::Ident (input_ident), TokenTree::Ident (expected_ident)) =>
		{
			if input_ident != expected_ident
			{
				return Err
				(
					Error::new_spanned
					(
						input_ident,
						format! ("expected `{}`", expected_ident)
					)
				);
			}
		},
		(TokenTree::Punct (input_punct), TokenTree::Punct (expected_punct)) =>
		{
			if input_punct . as_char () != expected_punct . as_char ()
			{
				return Err
				(
					Error::new_spanned
					(
						input_punct,
						format! ("expected `{}`", expected_punct)
					)
				);
			}
		},
		(TokenTree::Literal (input_literal), TokenTree::Literal (expected_literal)) =>
		{
			if input_literal . to_string () != expected_literal . to_string ()
			{
				return Err
				(
					Error::new_spanned
					(
						input_literal,
						format! ("expected `{}`", expected_literal)
					)
				);
			}
		},
		(input @ _, TokenTree::Group (expected_group)) =>
		{
			let expected_char = match expected_group . delimiter ()
			{
					Delimiter::Parenthesis => "(",
					Delimiter::Brace => "{",
					Delimiter::Bracket => "[",
					Delimiter::None => "∅"
			};

			return Err
			(
				Error::new
				(
					input . span (),
					format! ("expected `{}`", expected_char)
				)
			);
		},
		(input @ _, expected @ _) =>
		{
			return Err
			(
				Error::new
				(
					input . span (),
					format! ("expected `{}`", expected)
				)
			);
		}
	}

	Ok (())
}

fn expect_tokens (input: ParseStream <'_>, expected_tokens: TokenStream)
-> Result <()>
{
	for expected_token_tree in expected_tokens
	{
		expect_token_tree (input, expected_token_tree)?;
	}

	Ok (())
}

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
		trait_ident: &Ident
	)
	-> Result <Self>
	{
		let parser = |input: ParseStream <'_>|
		{
			expect_tokens (input, quote! (<Self as #trait_ident>::))?;

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
			let trait_ident = &last_segment . ident;

			if let Ok (assoc_bindings) =
				AssocBindings::match_self_as_trait_assoc_pattern
			(
				ty,
				trait_ident
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
