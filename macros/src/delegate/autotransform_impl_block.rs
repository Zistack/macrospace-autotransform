use proc_macro2::TokenStream;
use syn::{Generics, Path, bracketed, braced};
use syn::parse::{Parse, ParseStream, Result, Error};
use quote::ToTokens;

use macrospace_autotransform_core::{
	AutotransformInputs,
	AutotransformBank
};

use super::{AutotransformImplItem, kw};
use super::autotransform_path::*;

#[derive (Clone, Debug)]
pub struct AutotransformImplBlock <A>
{
	pub impl_token: syn::token::Impl,
	pub generics: Generics,
	pub receiver_type_path: Option <Path>,
	pub with_token: kw::with,
	pub to_bracket_token: syn::token::Bracket,
	pub to_delegate_transforms: A,
	pub arrow_token: syn::token::RArrow,
	pub from_bracket_token: syn::token::Bracket,
	pub from_delegate_transforms: A,
	// where clause
	pub brace_token: syn::token::Brace,
	pub impl_items: Vec <AutotransformImplItem>
}

impl <A> AutotransformImplBlock <A>
where A: Parse
{
	pub fn parse_all (input: ParseStream <'_>) -> Result <Vec <Self>>
	{
		let mut impl_blocks = Vec::new ();

		while ! input . is_empty ()
		{
			impl_blocks . push (input . parse ()?);
		}

		Ok (impl_blocks)
	}
}

impl <A> Parse for AutotransformImplBlock <A>
where A: Parse
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let impl_token = input . parse ()?;
		let mut generics: Generics = input . parse ()?;

		let receiver_type_path = if input . peek (kw::with)
		{
			None
		}
		else
		{
			Some (input .  parse ()?)
		};

		let with_token = input . parse ()?;

		let content;
		let to_bracket_token = bracketed! (content in input);
		let to_delegate_transforms = content . parse ()?;

		let arrow_token = input . parse ()?;

		let content;
		let from_bracket_token = bracketed! (content in input);
		let from_delegate_transforms = content . parse ()?;

		generics . where_clause = input . parse ()?;

		let content;
		let brace_token = braced! (content in input);
		let impl_items = AutotransformImplItem::parse_all (&content)?;

		Ok
		(
			Self
			{
				impl_token,
				generics,
				receiver_type_path,
				with_token,
				to_bracket_token,
				to_delegate_transforms,
				arrow_token,
				from_bracket_token,
				from_delegate_transforms,
				brace_token,
				impl_items
			}
		)
	}
}

impl <A> ToTokens for AutotransformImplBlock <A>
where A: ToTokens
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		self . impl_token . to_tokens (tokens);
		self . generics . to_tokens (tokens);
		self . receiver_type_path . to_tokens (tokens);
		self . with_token . to_tokens (tokens);

		self . to_bracket_token . surround
		(
			tokens,
			|inner_tokens|
			self . to_delegate_transforms . to_tokens (inner_tokens)
		);

		self . arrow_token . to_tokens (tokens);

		self . from_bracket_token . surround
		(
			tokens,
			|inner_tokens|
			self . from_delegate_transforms . to_tokens (inner_tokens)
		);

		self . generics . where_clause . to_tokens (tokens);

		self . brace_token . surround
		(
			tokens,
			|inner_tokens|
			for impl_item in &self . impl_items
			{
				impl_item . to_tokens (inner_tokens)
			}
		);
	}
}

pub type UserAutotransformImplBlock =
	AutotransformImplBlock <AutotransformPaths>;
pub type PostGatherImplBlock =
	AutotransformImplBlock <AutotransformBank>;

impl UserAutotransformImplBlock
{
	pub fn try_into_post_gather (self, autotransforms: AutotransformInputs)
	-> Result <PostGatherImplBlock>
	{
		let num_to_delegate_transforms =
			self . to_delegate_transforms . paths . len ();
		let num_from_delegate_transforms =
			self . from_delegate_transforms . paths . len ();

		let total_expected_transforms =
			num_to_delegate_transforms + num_from_delegate_transforms;

		if autotransforms . inputs . len () != total_expected_transforms
		{
			return Err
			(
				Error::new_spanned
				(
					&self . to_delegate_transforms,
					format!
					(
						"expected at {} autotransforms, found {}",
						total_expected_transforms,
						autotransforms . inputs . len ()
					)
				)
			);
		}

		let mut to_delegate_transforms = Vec::new ();
		let mut from_delegate_transforms = Vec::new ();

		let mut autotransform_inputs_iter =
			autotransforms . inputs . into_iter ();

		for (autotransform_path, mut autotransform_input)
		in self
			. to_delegate_transforms
			. paths
			. into_iter ()
			. zip
			(
				(&mut autotransform_inputs_iter)
					. take (num_to_delegate_transforms)
			)
		{
			if let Some (parameters) = autotransform_path
				. autotransform_parameters
			{
				autotransform_input . specialize
				(
					parameters
						. assignments
						. into_iter ()
						. map (|assignment| (assignment . ident, assignment . value))
				)?;
			}

			to_delegate_transforms . extend
			(
				autotransform_input . into_autotransforms ()
			);
		}

		for (autotransform_path, mut autotransform_input)
		in self
			. from_delegate_transforms
			. paths
			. into_iter ()
			. zip
			(
				(&mut autotransform_inputs_iter)
					. take (num_from_delegate_transforms)
			)
		{
			if let Some (parameters) = autotransform_path
				. autotransform_parameters
			{
				autotransform_input . specialize
				(
					parameters
						. assignments
						. into_iter ()
						. map (|assignment| (assignment . ident, assignment . value))
				)?;
			}

			from_delegate_transforms . extend
			(
				autotransform_input . into_autotransforms ()
			);
		}

		let post_gather_block = PostGatherImplBlock
		{
			impl_token: self . impl_token,
			generics: self . generics,
			receiver_type_path: self . receiver_type_path,
			with_token: self . with_token,
			to_bracket_token: self . to_bracket_token,
			to_delegate_transforms:
				AutotransformBank::from (to_delegate_transforms),
			arrow_token: self . arrow_token,
			from_bracket_token: self . from_bracket_token,
			from_delegate_transforms:
				AutotransformBank::from (from_delegate_transforms),
			brace_token: self . brace_token,
			impl_items: self . impl_items
		};

		Ok (post_gather_block)
	}
}
