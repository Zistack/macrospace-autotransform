use std::iter::{Once, once};

use macrospace::substitute::Argument;
use syn::{Visibility, Ident};
use syn::parse::{ParseStream, Result};
use syn_derive::{Parse, ToTokens};
use quote::TokenStreamExt;

use crate::{Autotransform, kw};
use crate::bindings::SpecializationBindings;

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformGroup
{
	pub vis: Visibility,
	pub autotransform_token: kw::autotransform,
	pub ident: Ident,

	#[syn (braced)]
	pub brace_token: syn::token::Brace,
	#[syn (in = brace_token)]
	#[parse (Autotransform::parse_all)]
	#[to_tokens (|tokens, val| tokens . append_all (val))]
	pub autotransforms: Vec <Autotransform>
}

impl AutotransformGroup
{
	pub fn specialize <I> (&mut self, assignments: I) -> syn::Result <()>
	where I: IntoIterator <Item = (Ident, Argument)>
	{
		let specialization_bindings =
			SpecializationBindings::from_iter (assignments);

		for autotransform in &mut self . autotransforms
		{
			autotransform . specialize_with_bindings (&specialization_bindings)?;
		}

		Ok (())
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
#[parse (
	prefix = |input|
	{
		input . parse::<Visibility> ()?;
		input . parse::<kw::autotransform> ()?;
		input . parse::<Ident> ()?;
		Ok (())
	}
)]
pub enum AutotransformInput
{
	#[parse (peek = syn::token::Bracket)]
	Single (Autotransform),

	#[parse (peek = syn::token::Brace)]
	Group (AutotransformGroup)
}

impl AutotransformInput
{
	pub fn try_parse (input: ParseStream <'_>) -> Result <Option <Self>>
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

		speculative . parse::<Ident> ()?;

		let lookahead = speculative . lookahead1 ();

		if lookahead . peek (syn::token::Bracket)
		{
			return Ok (Some (Self::Single (input . parse ()?)))
		}

		if lookahead . peek (syn::token::Brace)
		{
			return Ok (Some (Self::Group (input . parse ()?)))
		}

		Err (lookahead . error ())
	}

	pub fn parse_all (input: ParseStream <'_>) -> Result <Vec <Self>>
	{
		let mut autotransform_inputs = Vec::new ();

		while let Some (autotransform_input) = Self::try_parse (input)?
		{
			autotransform_inputs . push (autotransform_input);
		}

		Ok (autotransform_inputs)
	}

	pub fn specialize <I> (&mut self, assignments: I) -> syn::Result <()>
	where I: IntoIterator <Item = (Ident, Argument)>
	{
		match self
		{
			Self::Single (autotransform) =>
				autotransform . specialize (assignments),
			Self::Group (autotransform_group) =>
				autotransform_group . specialize (assignments)
		}
	}

	pub fn into_autotransforms (self) -> AutotransformInputIntoIter
	{
		match self
		{
			Self::Single (autotransform) => AutotransformInputIntoIter::Single
			(
				once (autotransform)
			),
			Self::Group (autotransform_group) => AutotransformInputIntoIter::Group
			(
				autotransform_group . autotransforms . into_iter ()
			)
		}
	}
}

pub enum AutotransformInputIntoIter
{
	Single (Once <Autotransform>),
	Group (<Vec <Autotransform> as IntoIterator>::IntoIter)
}

impl Iterator for AutotransformInputIntoIter
{
	type Item = Autotransform;

	fn next (&mut self) -> Option <Self::Item>
	{
		match self
		{
			Self::Single (one) => one . next (),
			Self::Group (many) => many . next ()
		}
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformInputs
{
	#[parse (AutotransformInput::parse_all)]
	#[to_tokens (|tokens, val| tokens . append_all (val))]
	pub inputs: Vec <AutotransformInput>
}

impl AutotransformInputs
{
	pub fn into_flattened (self) -> Vec <Autotransform>
	{
		self
			. inputs
			. into_iter ()
			. flat_map (AutotransformInput::into_autotransforms)
			. collect ()
	}
}
