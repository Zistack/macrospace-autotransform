use syn::{Visibility, Ident};
use syn::parse::{ParseStream, Result};
use syn_derive::{Parse, ToTokens};
use quote::TokenStreamExt;

use crate::{Autotransform, kw};

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
}

pub struct FlattenAutotransformInputs <I>
{
	inputs: I,
	current_group: Option <<Vec <Autotransform> as IntoIterator>::IntoIter>
}

impl <I> FlattenAutotransformInputs <I>
{
	fn new (inputs: I) -> Self
	{
		Self {inputs, current_group: None}
	}
}

impl <I> Iterator for FlattenAutotransformInputs <I>
where I: Iterator <Item = AutotransformInput>
{
	type Item = Autotransform;

	fn next (&mut self) -> Option <Autotransform>
	{
		loop
		{
			if let Some (iter) = &mut self . current_group
			{
				match iter . next ()
				{
					Some (autotransform) => return Some (autotransform),
					None => self . current_group = None
				}
			}

			match self . inputs . next ()
			{
				Some (AutotransformInput::Single (autotransform)) =>
					return Some (autotransform),
				Some (AutotransformInput::Group (autotransform_group)) =>
				{
					self . current_group = Some
					(
						autotransform_group . autotransforms . into_iter ()
					);
					continue;
				},
				None => return None
			}
		}
	}
}

pub fn flatten_autotransform_inputs <I> (inputs: I)
-> FlattenAutotransformInputs <<I as IntoIterator>::IntoIter>
where I: IntoIterator <Item = AutotransformInput>
{
	FlattenAutotransformInputs::new (inputs . into_iter ())
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
		flatten_autotransform_inputs (self . inputs) . collect ()
	}
}
