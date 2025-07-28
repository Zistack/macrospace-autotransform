use proc_macro2::TokenStream;
use syn::{Generics, Path, Token, LitInt, bracketed, braced};
use syn::parse::{Parse, ParseStream, Result, Error};
use syn::punctuated::Punctuated;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use macrospace_autotransform_core::{Autotransform, AutotransformBank};

use super::{AutotransformImplItem, kw};

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

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformPaths
{
	#[parse (Punctuated::parse_terminated)]
	pub paths: Punctuated <Path, Token! [,]>
}

pub type UserAutotransformImplBlock =
	AutotransformImplBlock <AutotransformPaths>;
pub type PreGatherImplBlock =
	AutotransformImplBlock <LitInt>;
pub type PostGatherImplBlock =
	AutotransformImplBlock <AutotransformBank>;

impl UserAutotransformImplBlock
{
	pub fn into_pre_gather (self)
	->
	(
		AutotransformPaths,
		AutotransformPaths,
		PreGatherImplBlock
	)
	{
		let pre_gather_block = PreGatherImplBlock
		{
			impl_token: self . impl_token,
			generics: self . generics,
			receiver_type_path: self . receiver_type_path,
			with_token: self . with_token,
			to_bracket_token: self . to_bracket_token,
			to_delegate_transforms: LitInt::new
			(
				&self . to_delegate_transforms . paths . len () . to_string (),
				proc_macro2::Span::call_site ()
			),
			arrow_token: self . arrow_token,
			from_bracket_token: self . from_bracket_token,
			from_delegate_transforms: LitInt::new
			(
				&self . from_delegate_transforms . paths . len () . to_string (),
				proc_macro2::Span::call_site ()
			),
			brace_token: self . brace_token,
			impl_items: self . impl_items
		};

		(
			self . to_delegate_transforms,
			self . from_delegate_transforms,
			pre_gather_block
		)
	}
}

impl PreGatherImplBlock
{
	pub fn try_into_post_gather (self, autotransforms: Vec <Autotransform>)
	-> Result <PostGatherImplBlock>
	{
		let num_to_delegate_transforms: usize =
			self . to_delegate_transforms . base10_parse ()?;
		let num_from_delegate_transforms: usize =
			self . from_delegate_transforms . base10_parse ()?;

		if autotransforms . len () < num_to_delegate_transforms
		{
			return Err
			(
				Error::new_spanned
				(
					&self . to_delegate_transforms,
					format!
					(
						"expected at least {} autotransforms, found {}",
						num_to_delegate_transforms,
						autotransforms . len ()
					)
				)
			);
		}

		let mut to_delegate_transforms = autotransforms;
		let from_delegate_transforms =
			to_delegate_transforms . split_off (num_to_delegate_transforms);

		if from_delegate_transforms . len () != num_from_delegate_transforms
		{
			return Err
			(
				Error::new_spanned
				(
					&self . from_delegate_transforms,
					format!
					(
						"expected {} autotransforms from delegate type, found {}",
						num_from_delegate_transforms,
						from_delegate_transforms . len ()
					)
				)
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
