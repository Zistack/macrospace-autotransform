use std::fmt::{Debug, Display, Formatter};

use macrospace::pattern::StructuredBindings;
use proc_macro2::{TokenStream, Group};
use syn::buffer::{TokenBuffer, Cursor};
use syn::parse::{Parse, ParseStream};
use quote::{ToTokens, TokenStreamExt};

pub struct ExprBinding
{
	tokens: TokenBuffer
}

impl Clone for ExprBinding
{
	fn clone (&self) -> Self
	{
		Self {tokens: TokenBuffer::new2 (self . tokens . begin () . token_stream ())}
	}
}

impl Parse for ExprBinding
{
	fn parse (input: ParseStream <'_>) -> syn::Result <Self>
	{
		Ok (Self {tokens: TokenBuffer::new2 (input . parse ()?)})
	}
}

impl ToTokens for ExprBinding
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		let mut cursor = self . tokens . begin ();

		while let Some ((tt, next)) = cursor . token_tree ()
		{
			tokens . append (tt);

			cursor = next;
		}
	}
}

impl Debug for ExprBinding
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		f
			. debug_struct ("ExprBinding")
			. field ("tokens", &self . tokens . begin () . token_stream ())
			. finish ()
	}
}

impl Display for ExprBinding
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		Display::fmt (&self . tokens . begin () . token_stream (), f)
	}
}

impl PartialEq for ExprBinding
{
	fn eq (&self, other: &Self) -> bool
	{
		self . tokens . begin () . token_stream () . to_string ()
			== other . tokens . begin () . token_stream () . to_string ()
	}
}

impl ExprBinding
{
	pub fn substitute_arg <T> (&self, arg_tokens: &T, tokens: &mut TokenStream)
	where T: ToTokens
	{
		let mut cursor = self . tokens . begin ();

		while ! cursor . eof ()
		{
			cursor = Self::substitute_arg_cursor (cursor, arg_tokens, tokens);
		}
	}

	pub fn to_substituted_arg <T> (&self, arg_tokens: &T) -> TokenStream
	where T: ToTokens
	{
		let mut tokens = TokenStream::new ();

		self . substitute_arg (arg_tokens, &mut tokens);

		tokens
	}

	fn substitute_arg_cursor <'a, T>
	(
		cursor: Cursor <'a>,
		arg_tokens: &T,
		tokens: &mut TokenStream
	)
	-> Cursor <'a>
	where T: ToTokens
	{
		if let Some ((ident, next)) = cursor . ident () && ident == "arg"
		{
			arg_tokens . to_tokens (tokens);

			next
		}
		else if let Some ((mut group_cursor, delimiter, delim_span, next)) =
			cursor . any_group ()
		{
			let mut group_tokens = TokenStream::new ();

			while ! group_cursor . eof ()
			{
				group_cursor = Self::substitute_arg_cursor
				(
					group_cursor,
					arg_tokens,
					&mut group_tokens
				);
			}

			let mut group = Group::new (delimiter, group_tokens);
			group . set_span (delim_span . join ());

			tokens . append (group);

			next
		}
		else if let Some ((tt, next)) = cursor . token_tree ()
		{
			tokens . append (tt);

			next
		}
		else
		{
			cursor
		}
	}
}

pub type ExprBindings = StructuredBindings <ExprBinding>;
