use std::collections::hash_map::{HashMap, Entry};
use std::convert::Infallible;
use std::fmt::{Display, Formatter};

use proc_macro2::{TokenStream, Delimiter};
use syn::{Ident, Expr, Token, parse2};
use syn::buffer::Cursor;
use syn::token::Paren;
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

use macrospace::pattern::{
	CursorParse,
	ParameterCollector,
	SubstitutionBindings,
	MergeableBindings,
	ParameterBindingMismatch
};

mod kw
{
	syn::custom_keyword! (arg);
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct SubvalueParameter
{
	dollar_token: Token! [$],
	ident: Ident,
	#[syn (parenthesized)]
	paren_token: Paren,
	#[syn (in = paren_token)]
	arg_expr: Expr
}

impl CursorParse for SubvalueParameter
{
	fn parse_from_cursor (cursor: Cursor <'_>) -> Option <(Self, Cursor <'_>)>
	{
		let (dollar_token, cursor) = match cursor . punct ()
		{
			Some ((punct, cursor)) if punct . as_char () == '$' =>
			(
				syn::token::Dollar {spans: [punct . span ()]},
				cursor
			),
			_ => { return None; }
		};

		let (ident, cursor) = match cursor . ident ()
		{
			Some ((ident, cursor)) => (ident, cursor),
			_ => { return None; }
		};

		let (paren_cursor, paren_token, cursor) =
			match cursor . group (Delimiter::Parenthesis)
		{
			Some ((paren_cursor, paren_span, cursor)) =>
			(
				paren_cursor,
				syn::token::Paren {span: paren_span},
				cursor
			),
			_ => { return None; }
		};

		let arg_expr = match parse2 (paren_cursor . token_stream ())
		{
			Ok (expr) => expr,
			_ => { return None; }
		};

		Some ((Self {dollar_token, ident, paren_token, arg_expr}, cursor))
	}
}

impl Display for SubvalueParameter
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		f . write_fmt
		(
			format_args!
			(
				"`${} ({})`",
				self . ident,
				self . arg_expr . to_token_stream ()
			)
		)
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct ArgParameter
{
	dollar_token: Token! [$],
	arg_token: kw::arg
}

impl CursorParse for ArgParameter
{
	fn parse_from_cursor (cursor: Cursor <'_>) -> Option <(Self, Cursor <'_>)>
	{
		let (dollar_token, cursor) = match cursor . punct ()
		{
			Some ((punct, cursor)) if punct . as_char () == '$' =>
			(
				syn::token::Dollar {spans: [punct . span ()]},
				cursor
			),
			_ => { return None; }
		};

		let (arg_token, cursor) = match cursor . ident ()
		{
			Some ((ident, cursor)) if ident == "arg" =>
			(
				kw::arg {span: ident . span ()},
				cursor
			),
			_ => { return None; }
		};

		Some ((Self {dollar_token, arg_token}, cursor))
	}
}

impl Display for ArgParameter
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		f . write_str ("`$arg`")
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
#[parse (prefix = |input| input . parse::<Token! [$]> ())]
pub enum ExprParameter
{
	#[parse (peek = kw::arg)]
	Arg (ArgParameter),
	Subvalue (SubvalueParameter)
}

impl CursorParse for ExprParameter
{
	fn parse_from_cursor (cursor: Cursor <'_>) -> Option <(Self, Cursor <'_>)>
	{
		if let Some ((arg_parameter, cursor)) =
			ArgParameter::parse_from_cursor (cursor)
		{
			return Some ((Self::Arg (arg_parameter), cursor));
		}

		if let Some ((subvalue_parameter, cursor)) =
			SubvalueParameter::parse_from_cursor (cursor)
		{
			return Some ((Self::Subvalue (subvalue_parameter), cursor));
		}

		None
	}
}

impl Display for ExprParameter
{
	fn fmt (&self, f: &mut Formatter <'_>) -> Result <(), std::fmt::Error>
	{
		match self
		{
			Self::Arg (arg_parameter) => Display::fmt (arg_parameter, f),
			Self::Subvalue (subvalue_parameter) =>
				Display::fmt (subvalue_parameter, f)
		}
	}
}

#[derive (Default)]
pub struct ExprParameters
{
	params: HashMap <Ident, Expr>
}

impl ExprParameters
{
	pub fn get_arg_expr (&self, subvalue_ident: &Ident) -> Option <&Expr>
	{
		self . params . get (subvalue_ident)
	}
}


impl ParameterCollector <ExprParameter> for ExprParameters
{
	fn add_parameter (&mut self, parameter: ExprParameter)
	{
		if let ExprParameter::Subvalue (parameter) = parameter
		{
			self . params . insert (parameter . ident, parameter . arg_expr);
		}
	}
}

impl MergeableBindings for ExprParameters
{
	type Error = ParameterBindingMismatch <TokenStream>;

	fn try_merge (&mut self, other: Self) -> Result <(), Self::Error>
	{
		for (other_ident, other_expr) in other . params
		{
			match self . params . entry (other_ident . clone ())
			{
				Entry::Occupied (occupied) => if *occupied . get () != other_expr
				{
					return Err
					(
						ParameterBindingMismatch::new
						(
							other_ident,
							occupied . get () . to_token_stream (),
							other_expr . into_token_stream ()
						)
					);
				},
				Entry::Vacant (vacant) =>
				{
					vacant . insert (other_expr);
				}
			}
		}

		Ok (())
	}
}

pub struct ExprBindings
{
	arg: Expr,
	subvalue_map: HashMap <Ident, Expr>
}

impl ExprBindings
{
	pub fn new (arg: Expr) -> Self
	{
		Self {arg, subvalue_map: HashMap::new ()}
	}

	pub fn add_subvalue (&mut self, ident: Ident, subvalue: Expr)
	{
		self . subvalue_map . insert (ident, subvalue);
	}
}

impl SubstitutionBindings <ExprParameter> for ExprBindings
{
	type Error = Infallible;

	fn write_parameter_tokens
	(
		&self,
		parameter: ExprParameter,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match parameter
		{
			ExprParameter::Arg (_) => self . arg . to_tokens (tokens),
			ExprParameter::Subvalue (parameter) =>
			{
				match self . subvalue_map . get (&parameter . ident)
				{
					Some (value) => value . to_tokens (tokens),
					None => parameter . arg_expr . to_tokens (tokens)
				}
			}
		}

		Ok (())
	}
}
