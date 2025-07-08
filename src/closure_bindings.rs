use std::collections::HashMap;
use std::convert::Infallible;

use proc_macro2::TokenStream;
use syn::{Ident, Expr};
use quote::{ToTokens, quote};

use macrospace::pattern::{SubstitutionBindings, UntypedParameter};

pub struct ClosureBindings
{
	map: HashMap <Ident, Expr>
}

impl ClosureBindings
{
	pub fn new () -> Self
	{
		Self {map: HashMap::new ()}
	}

	pub fn insert (&mut self, ident: Ident, expr: Expr)
	{
		self . map . insert (ident, expr);
	}
}

impl SubstitutionBindings <UntypedParameter> for ClosureBindings
{
	type Error = Infallible;

	fn write_parameter_tokens
	(
		&self,
		parameter: UntypedParameter,
		tokens: &mut TokenStream
	)
	-> Result <(), Self::Error>
	{
		match self . map . get (&parameter . ident)
		{
			Some (expr) =>
			{
				expr . to_tokens (tokens);
			},
			None =>
			{
				tokens . extend (quote! (|x|{x}));
			}
		}

		Ok (())
	}
}
