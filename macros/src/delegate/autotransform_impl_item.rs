use proc_macro2::TokenStream;
use syn::{Generics, Path, Ident, Type, Token, braced};
use syn::parse::{Parse, ParseStream, Result};
use syn_derive::{Parse, ToTokens};
use quote::ToTokens;

#[derive (Clone, Debug, Parse, ToTokens)]
pub enum AutotransformImplItem
{
	#[parse (peek = Token! [trait])]
	Trait (AutotransformImplTrait),
	#[parse (peek = Token! [fn])]
	Fn (AutotransformImplFn)
}

impl AutotransformImplItem
{
	pub fn parse_all (input: ParseStream <'_>) -> Result <Vec <Self>>
	{
		let mut impl_items = Vec::new ();

		while ! input . is_empty ()
		{
			impl_items . push (input . parse ()?);
		}

		Ok (impl_items)
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AssocTypeTranslation
{
	pub type_token: Token! [type],
	pub ident: Ident,
	pub generics: Generics,
	pub eq_token: Token! [=],
	pub ty: Type,
	pub semicolon_token: Token! [;]
}

#[derive (Clone, Debug)]
pub struct AssocTypeTranslations
{
	pub brace_token: syn::token::Brace,
	pub translations: Vec <AssocTypeTranslation>
}

impl Parse for AssocTypeTranslations
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let content;
		let brace_token = braced! (content in input);

		let mut translations = Vec::new ();

		while ! content . is_empty ()
		{
			translations . push (content . parse ()?);
		}

		Ok (Self {brace_token, translations})
	}
}

impl ToTokens for AssocTypeTranslations
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		self . brace_token . surround
		(
			tokens,
			|inner_tokens|
			for translation in &self . translations
			{
				translation . to_tokens (inner_tokens);
			}
		);
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub enum ImplTraitBody
{
	#[parse (peek = Token! [;])]
	None (Token! [;]),
	#[parse (peek = syn::token::Brace)]
	Some (AssocTypeTranslations)
}

#[derive (Clone, Debug)]
pub struct AutotransformImplTrait
{
	pub trait_token: Token! [trait],
	pub generics: Generics,
	pub trait_path: Path,
	// where clause
	pub body: ImplTraitBody
}

impl Parse for AutotransformImplTrait
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let trait_token = input . parse ()?;
		let mut generics: Generics = input . parse ()?;
		let trait_path = input . parse ()?;

		generics . where_clause = input . parse ()?;

		let body = input . parse ()?;

		Ok (Self {trait_token, generics, trait_path, body})
	}
}

impl ToTokens for AutotransformImplTrait
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		self . trait_token . to_tokens (tokens);
		self . generics . to_tokens (tokens);
		self . trait_path . to_tokens (tokens);
		self . generics . where_clause . to_tokens (tokens);
		self . body . to_tokens (tokens);
	}
}

#[derive (Clone, Debug)]
pub struct AutotransformImplFn
{
	pub fn_token: Token! [fn],
	pub generics: Generics,
	pub fn_path: Path,
	// where clause
	pub semicolon_token: Token! [;]
}

impl Parse for AutotransformImplFn
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let fn_token = input . parse ()?;
		let mut generics: Generics = input . parse ()?;
		let fn_path = input . parse ()?;

		generics . where_clause = input . parse ()?;

		let semicolon_token = input . parse ()?;

		Ok (Self {fn_token, generics, fn_path, semicolon_token})
	}
}

impl ToTokens for AutotransformImplFn
{
	fn to_tokens (&self, tokens: &mut TokenStream)
	{
		self . fn_token . to_tokens (tokens);
		self . generics . to_tokens (tokens);
		self . fn_path . to_tokens (tokens);
		self . generics . where_clause . to_tokens (tokens);
		self . semicolon_token . to_tokens (tokens);
	}
}
