use std::marker::PhantomData;

use proc_macro2::TokenStream;
use syn::{Type, Token, parse_quote};
use syn::parse::{Parser, ParseStream};

use super::ref_status::RefStatus;
use super::util::{TranslationTarget, Base, Delegated};
use super::get_translation::GetTranslation;

#[derive (Clone, Debug)]
pub struct ReceiverTypes
{
	receiver_type: Type,
	delegated_receiver_type: Type
}

impl ReceiverTypes
{
	pub fn new (receiver_type: Type, delegated_receiver_type: Type) -> Self
	{
		Self {receiver_type, delegated_receiver_type}
	}

	pub fn translate_base (&self) -> ReceiverTranslator <'_, Base>
	{
		ReceiverTranslator::new (self)
	}

	pub fn translate_delegated (&self) -> ReceiverTranslator <'_, Delegated>
	{
		ReceiverTranslator::new (self)
	}
}

#[derive (Debug)]
pub struct ReceiverTranslator <'a, D>
{
	receiver_types: &'a ReceiverTypes,
	_d: PhantomData <D>
}

impl <'a, D> ReceiverTranslator <'a, D>
{
	fn new (receiver_types: &'a ReceiverTypes) -> Self
	{
		Self {receiver_types, _d: PhantomData::default ()}
	}
}

impl <'a, D> Copy for ReceiverTranslator <'a, D>
{
}

impl <'a, D> Clone for ReceiverTranslator <'a, D>
{
	fn clone (&self) -> Self
	{
		*self
	}
}

impl <'a, D> GetTranslation for ReceiverTranslator <'a, D>
where D: TranslationTarget
{
	fn get_translation (self, ty_tokens: TokenStream) -> Option <Type>
	{
		let parser = |input: ParseStream <'_>|
		{
			let ref_status: RefStatus = input . parse ()?;

			let _self_token: Token! [Self] = input . parse ()?;

			let translated_type = D::select
			(
				&self . receiver_types . receiver_type,
				&self . receiver_types . delegated_receiver_type
			);

			Ok (parse_quote! (#ref_status #translated_type))
		};

		parser . parse2 (ty_tokens) . ok ()
	}
}
