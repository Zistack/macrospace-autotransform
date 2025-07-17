use proc_macro2::TokenStream;
use syn::Type;

use super::get_translation::GetTranslation;

#[derive (Copy, Clone, Debug)]
pub struct CombinedTranslator <T, R>
{
	first_translator: T,
	second_translator: R
}

impl <T, R> CombinedTranslator <T, R>
{
	pub fn new (first_translator: T, second_translator: R)
	-> Self
	{
		Self {first_translator, second_translator}
	}
}

impl <T, R> GetTranslation for CombinedTranslator <T, R>
where
	T: Copy + GetTranslation,
	R: Copy + GetTranslation
{
	fn get_translation (self, ty_tokens: TokenStream) -> Option <Type>
	{
		self
			. first_translator
			. get_translation (ty_tokens . clone ())
			. or_else
		(
			|| self . second_translator . get_translation (ty_tokens)
		)
	}
}
