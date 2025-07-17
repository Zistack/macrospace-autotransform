use proc_macro2::TokenStream;
use syn::Type;

use super::combined_translator::CombinedTranslator;

pub trait GetTranslation
{
	fn get_translation (self, ty_tokens: TokenStream) -> Option <Type>;

	fn chain <T> (self, other: T) -> CombinedTranslator <Self, T>
	where
		Self: Copy,
		T: Copy + GetTranslation
	{
		CombinedTranslator::new (self, other)
	}
}
