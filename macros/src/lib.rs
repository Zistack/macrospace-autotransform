use proc_macro::TokenStream;

mod define_autotransform;

#[proc_macro]
pub fn define_autotransform (input: TokenStream) -> TokenStream
{
	define_autotransform::define_autotransform_impl (input)
}
