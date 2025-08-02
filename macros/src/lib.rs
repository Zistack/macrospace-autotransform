use proc_macro::TokenStream;

mod define_autotransform;

#[proc_macro]
pub fn define_autotransform (input: TokenStream) -> TokenStream
{
	define_autotransform::define_autotransform_impl (input)
}

mod group_autotransforms;

#[proc_macro]
pub fn group_autotransforms (input: TokenStream) -> TokenStream
{
	group_autotransforms::group_autotransforms_impl (input)
}

#[doc (hidden)]
#[proc_macro]
pub fn __group_autotransforms_inner__ (input: TokenStream) -> TokenStream
{
	group_autotransforms::group_autotransforms_inner_impl (input)
}

mod delegate;

#[doc (hidden)]
#[proc_macro]
pub fn __impl_fn__ (input: TokenStream) -> TokenStream
{
	delegate::impl_fn::impl_fn_impl (input)
}

#[doc (hidden)]
#[proc_macro]
pub fn __impl_trait__ (input: TokenStream) -> TokenStream
{
	delegate::impl_trait::impl_trait_impl (input)
}

#[proc_macro]
pub fn delegate (input: TokenStream) -> TokenStream
{
	delegate::delegate::delegate_impl (input)
}

#[doc (hidden)]
#[proc_macro]
pub fn __post_gather_delegate__ (input: TokenStream) -> TokenStream
{
	delegate::delegate::post_gather_delegate_impl (input)
}
