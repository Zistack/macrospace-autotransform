mod kw
{
	syn::custom_keyword! (with);
}

mod autotransform_impl_block;
use autotransform_impl_block::*;

mod autotransform_impl_item;
use autotransform_impl_item::*;

mod type_transformer;
use type_transformer::*;

pub mod impl_fn;

pub mod impl_trait;

pub mod delegate;
