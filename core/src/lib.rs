mod bindings;

mod autotransform;
pub use autotransform::*;

mod autotransform_group;
pub use autotransform_group::*;

mod autotransform_bank;
pub use autotransform_bank::*;

pub mod kw
{
	syn::custom_keyword! (autotransform);
}
