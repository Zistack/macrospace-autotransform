use syn::Type;

pub trait TranslationTarget
{
	fn select <'a> (ty: &'a Type, delegated_ty: &'a Type) -> &'a Type;
}

pub struct Base;

impl TranslationTarget for Base
{
	fn select <'a> (ty: &'a Type, _delegated_ty: &'a Type) -> &'a Type
	{
		ty
	}
}

pub struct Delegated;

impl TranslationTarget for Delegated
{
	fn select <'a> (_ty: &'a Type, delegated_ty: &'a Type) -> &'a Type
	{
		delegated_ty
	}
}
