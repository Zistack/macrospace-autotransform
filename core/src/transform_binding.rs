use std::borrow::BorrowMut;

use macrospace::pattern::StructuredBinding;
use proc_macro2::TokenStream;
use syn::{parse_quote, parse2};
use quote::ToTokens;

use crate::{TypePatternBinding, ExprBinding, ApplicationError};

pub fn transform_binding <F, FF>
(
	binding: &mut StructuredBinding <TypePatternBinding>,
	mut apply_inner: F
)
-> Result <StructuredBinding <ExprBinding>, ApplicationError>
where
	F: BorrowMut <FF>,
	FF: FnMut (TokenStream)
		-> Result <Option <(TokenStream, ExprBinding)>, ApplicationError>
{
	match binding
	{
		StructuredBinding::Value (TypePatternBinding::InnerType (ty)) =>
		{
			match (apply_inner . borrow_mut ()) (ty . to_token_stream ())?
			{
				Some ((transformed_ty, expr_binding)) =>
				{
					*ty = parse2 (transformed_ty)?;

					Ok (StructuredBinding::Value (expr_binding))
				},
				None => Ok (StructuredBinding::Value (parse_quote! (arg)))
			}
		},
		StructuredBinding::Index (i) =>
			Ok (StructuredBinding::Index (*i)),
		StructuredBinding::Value (_) =>
			Ok (StructuredBinding::Value (parse_quote! (arg))),
		StructuredBinding::Optional (option) => Ok
		(
			StructuredBinding::Optional
			(
				match option
				{
					Some (boxed_binding) => Some
					(
						Box::new
						(
							transform_binding (&mut *boxed_binding, apply_inner)?
						)
					),
					None => None
				}
			)
		),
		StructuredBinding::ZeroOrMore (vec) =>
		{
			let mut expr_binding_vec = Vec::new ();

			for ty_binding in vec
			{
				expr_binding_vec . push
				(
					transform_binding::<&mut FF, FF>
					(
						ty_binding,
						apply_inner . borrow_mut ()
					)?
				);
			}

			Ok (StructuredBinding::ZeroOrMore (expr_binding_vec))
		},
		StructuredBinding::OneOrMore (vec) =>
		{
			let mut expr_binding_vec = Vec::new ();

			for ty_binding in vec
			{
				expr_binding_vec . push
				(
					transform_binding::<&mut FF, FF>
					(
						ty_binding,
						apply_inner . borrow_mut ()
					)?
				);
			}

			Ok (StructuredBinding::OneOrMore (expr_binding_vec))
		}
	}
}

pub fn transform_binding_type <F, FF>
(
	binding: &mut StructuredBinding <TypePatternBinding>,
	mut apply_inner: F
)
-> Result <(), ApplicationError>
where
	F: BorrowMut <FF>,
	FF: FnMut (TokenStream) -> Result <Option <TokenStream>, ApplicationError>
{
	match binding
	{
		StructuredBinding::Value (TypePatternBinding::InnerType (ty)) =>
		{
			match (apply_inner . borrow_mut ()) (ty . to_token_stream ())?
			{
				Some (transformed_ty) =>
				{
					*ty = parse2 (transformed_ty)?;

					Ok (())
				},
				None => Ok (())
			}
		},
		StructuredBinding::Optional (Some (boxed_binding)) =>
			transform_binding_type (&mut *boxed_binding, apply_inner),
		StructuredBinding::ZeroOrMore (vec) =>
		{
			for ty_binding in vec
			{
				transform_binding_type::<&mut FF, FF>
				(
					ty_binding,
					apply_inner . borrow_mut ()
				)?;
			}

			Ok (())
		},
		StructuredBinding::OneOrMore (vec) =>
		{
			for ty_binding in vec
			{
				transform_binding_type::<&mut FF, FF>
				(
					ty_binding,
					apply_inner . borrow_mut ()
				)?;
			}

			Ok (())
		},
		_ => Ok (())
	}
}
