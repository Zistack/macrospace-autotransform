use macrospace::{generate_macrospace_invokation, parse_args};
use macrospace::generics::combine_generics;
use syn::{parse_quote};
use syn::parse::{Parser, Result, Error};
use quote::quote;

use macrospace_autotransform_core::Autotransform;

use super::{
	UserAutotransformImplBlock,
	PreGatherImplBlock,
	PostGatherImplBlock,
	AutotransformImplItem,
	AutotransformImplTrait,
	AutotransformImplFn
};

fn delegate_inner (impl_blocks: Vec <UserAutotransformImplBlock>)
-> proc_macro2::TokenStream
{
	let mut tokens = proc_macro2::TokenStream::new ();

	for impl_block in impl_blocks
	{
		let
		(
			to_delegate_transforms,
			from_delegate_transforms,
			pre_gather_block
		)
			= impl_block . into_pre_gather ();

		let transform_args = to_delegate_transforms
			. paths
			. into_iter ()
			. chain (from_delegate_transforms . paths)
			. map (|path| parse_quote! (#path: autotransform));

		tokens . extend
		(
			generate_macrospace_invokation
			(
				parse_quote! (macrospace_autotransform::_post_gather_delegate__),
				transform_args,
				pre_gather_block
			)
		);
	}

	tokens
}

fn try_delegate_impl (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	Ok (delegate_inner (UserAutotransformImplBlock::parse_all . parse (input)?))
}

pub fn delegate_impl (input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
	try_delegate_impl (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}

fn post_gather_delegate_inner (impl_block: PostGatherImplBlock)
-> Result <proc_macro2::TokenStream>
{
	let mut tokens = proc_macro2::TokenStream::new ();

	let PostGatherImplBlock
	{
		impl_items,
		receiver_type_path,
		to_delegate_transforms,
		from_delegate_transforms,
		..
	}
		= impl_block;

	for impl_item in impl_items
	{
		match (impl_item, &receiver_type_path)
		{
			(AutotransformImplItem::Trait (impl_trait), Some (receiver_type_path)) =>
			{
				let AutotransformImplTrait
				{
					generics,
					trait_path,
					body,
					..
				}
					= impl_trait;

				let generics = combine_generics
				([
					impl_block . generics . clone (),
					generics
				]);

				let (impl_generics, _, where_clause) = generics . split_for_impl ();

				tokens . extend
				(
					generate_macrospace_invokation
					(
						parse_quote! (macrospace_autotransform::__impl_trait__),
						[parse_quote! (#trait_path: trait)],
						quote!
						(
							trait #impl_generics #trait_path
							for #receiver_type_path
							with [#to_delegate_transforms] -> [#from_delegate_transforms]
							#where_clause #body
						)
					)
				);
			},
			(AutotransformImplItem::Trait (impl_trait), None) =>
			{
				tokens . extend
				(
					Error::new_spanned
					(
						impl_trait,
						"cannot delegate trait implementation without a receiver type"
					)
						. into_compile_error ()
				);
			},
			(AutotransformImplItem::Fn (impl_fn), Some (_)) =>
			{
				tokens . extend
				(
					Error::new_spanned
					(
						impl_fn,
						"cannot delegate method implementations"
					)
						. into_compile_error ()
				);
			},
			(AutotransformImplItem::Fn (impl_fn), None) =>
			{
				let AutotransformImplFn
				{
					generics,
					fn_path,
					..
				}
					= impl_fn;

				let generics = combine_generics
				([
					impl_block . generics . clone (),
					generics
				]);

				let (impl_generics, _, where_clause) = generics . split_for_impl ();

				tokens . extend
				(
					generate_macrospace_invokation
					(
						parse_quote! (macrospace_autotransform::__impl_fn__),
						[parse_quote! (#fn_path: fn)],
						quote!
						(
							fn #impl_generics #fn_path
							with [#to_delegate_transforms] -> [#from_delegate_transforms]
							#where_clause;
						)
					)
				);
			}
		}
	}

	Ok (tokens)
}

fn try_post_gather_delegate_impl (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let (tokens, pre_gather_impl_block): (proc_macro2::TokenStream, PreGatherImplBlock)
		= parse_args! (1, input)?;

	let autotransforms = Autotransform::parse_all . parse2 (tokens)?;

	let post_gather_impl_block =
		pre_gather_impl_block . try_into_post_gather (autotransforms)?;

	Ok (post_gather_delegate_inner (post_gather_impl_block)?)
}

pub fn post_gather_delegate_impl (input: proc_macro::TokenStream)
-> proc_macro::TokenStream
{
	try_post_gather_delegate_impl (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}
