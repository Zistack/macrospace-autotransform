use macrospace::parse_args;
use macrospace::path_utils::without_arguments;
use macrospace::generics::get_path_arguments;
use macrospace::substitute::Substitutions;
use syn::{
	Type,
	Path,
	TraitItemConst,
	TraitItemFn,
	Signature,
	Expr,
	FnArg,
	Receiver,
	PatType,
	ReturnType,
	TraitItemType,
	Generics,
	GenericArgument,
	ItemTrait,
	TraitItem,
	Token,
	parse2,
	bracketed,
	parse_quote
};
use syn::parse::{Parse, ParseStream, Result, Error};
use syn::punctuated::Punctuated;
use syn::fold::Fold;
use quote::{ToTokens, quote};

use macrospace_autotransform_core::AutotransformBank;

use super::{kw, TypeTransformer, ImplTraitBody};

fn impl_associated_const
(
	delegated_receiver_type: &Type,
	delegated_trait_path: &Path,
	type_transformer: &mut TypeTransformer,
	trait_item: TraitItemConst,
	to_delegate_transforms: &AutotransformBank,
)
-> Result <proc_macro2::TokenStream>
{
	let TraitItemConst
	{
		attrs,
		ident,
		generics,
		ty,
		..
	}
		= trait_item;

	let mut scrubber = Substitutions::scrubber ("__from_def_", &generics . params);
	let generics = scrubber . fold_generics (generics);
	let generics = type_transformer . fold_generics (generics);

	let ty = scrubber . fold_type (ty);
	let ty = type_transformer . fold_type (ty);

	let (transformed_ty, transformed_expr) = match to_delegate_transforms
		. try_apply_forward
		(
			&ty . to_token_stream (),
			&parse_quote! (<#delegated_receiver_type as #delegated_trait_path>::#ident)
		)
		. map_err (|e| Into::<Error>::into (e))?
	{
		Some ((transformed_ty, transformed_expr)) =>
		(
			transformed_ty,
			transformed_expr
		),
		None =>
		(
			ty . to_token_stream (),
			quote! (<#delegated_receiver_type as #delegated_trait_path>::#ident)
		)
	};

	let (impl_generics, _, where_clause) =
		generics . split_for_impl ();

	let tokens = quote!
	{
		#(#attrs)*
		const #ident #impl_generics: #transformed_ty = #transformed_expr
		#where_clause;
	};

	Ok (tokens)
}

fn impl_associated_fn
(
	delegated_receiver_type: &Type,
	delegated_trait_path: &Path,
	type_transformer: &mut TypeTransformer,
	trait_item: TraitItemFn,
	to_delegate_transforms: &AutotransformBank,
	from_delegate_transforms: &AutotransformBank
)
-> Result <proc_macro2::TokenStream>
{
	let TraitItemFn
	{
		attrs,
		sig: Signature
		{
			constness,
			asyncness,
			unsafety,
			abi,
			ident,
			generics,
			mut inputs,
			output,
			..
		},
		..
	}
		= trait_item;

	let mut scrubber = Substitutions::scrubber ("__from_def_", &generics . params);
	let generics = scrubber . fold_generics (generics);
	let generics = type_transformer . fold_generics (generics);

	let mut arg_expressions = Punctuated::<Expr, Token! [,]>::new ();

	for input in &mut inputs
	{
		// The clone here could arguably be replaced with some use of
		// replace_with!.
		*input = type_transformer . fold_fn_arg
		(
			scrubber . fold_fn_arg (input . clone ())
		);

		match input
		{
			FnArg::Receiver (Receiver {self_token, ty, ..}) =>
				match to_delegate_transforms . try_apply_forward
			(
				&ty . to_token_stream (),
				&parse_quote! (#self_token)
			)
				. map_err (|e| Into::<Error>::into (e))?
			{
				Some ((_, transformed_expr)) =>
					arg_expressions . push (parse2 (transformed_expr)?),
				None => arg_expressions . push (parse_quote! (#self_token))
			},
			FnArg::Typed (PatType {pat, ty, ..}) =>
				match to_delegate_transforms . try_apply_forward
			(
				&ty . to_token_stream (),
				&parse_quote! (#pat)
			)
				. map_err (|e| Into::<Error>::into (e))?
			{
				Some ((_, transformed_expr)) =>
					arg_expressions . push (parse2 (transformed_expr)?),
				None => arg_expressions . push (parse_quote! (#pat))
			}
		}
	}

	let call_expr = parse_quote!
	(
		<#delegated_receiver_type as #delegated_trait_path>::#ident (#arg_expressions)
	);

	let call_expr = match asyncness
	{
		Some (_) => parse_quote! (#call_expr . await),
		None => call_expr
	};

	let output = scrubber . fold_return_type (output);
	let output = type_transformer . fold_return_type (output);

	let body_expr = match &output
	{
		ReturnType::Default => call_expr,
		ReturnType::Type (_, ty) => match from_delegate_transforms
			. try_apply_backward (&ty . to_token_stream (), &call_expr)
			. map_err (|e| Into::<Error>::into (e))?
		{
			Some ((_, transformed_expr)) =>
				parse2 (transformed_expr)?,
			None => call_expr
		}
	};

	let (impl_generics, _, where_clause) = generics . split_for_impl ();

	let tokens = quote!
	{
		#(#attrs)*
		#constness #asyncness #unsafety #abi
		fn #ident #impl_generics (#inputs) #output
		#where_clause
		{
			#body_expr
		}
	};

	Ok (tokens)
}

fn impl_associated_type
(
	delegated_receiver_type: &Type,
	delegated_trait_path: &Path,
	type_transformer: &mut TypeTransformer,
	trait_item: TraitItemType
)
-> Result <proc_macro2::TokenStream>
{
	let TraitItemType
	{
		attrs,
		ident,
		generics,
		..
	}
		= trait_item;

	let mut scrubber = Substitutions::scrubber ("__from_def_", &generics . params);
	let generics = scrubber . fold_generics (generics);
	let generics = type_transformer . fold_generics (generics);

	let tokens = match type_transformer . associated_types . get (&ident)
	{
		Some ((user_generics, user_ty)) =>
		{
			let where_clause = generics . where_clause . map
			(
				|where_clause|
				{
					let mut substitutions = Substitutions::from_generic_parameters
					(
						&generics . params,
						&user_generics . params
					);

					substitutions . fold_where_clause (where_clause)
				}
			);

			quote!
			{
				#(#attrs)*
				type #ident #user_generics = #user_ty
				#where_clause;
			}
		},
		None =>
		{
			let (impl_generics, _, where_clause) = generics . split_for_impl ();

			quote!
			{
				#(#attrs)*
				type #ident #impl_generics =
					<#delegated_receiver_type as #delegated_trait_path>::#ident
				#where_clause;
			}
		}
	};

	Ok (tokens)
}

fn impl_trait_inner
(
	receiver_type: Type,
	mut generics: Generics,
	trait_path: Path,
	orig_trait: ItemTrait,
	associated_type_assignments: ImplTraitBody,
	to_delegate_transforms: AutotransformBank,
	from_delegate_transforms: AutotransformBank
)
-> Result <proc_macro2::TokenStream>
{
	let delegated_receiver_type = match to_delegate_transforms
		. try_apply_type_forward (&receiver_type . to_token_stream ())
		. map_err (Into::<Error>::into)?
	{
		Some (transformed_ty) => parse2 (transformed_ty)?,
		None => receiver_type . clone ()
	};

	let mut trait_path_arguments =
		Punctuated::<GenericArgument, Token! [,]>::new ();

	for path_argument in get_path_arguments (&trait_path)?
	{
		let new_path_argument = match path_argument
		{
			GenericArgument::Type (ty) => match to_delegate_transforms
				. try_apply_type_forward (&ty . to_token_stream ())
				. map_err (Into::<Error>::into)?
			{
				Some (transformed_ty) =>
					GenericArgument::Type (parse2 (transformed_ty)?),
				None => GenericArgument::Type (ty)
			},
			a @ _ => a
		};

		trait_path_arguments . push (new_path_argument);
	}

	let stripped_trait_path = without_arguments (trait_path . clone ());

	let delegated_trait_path = if trait_path_arguments . is_empty ()
	{
		stripped_trait_path
	}
	else
	{
		parse_quote! (#stripped_trait_path <#trait_path_arguments>)
	};

	let mut scrubber = Substitutions::scrubber
	(
		"__from_def_",
		&orig_trait . generics . params
	);

	let ItemTrait
	{
		unsafety,
		generics: orig_generics,
		items,
		..
	}
		= scrubber . fold_item_trait (orig_trait);

	let trait_path_arguments = get_path_arguments (&trait_path)?;
	let mut substitutions = Substitutions::try_from_path_arguments
	(
		&orig_generics . params,
		&trait_path_arguments
	)?;

	if let Some (where_clause) = orig_generics . where_clause
	{
		generics
			. make_where_clause ()
			. predicates
			. extend
			(
				where_clause
					. predicates
					. into_iter ()
					. map
					(
						|predicate|
						substitutions . fold_where_predicate (predicate)
					)
			);
	}

	generics
		. make_where_clause ()
		. predicates
		. push (parse_quote! (#delegated_receiver_type: #delegated_trait_path));

	let mut type_transformer = TypeTransformer::new
	(
		receiver_type . clone (),
		scrubber . fold_path (trait_path . clone ())
	);

	if let ImplTraitBody::Some (translations) = associated_type_assignments
	{
		for translation in translations . translations
		{
			let ty = translation . ty;

			type_transformer . add_associated_type
			(
				translation . ident,
				translation . generics,
				ty
			);
		}
	}

	let mut item_impls = Vec::new ();

	for item in items
	{
		let item = substitutions . fold_trait_item (item);

		match item
		{
			TraitItem::Const (trait_item) => item_impls . push
			(
				impl_associated_const
				(
					&delegated_receiver_type,
					&delegated_trait_path,
					&mut type_transformer,
					trait_item,
					&to_delegate_transforms
				)?
			),
			TraitItem::Fn (trait_item) => item_impls . push
			(
				impl_associated_fn
				(
					&delegated_receiver_type,
					&delegated_trait_path,
					&mut type_transformer,
					trait_item,
					&to_delegate_transforms,
					&from_delegate_transforms
				)?
			),
			TraitItem::Type (trait_item) => item_impls . push
			(
				impl_associated_type
				(
					&delegated_receiver_type,
					&delegated_trait_path,
					&mut type_transformer,
					trait_item
				)?
			),
			TraitItem::Macro (trait_item) => return Err
			(
				Error::new_spanned
				(
					trait_item,
					"cannot delegate trait implementations for traits that use trait item macros"
				)
			),
			TraitItem::Verbatim (trait_item) => return Err
			(
				Error::new_spanned
				(
					trait_item,
					"cannot delegate trait implementations for traits with verbatim trait items"
				)
			),
			trait_item @ _ => return Err
			(
				Error::new_spanned
				(
					trait_item,
					"cannot delegate trait implementations for traits with unsupported trait items"
				)
			)
		}
	}

	let (impl_generics, _, where_clause) = generics . split_for_impl ();

	let tokens = quote!
	{
		#[automatically_derived]
		#unsafety impl #impl_generics #trait_path for #receiver_type
		#where_clause
		{
			#(#item_impls)*
		}
	};

	Ok (tokens)
}

#[derive (Clone, Debug)]
struct ImplTraitInput
{
	trait_token: Token! [trait],
	generics: Generics,
	trait_path: Path,
	for_token: Token! [for],
	receiver_type: Type,
	with_token: kw::with,
	to_bracket_token: syn::token::Bracket,
	to_delegate_transforms: AutotransformBank,
	arrow_token: Token! [->],
	from_bracket_token: syn::token::Bracket,
	from_delegate_transforms: AutotransformBank,
	// where clause
	body: ImplTraitBody
}

impl Parse for ImplTraitInput
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let trait_token = input . parse ()?;
		let mut generics: Generics = input . parse ()?;
		let trait_path = input . parse ()?;
		let for_token = input . parse ()?;
		let receiver_type = input . parse ()?;
		let with_token = input . parse ()?;

		let content;
		let to_bracket_token = bracketed! (content in input);
		let to_delegate_transforms = content . parse ()?;

		let arrow_token = input . parse ()?;

		let content;
		let from_bracket_token = bracketed! (content in input);
		let from_delegate_transforms = content . parse ()?;

		generics . where_clause = input . parse ()?;

		let body = input . parse ()?;

		Ok
		(
			Self
			{
				trait_token,
				generics,
				trait_path,
				for_token,
				receiver_type,
				with_token,
				to_bracket_token,
				to_delegate_transforms,
				arrow_token,
				from_bracket_token,
				from_delegate_transforms,
				body
			}
		)
	}
}

impl ToTokens for ImplTraitInput
{
	fn to_tokens (&self, tokens: &mut proc_macro2::TokenStream)
	{
		self . trait_token . to_tokens (tokens);
		self . generics . to_tokens (tokens);
		self . trait_path . to_tokens (tokens);
		self . for_token . to_tokens (tokens);
		self . receiver_type . to_tokens (tokens);
		self . with_token . to_tokens (tokens);

		self . to_bracket_token . surround
		(
			tokens,
			|inner_tokens|
			self . to_delegate_transforms . to_tokens (inner_tokens)
		);

		self . arrow_token . to_tokens (tokens);

		self . from_bracket_token . surround
		(
			tokens,
			|inner_tokens|
			self . from_delegate_transforms . to_tokens (inner_tokens)
		);

		self . generics . where_clause . to_tokens (tokens);

		self . body . to_tokens (tokens);
	}
}

fn try_impl_trait_impl (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let (orig_trait, tokens) = parse_args! (1, input)?;

	let ImplTraitInput
	{
		generics,
		trait_path,
		receiver_type,
		to_delegate_transforms,
		from_delegate_transforms,
		body: associated_type_assignments,
		..
	}
		= parse2 (tokens)?;

	impl_trait_inner
	(
		receiver_type,
		generics,
		trait_path,
		orig_trait,
		associated_type_assignments,
		to_delegate_transforms,
		from_delegate_transforms
	)
}

pub fn impl_trait_impl (input: proc_macro::TokenStream)
-> proc_macro::TokenStream
{
	try_impl_trait_impl (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}
