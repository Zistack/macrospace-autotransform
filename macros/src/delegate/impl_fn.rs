use macrospace::parse_args;
use macrospace::path_utils::{get_path_arguments, as_prefix};
use macrospace::substitute::Substitutions;
use macrospace::stream_utils::strip_spans;
use syn::{
	Generics,
	Path,
	ItemFn,
	Signature,
	FnArg,
	Expr,
	PatType,
	ReturnType,
	Token,
	parse2,
	bracketed,
	parse_quote
};
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream, Result, Error};
use syn::punctuated::Punctuated;
use quote::{ToTokens, quote};

use macrospace_autotransform_core::AutotransformBank;

use super::kw;

fn impl_fn_inner
(
	mut generics: Generics,
	fn_path: Path,
	orig_fn: ItemFn,
	to_delegate_transform: AutotransformBank,
	from_delegate_transform: AutotransformBank
)
-> Result <proc_macro2::TokenStream>
{
	let ItemFn
	{
		attrs,
		vis,
		sig: Signature
		{
			constness,
			asyncness,
			unsafety,
			abi,
			ident,
			generics: orig_generics,
			inputs,
			output,
			..
		},
		..
	}
		= orig_fn;

	let fn_path_arguments = get_path_arguments (&fn_path)?;
	let mut substitutions = Substitutions::try_from_path_arguments
	(
		&orig_generics . params,
		&fn_path_arguments . cloned () . unwrap_or_default ()
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

	let mut new_inputs = Punctuated::<FnArg, Token! [,]>::new ();
	let mut arg_expressions = Punctuated::<Expr, Token! [,]>::new ();

	for input in inputs
	{
		match substitutions . fold_fn_arg (input)
		{
			FnArg::Receiver (input) => return Err
			(
				Error::new_spanned
				(
					input,
					"fn items should not have receiver types"
				)
			),
			FnArg::Typed (PatType {attrs, pat, ty, ..}) =>
			{
				match to_delegate_transform . try_apply_backward
				(
					&ty . to_token_stream (),
				) . map_err (|e| Into::<Error>::into (e))?
				{
					Some ((transformed_ty, expr_binding)) =>
					{
						new_inputs . push
						(
							parse_quote! (#(#attrs)* #pat : #transformed_ty)
						);
						arg_expressions . push
						(
							parse2 (expr_binding . to_substituted_arg (&*pat))?
						);
					},
					None =>
					{
						new_inputs . push (parse_quote! (#(#attrs)* #pat : #ty));
						arg_expressions . push (parse_quote! (#pat));
					}
				}
			}
		}
	}

	let fn_expr = as_prefix (fn_path);

	let call_expr = quote! (#fn_expr (#arg_expressions));

	let call_expr = match asyncness
	{
		Some (_) => quote! (#call_expr . await),
		None => call_expr
	};

	let output = substitutions . fold_return_type (output);

	let (new_output, body_expr) = match output
	{
		ReturnType::Default => (ReturnType::Default, call_expr),
		ReturnType::Type (arrow_token, ty) => match from_delegate_transform
			. try_apply_forward
		(
			&ty . to_token_stream (),
		) . map_err (|e| Into::<Error>::into (e))?
		{
			Some ((transformed_ty, expr_binding)) =>
			{
				let transformed_output = ReturnType::Type
				(
					Default::default (),
					Box::new (parse2 (transformed_ty)?)
				);

				(
					transformed_output,
					parse2 (expr_binding . to_substituted_arg (&call_expr))?
				)
			},
			None => (ReturnType::Type (arrow_token, ty), call_expr)
		}
	};

	let (impl_generics, _, where_clause) = generics . split_for_impl ();

	let tokens = quote!
	{
		#[automatically_derived]
		#(#attrs)*
		#vis #constness #asyncness #unsafety #abi
		fn #ident #impl_generics (#new_inputs) #new_output
		#where_clause
		{
			#body_expr
		}
	};

	Ok (strip_spans (tokens))
}

#[derive (Clone, Debug)]
struct ImplFnInput
{
	fn_token: Token! [fn],
	generics: Generics,
	fn_path: Path,
	with_token: kw::with,
	to_bracket_token: syn::token::Bracket,
	to_delegate_transforms: AutotransformBank,
	arrow_token: Token! [->],
	from_bracket_token: syn::token::Bracket,
	from_delegate_transforms: AutotransformBank,
	// where clause
	semicolon_token: Token! [;]
}

impl Parse for ImplFnInput
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let fn_token = input . parse ()?;
		let mut generics: Generics = input . parse ()?;
		let fn_path = input . parse ()?;
		let with_token = input . parse ()?;

		let content;
		let to_bracket_token = bracketed! (content in input);
		let to_delegate_transforms = content . parse ()?;

		let arrow_token = input . parse ()?;

		let content;
		let from_bracket_token = bracketed! (content in input);
		let from_delegate_transforms = content . parse ()?;

		generics . where_clause = input . parse ()?;

		let semicolon_token = input . parse ()?;

		Ok
		(
			Self
			{
				fn_token,
				generics,
				fn_path,
				with_token,
				to_bracket_token,
				to_delegate_transforms,
				arrow_token,
				from_bracket_token,
				from_delegate_transforms,
				semicolon_token
			}
		)
	}
}

impl ToTokens for ImplFnInput
{
	fn to_tokens (&self, tokens: &mut proc_macro2::TokenStream)
	{
		self . fn_token . to_tokens (tokens);
		self . generics . to_tokens (tokens);
		self . fn_path . to_tokens (tokens);
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

		self . semicolon_token . to_tokens (tokens);
	}
}

fn try_impl_fn_impl (input: proc_macro::TokenStream)
-> Result <proc_macro2::TokenStream>
{
	let (orig_fn, tokens) = parse_args! (1, input)?;

	let ImplFnInput
	{
		generics,
		fn_path,
		to_delegate_transforms,
		from_delegate_transforms,
		..
	}
		= parse2 (tokens)?;

	impl_fn_inner
	(
		generics,
		fn_path,
		orig_fn,
		to_delegate_transforms,
		from_delegate_transforms
	)
}

pub fn impl_fn_impl (input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
	try_impl_fn_impl (input)
		. unwrap_or_else (Error::into_compile_error)
		. into ()
}
