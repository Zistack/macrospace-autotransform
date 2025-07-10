fn impl_fn
(
	generics: Generics,
	fn_path: Path,
	orig_fn: ItemFn,
	to_delegate_transform: AutotransformBank,
	from_delegate_transform: AutotransformBank
)
-> Result <TokenStream>
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
			generics as orig_generics,
			inputs,
			output
		},
		...
	}
		= orig_fn;

	let fn_path_arguments = get_path_arguments (fn_path)?;
	let substitutions = Substitutions::try_from_generics
	(
		&orig_generics . params,
		&fn_path_arguments
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

	let inputs: Punctuated <FnArg, Token! [,]> = inputs
		. into_iterator ()
		. map (|fn_arg| substitute . fold_fn_arg (fn_arg))
		. collect ();

	let mut new_inputs: Punctuated <FnArg, Token! [,]>::new ();
	let mut arg_expressions: Punctuated <Expr, Token! [,]>::new ();

	for input in inputs
	{
		match input
		{
			Receiver (input) => return Err
			(
				Error::new_spanned
				(
					input,
					"fn items should not have receiver types"
				)
			),
			Typed (PatType {attrs, pat, ty, ..) =>
			{
				let (transformed_type, closure_expr) = to_delegate_transform
					. try_apply_reverse (input . ty . to_token_stream ())?;

				let transformed_fn_arg: FnArg =
					parse_quote! (#attrs #pat : #transformed_type);

				let transformed_expr: Expr =
					parse_quote! ((#closure_expr) (#pat));

				new_inputs . push (transformed_fn_arg);
				arg_expression . push (transformed_expr);
			}
		}
	}

	let fn_expr = as_prefix (fn_path);

	let call_expr = quote! (#fn_expr (#arg_expressions))

	let call_expr = match asyncness
	{
		Some (_) => quote! (#call_expr . await),
		None => call_expr
	};

	let output = substitute . fold_return_type (output);

	let (new_output, body_expr) = match output
	{
		ReturnType::Default => (ReturnType::Default, call_expr),
		ReturnType::Type (arrow_token, ty) =>
		{
			let (transformed_type, closure_expr) = from_delegate_transform
				. try_apply (ty . to_token_stream ())?;

			let transformed_output = ReturnType::Type
			(
				Default::default (),
				transformed_type
			);

			let body_expr = quote! ((#closure_expr) (#call_expr));

			(transformed_output, body_expr)
		}
	}

	let (impl_generics, _, where_clause) = generics . split_for_impl ();

	let tokens = quote!
	{
		#attrs
		#vis #constness #asyncness #unsafety #abi
		fn #ident #impl_generics (#new_inputs) #new_output
		{
			#body_expr
		}
	}l

	Ok (tokens)
}

fn impl_method (autotransform_bank: AutotransformBank, sig: Signature)
