use syn::{Type, parse2, parse_quote};
use quote::{ToTokens, format_ident};

use macrospace_autotransform_core::{
	Autotransform,
	AutotransformBank,
	SpecializationBindings
};

#[test]
fn substitute_and_match () -> syn::Result <()>
{
	let mut autotransform: Autotransform = parse_quote!
	{
		autotransform OwnedExponentialDistributionToSample
		[ExponentialDistribution <$S: type, $D: type>]
			-> [Sample <$S: type, $D: type, 1>]
		{
			arg . hyperparameters
		}
	};

	autotransform . weak_validate ()?;

	let mut specialization_bindings = SpecializationBindings::new ();

	specialization_bindings . add_value_binding
	(
		format_ident! ("S"),
		parse_quote! (<A as Accumulatable <S>>::Accumulator)
	) . unwrap ();
	specialization_bindings . add_value_binding
	(
		format_ident! ("D"),
		parse_quote! (<A as Accumulatable <D>>::Accumulator)
	) . unwrap ();

	autotransform
		. specialize (&specialization_bindings)
		. map_err (Into::<syn::Error>::into)?;

	let autotransform_bank = AutotransformBank::from (vec! (autotransform));

	let ty: Type = parse_quote!
	(
		ExponentialDistribution
		<
			<A as Accumulatable <S>>::Accumulator,
			<A as Accumulatable <D>>::Accumulator
		>
	);

	let expected_ty: Type = parse_quote!
	(
		Sample
		<
			<A as Accumulatable <S>>::Accumulator,
			<A as Accumulatable <D>>::Accumulator,
			1
		>
	);

	let output_tokens = autotransform_bank
		. try_apply_type_forward (&ty . to_token_stream ())
		. map_err (Into::<syn::Error>::into)?
		. expect ("Transform should have applied");

	assert_eq! (expected_ty, parse2 (output_tokens)?);

	Ok (())
}
