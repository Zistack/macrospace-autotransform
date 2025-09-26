use syn::{Type, Token, parse2, parse_quote};
use quote::quote;

use macrospace_autotransform_core::{AutotransformBank, ApplicationError};

#[test]
fn match_tuple () -> Result <(), ApplicationError>
{
	let literal_pound: Token! [#] = Default::default ();

	let tuple_transform_bank: AutotransformBank = parse_quote!
	(
		autotransform Tuple
		[($[i]($T: inner_type),*)] -> [($[i]($T: inner_type),*)]
		{{
			let __tuple_x__ = arg;
			($[i]($T (__tuple_x__ . $#literal_pound i)),*)
		}}
		autotransform RToS [R] -> [S] { arg . into () }
	);

	let apply_result = tuple_transform_bank
		. try_apply_type_forward (&quote! ((R, S, T)))?;

	match apply_result
	{
		Some (transformed_ty) => assert_eq!
		(
			parse2::<Type> (transformed_ty)?,
			parse_quote! ((S, S, T))
		),
		None => panic! ("Autotransform did not transform type")
	}

	Ok (())
}
