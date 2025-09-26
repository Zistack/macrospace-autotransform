use syn::{Ident, Path, PathSegment, PathArguments, Token};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn_derive::{Parse, ToTokens};

use macrospace_autotransform_core::SpecializationBinding;

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformParameterAssignment
{
	pub ident: Ident,
	pub eq_token: Token! [=],
	pub value: SpecializationBinding
}

#[derive (Clone, Debug, ToTokens)]
pub struct AutotransformParameters
{
	pub l_angle_token: Token! [<],
	pub assignments: Punctuated <AutotransformParameterAssignment, Token! [,]>,
	pub r_angle_token: Token! [>]
}

impl Parse for AutotransformParameters
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let l_angle_token = input . parse ()?;

		let mut assignments = Punctuated::new ();

		loop
		{
			if input . peek (Token! [>]) { break; }

			let assignment = input . parse ()?;
			assignments . push_value (assignment);

			let lookahead = input . lookahead1 ();

			if lookahead . peek (Token! [>]) { break; }

			if lookahead . peek (Token! [,])
			{
				assignments . push_punct (input . parse ()?);
			}
			else
			{
				return Err (lookahead . error ())
			}
		}

		let r_angle_token = input . parse ()?;

		Ok (Self {l_angle_token, assignments, r_angle_token})
	}
}

#[derive (Clone, Debug, ToTokens)]
pub struct AutotransformPath
{
	pub autotransform_path: Path,
	pub autotransform_parameters: Option <AutotransformParameters>
}

impl Parse for AutotransformPath
{
	fn parse (input: ParseStream <'_>) -> Result <Self>
	{
		let leading_colon: Option <Token! [::]> = input . parse ()?;

		let mut segments = Punctuated::<PathSegment, Token! [::]>::new ();

		let autotransform_parameters = loop
		{
			let ident = input . parse ()?;
			let path_segment = PathSegment
			{
				ident: ident,
				arguments: PathArguments::None
			};

			segments . push_value (path_segment);

			let lookahead = input . lookahead1 ();

			if lookahead . peek (Token! [::])
			{
				segments . push_punct (input . parse ()?);
				continue
			}

			if lookahead . peek (Token! [<])
			{
				break Some (input . parse ()?);
			}

			if lookahead . peek (Token! [,]) || input . is_empty ()
			{
				break None;
			}

			return Err (lookahead . error ());
		};

		let autotransform_path = Path {leading_colon, segments};

		Ok (Self {autotransform_path, autotransform_parameters})
	}
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformPaths
{
	#[parse (Punctuated::parse_terminated)]
	pub paths: Punctuated <AutotransformPath, Token! [,]>
}
