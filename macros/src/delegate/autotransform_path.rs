use macrospace::substitute::Argument;
use syn::{Ident, Path, PathSegment, PathArguments, Token};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn_derive::{Parse, ToTokens};

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformParameterAssignment
{
	pub ident: Ident,
	pub eq_token: Token! [=],
	pub value: Argument
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct AutotransformParameters
{
	pub l_angle_token: Token! [<],
	#[parse (Punctuated::parse_terminated)]
	pub assignments: Punctuated <AutotransformParameterAssignment, Token! [,]>,
	pub r_angle_token: Token! [>]
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
