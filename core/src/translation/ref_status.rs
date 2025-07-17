use syn::{Lifetime, Token};
use syn_derive::{Parse, ToTokens};

#[derive (Clone, Debug, Parse, ToTokens)]
pub struct Ref
{
	pub ampersand_token: Token! [&],
	pub lifetime: Option <Lifetime>,
	pub mut_token: Option <Token! [mut]>
}

#[derive (Clone, Debug, Parse, ToTokens)]
pub enum RefStatus
{
	#[parse (peek = Token! [&])]
	Ref (Ref),
	Owned
}
