use crate::{define_autotransform, group_autotransforms};

use crate as macrospace_autotransform;

define_autotransform!
{
	pub autotransform UnqualifiedResult
	[Result < $T: inner_type, $E: inner_type>]
		-> [Result < $T: inner_type, $E: inner_type>]
	{
		match $arg
		{
			Ok (ret) => Ok ($T (ret)),
			Err (err) => Err ($E (err))
		}
	}
}

define_autotransform!
{
	pub autotransform QualifiedResult
	[std::result::Result < $T: inner_type, $E: inner_type>]
		-> [std::result::Result < $T: inner_type, $E: inner_type>]
	{
		match $arg
		{
			std::result::Result::Ok (ret) => Ok ($T (ret)),
			std::result::Result::Err (err) => Err ($E (err))
		}
	}
}

group_autotransforms!
{
	pub autotransform Result [UnqualifiedResult, QualifiedResult]
}

define_autotransform!
{
	pub autotransform UnqualifiedOption
	[Option < $T: inner_type>] -> [Option < $T: inner_type>]
	{
		$arg . map (|x| $T(x))
	}
}

define_autotransform!
{
	pub autotransform QualifiedOption
	[std::option::Option < $T: inner_type>]
		-> [std::option::Option < $T: inner_type>]
	{
		$arg . map (|x| $T(x))
	}
}

group_autotransforms!
{
	pub autotransform Option [UnqualifiedOption, QualifiedOption]
}
