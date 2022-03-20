#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::*;

macro_rules! operator {
	(
		$(
			#[$meta:meta]
		)*
		$name:ident,
		{
			$(
				$member:ident => $precedence:expr,
			)*
		},
		$associative_body:expr
	) => {
		$(
			#[$meta]
		)*
		pub enum $name {
			$(
				$member,
			)*
		}

		impl $name {
			/// Gets the operator precedence
			pub fn precedence(&self) -> usize {
				match self {
					$(
						$name::$member => $precedence,
					)*
				}
			}

			// A helper method checks if the operator is right associative.
			pub fn is_right_associative(&self) -> bool {
				$associative_body(self)
			}
		}
	};
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
/// Unary operator. It is in the start of the expression.
pub struct Unop {
	pub kind: UnopKind,
	pub token: AstToken,
}

impl Node for Unop {
	fn location(&self) -> crate::Location {
		self.token.location()
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
/// Binary operator. It is in between two expressions from left to right.
pub struct Binop {
	pub kind: BinopKind,
	pub token: AstToken,
}

impl Node for Binop {
	fn location(&self) -> crate::Location {
		self.token.location()
	}
}

operator!(
	#[allow(dead_code)]
	#[derive(Debug, Clone)]
	#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
	#[doc = "Unary operator kind."]
	UnopKind,
	{
		Length => 7,
		Negate => 7,
		Not => 7,
	},
	|_| false
);

operator!(
	#[allow(dead_code)]
	#[derive(Debug, Clone)]
	#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
	#[doc = "Binary operator kind"]
	BinopKind,
	{
		NilshCoalescing => 11,
		Exponent => 10,
		Multiply => 7,
		FloorDivision => 7,
		Divide => 7,
		Modulo => 7,
		Add => 6,
		Subtract => 6,
		Concat => 5,
		Equality => 3,
		Inequality => 3,
		GreaterThan => 3,
		GreaterEqual => 3,
		LessThan => 3,
		LessEqual => 3,
		And => 2,
		Or => 1,
	},
	| k: &BinopKind | matches!(k, Self::Concat | Self::Exponent)
);
