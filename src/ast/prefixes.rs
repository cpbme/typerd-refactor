#[cfg(target_feature = "serde")]
use serde::{Deserialize, Serialize};

use super::*;

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum PrefixExpr {
	FunctionCall(FunctionCall),
	Paren(Paren),
	Var(VarExpr),
}

impl Node for PrefixExpr {
	fn location(&self) -> Location {
		match self {
			PrefixExpr::FunctionCall(node) => node.location(),
			PrefixExpr::Paren(node) => node.location(),
			PrefixExpr::Var(node) => node.location(),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AccessMethod {
	pub prefix: Box<PrefixExpr>,
	pub colon: AstToken,
	pub indexer: Name,
}

impl Node for AccessMethod {
	fn location(&self) -> Location {
		Location::new(self.prefix.location().start, self.indexer.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AccessName {
	pub prefix: Box<PrefixExpr>,
	pub dot: AstToken,
	pub name: Name,
}

impl Node for AccessName {
	fn location(&self) -> Location {
		Location::new(self.prefix.location().start, self.name.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AccessExpr {
	pub prefix: Box<PrefixExpr>,
	pub brackets: AstTokenPairs,
	pub expr: Box<Expr>,
}

impl Node for AccessExpr {
	fn location(&self) -> Location {
		Location::new(self.prefix.location().start, self.brackets.1.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Paren {
	pub parens: AstTokenPairs,
	pub expr: Box<Expr>,
}

impl Node for Paren {
	fn location(&self) -> Location {
		let location = self.parens.0.location();
		Location::new(location.start, location.end)
	}
}
