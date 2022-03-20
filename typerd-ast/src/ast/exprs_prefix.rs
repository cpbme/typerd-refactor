#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use super::*;

/// An expression starts with that before the actual expressions
/// like ParenthesizedExpression and FunctionCall.
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

/// Accesses a table using a method. You must use this for only
/// function calls and function assignments.
///
/// **Grammar**:
///
/// ` <prefixexp> ':' <name> `
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

/// Accesses a table by a name or identifier.
///
/// **Grammar**:
///
/// ` <prefixexp> '.' <name> `
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

/// Accesses a table by an expression.
///
/// **Grammar**:
///
/// ` <prefixexp> '[' <exp> ']' `
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

/// An expression wrapped in parentheses.
///
/// **Grammar**:
///
/// ` '(' <exp> ')' `
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

/// ` <prefixexp> <callargs> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionCall {
	pub prefix: Box<PrefixExpr>,
	pub args: Box<CallArgs>,
}

impl Node for FunctionCall {
	fn location(&self) -> Location {
		Location::new(self.prefix.location().start, self.args.location().end)
	}
}

/// Allowed arguments to call a function including sugary ones.
///
/// **Grammar**:
///
/// | ` '(' <explist> ')' `
/// | ` <string> `
/// | ` <tablector> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum CallArgs {
	ExprList {
		parens: AstTokenPairs,
		list: ExprList,
	},
	Str(Value),
	Table(Value),
}

impl Node for CallArgs {
	fn location(&self) -> Location {
		match self {
			CallArgs::ExprList { parens, .. } => parens.location(),
			CallArgs::Str(node) => node.location(),
			CallArgs::Table(node) => node.location(),
		}
	}
}
