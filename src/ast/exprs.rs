#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Position;

use super::*;

/// | ` '[' <exp> ']' '=' <exp> `
/// | ` <name> `
/// | ` <name> '=' <expr> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TableField {
	ExprIndex {
		brackets: AstTokenPairs,
		index: Box<Expr>,
		equals: AstToken,
		value: Box<Expr>,
	},
	Expr(Box<Expr>),
	IdentIndex {
		name: Name,
		equals: AstToken,
		value: Box<Expr>,
	},
}

/// ` <tablesep> := ';' | ',' `
///
/// ` <tablefield> { <tablesep> <tablefield> } [ <tablesep> ] `
pub type TableFieldList = Vec<(TableField, Option<AstToken>)>;

/// ` '{' [ (<tablesep>)* ] '}' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TableCtor {
	pub brackets: AstTokenPairs,
	pub fields: TableFieldList,
}

impl Node for TableCtor {
	fn location(&self) -> crate::Location {
		self.brackets.location()
	}
}

/// ` <name> := Name `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Name {
	pub name: String,
	pub token: AstToken,
}

impl Node for Name {
	fn location(&self) -> Location {
		self.token.location()
	}
}

/// ` <exp> <binop> <exp> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Binary {
	pub lhs: Box<Expr>,
	pub op: Binop,
	pub rhs: Box<Expr>,
}

impl Node for Binary {
	fn location(&self) -> Location {
		Location::new(self.lhs.location().start, self.rhs.location().end)
	}
}

/// ` <unop> <exp> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Unary {
	pub op: Unop,
	pub expr: Box<Expr>,
}

impl Node for Unary {
	fn location(&self) -> Location {
		Location::new(self.op.location().start, self.expr.location().end)
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

/// ` [ <exp> { `,` <exp> } ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ExprList(pub Vec<(Expr, Option<AstToken>)>);

impl ExprList {
	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}
}

impl Node for ExprList {
	fn location(&self) -> Location {
		let start = self.0.first().map(|v| v.0.location().start);
		let end = self.0.last().map(|v| v.0.location().end);
		Location::new(
			start.unwrap_or_else(Position::empty),
			end.unwrap_or_else(Position::empty),
		)
	}
}

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

/// ` <name> | `...' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Param {
	Name(Name),
	Varargs(AstToken),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ParamMember {
	pub kind: Param,
	pub colon: Option<AstToken>,
	pub type_parameter: Option<TypeReference>,
}

impl Node for Param {
	fn location(&self) -> Location {
		match self {
			Param::Name(node) => node.location(),
			Param::Varargs(node) => node.location(),
		}
	}
}

impl Node for ParamMember {
    fn location(&self) -> Location {
		Location::new(
			self.kind.location().start,
			self.type_parameter.as_ref()
				.map(|v| v.location().end)
				.unwrap_or(self.kind.location().end)
		)
    }
}

/// ` [ <param> { ',' <param> } ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ParamList(pub Vec<(ParamMember, Option<AstToken>)>);

impl Node for ParamList {
	fn location(&self) -> Location {
		let start = self.0.first().map(|v| v.0.location().start);
		let end = self.0.last().map(|v| v.0.location().end);
		Location::new(
			start.unwrap_or_else(Position::empty),
			end.unwrap_or_else(Position::empty),
		)
	}
}

/// ` [ '<' <typeparams> '>' ] '(' <paramlist> ')' [ '->' <type> ] <block> 'end' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionBody {
	pub type_parameters: Option<TypeGenericParameters>,
	pub param_parenthesis: AstTokenPairs,
	pub params: ParamList,
	pub arrow_token: Option<AstToken>,
	pub return_type: Option<TypeReference>,
	pub block: Box<Block>,
	pub end_token: AstToken,
}

impl Node for FunctionBody {
	fn location(&self) -> Location {
		Location::new(
			self.type_parameters.as_ref()
				.map(|v| v.location().start)
				.unwrap_or(self.param_parenthesis.0.location().start),
			self.end_token.location().end,
		)
	}
}

/// ` <exp> '::' <type> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeAssertion {
	pub expr: Box<Expr>,
	pub colons: AstToken,
	pub cast: Box<TypeReference>,
}

impl Node for TypeAssertion {
    fn location(&self) -> Location {
        Location::new(
			self.expr.location().start,
			self.cast.location().end
		)
    }
}

/// ` 'function' <functionbody> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionExpr {
	pub function_token: AstToken,
	pub body: Box<FunctionBody>,
}

impl Node for FunctionExpr {
	fn location(&self) -> Location {
		Location::new(
			self.function_token.location().start,
			self.body.location().end,
		)
	}
}
