#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use super::*;
use crate::Position;

/// A field of table.
///
/// **Grammar**:
///
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

///	A collection of table fields.
///
/// **Grammar**:
/// ` <tablesep> := ';' | ',' `
///
/// ` <tablefield> { <tablesep> <tablefield> } [ <tablesep> ] `
pub type TableFieldList = Vec<(TableField, Option<AstToken>)>;

/// A collection of objects. It can be a map, array or a set object.
///
/// **Grammar**:
///
/// ` '{' <tablefieldlist> '}' `
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

/// A simple node object and expression.
/// It is a name can be used as a variable or declaration.
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

/// An expression chooses between left and right expressions
/// depending on the operator.
///
/// **Grammar**:
///
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

/// **Grammar**:
///
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

/// A collection of expressions
///
/// **Grammar**:
///
/// ` [ <exp> { `,` <exp> } ] `
/// #[allow(dead_code)]
/// #[allow(dead_code)]
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

/// Allowed kinds of node for a parameter.
///
/// **Grammar**:
///
/// ` <name> | `...' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum ParamKind {
	Name(Name),
	Varargs(AstToken),
}

impl Node for ParamKind {
	fn location(&self) -> Location {
		match self {
			ParamKind::Name(node) => node.location(),
			ParamKind::Varargs(node) => node.location(),
		}
	}
}

/// A simple node object but it is a variable passed from
/// the arguments.
///
/// **Grammar**:
///
/// ` <paramkind> [ ':' <type> ] = [ '=' <exp> ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Parameter {
	pub kind: ParamKind,
	pub colon: Option<AstToken>,
	pub required_type: Option<TypeReference>,
	pub equals: Option<AstToken>,
	pub default_exp: Option<Expr>,
}

impl Node for Parameter {
	fn location(&self) -> Location {
		Location::new(
			self.kind.location().start,
			self.default_exp
				.as_ref()
				.map(|v| v.location().end)
				.unwrap_or(
					self.required_type
						.as_ref()
						.map(|v| v.location().end)
						.unwrap_or(self.kind.location().end),
				),
		)
	}
}

/// A collection of parameters.
///
/// **Grammar**:
///
/// ` [ <param> { ',' <param } ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ParamList(pub Vec<(Parameter, Option<AstToken>)>);

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

/// The base node object for every function object.
///
/// **Grammar**:
///
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
			self.type_parameters
				.as_ref()
				.map(|v| v.location().start)
				.unwrap_or(self.param_parenthesis.0.location().start),
			self.end_token.location().end,
		)
	}
}

/// Acts an expression to a specific type
///
/// **Grammar**:
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
		Location::new(self.expr.location().start, self.cast.location().end)
	}
}

/// **Grammar**:
///
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
