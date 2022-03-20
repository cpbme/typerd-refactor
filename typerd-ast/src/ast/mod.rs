#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::*;

mod exprs_var;
pub use exprs_var::*;

mod exprs_prefix;
pub use exprs_prefix::*;

mod exprs;
pub use exprs::*;

mod operator;
pub use operator::*;

mod stmts;
pub use stmts::*;

mod traits;
pub use traits::*;

mod types;
pub use types::*;

/// A simple expression without any complex suffixes or anything.
///
/// **Grammar**:
/// | ` 'true' `
/// | ` 'false' `
/// | ` 'nil' `
/// | ` <number> `
/// | ` <prefixexp> `
/// | ` <string> `
/// | ` <table> `
/// | ` '...' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value {
	Bool(AstToken),
	Function(FunctionExpr),
	Nil(AstToken),
	Number(AstToken),
	Prefix(PrefixExpr),
	Str(AstToken),
	Table(TableCtor),
	Varargs(AstToken),
}

impl Node for Value {
	fn location(&self) -> crate::Location {
		match self {
			Value::Bool(node) => node.location(),
			Value::Function(node) => node.location(),
			Value::Nil(node) => node.location(),
			Value::Number(node) => node.location(),
			Value::Prefix(node) => node.location(),
			Value::Str(node) => node.location(),
			Value::Table(node) => node.location(),
			Value::Varargs(node) => node.location(),
		}
	}
}

/// A major component of the entire language.
/// It acts like a value.
///
/// **Grammar**:
/// | ` <exp> <binop> <exp> `
/// | ` <unop> <exp> `
/// | ` <value> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Expr {
	Binary(Binary),
	Unary(Unary),
	TypeAssertion(TypeAssertion),
	Value(Box<Value>),
}

impl Node for Expr {
	fn location(&self) -> crate::Location {
		match self {
			Expr::Binary(node) => node.location(),
			Expr::Unary(node) => node.location(),
			Expr::Value(node) => node.location(),
			Expr::TypeAssertion(node) => node.location(),
		}
	}
}

/// An instructional component of what program will do.
///
/// **Grammar**:
///
/// | ` <functioncall> `
/// | ` 'do' <block> 'end' `
/// | ` 'for' <namelist> 'in' <explist> 'do' <block> 'end' `
/// | ` 'if' <exp> 'then' <block> { 'elseif' <exp> 'then' } [ 'else' <block> ] 'end' `
/// | ` 'local' <varnamelist> '=' <explist> `
/// | ` 'local' 'function' <name> <functionbody> `
/// | ` 'for' <name> '=' <exp> ',' <exp> [ ',' <exp> ] 'do' <block> 'end' `
/// | ` 'repeat' <block> 'until' <exp> `
/// | ` 'while' <exp> 'do' <block> 'end' `
/// | ` <varlist> '=' <explist> `
/// | ` 'enum' <name> '{' <enummemberlist> '}' ` (in progress)
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt {
	Call(FunctionCall),
	Do(DoStmt),
	GenericFor(GenericFor),
	FunctionAssign(FunctionAssign),
	If(IfStmt),
	LocalAssignment(LocalAssignment),
	LocalFunction(LocalFunction),
	NumericFor(NumericFor),
	Repeat(RepeatStmt),
	TypeDeclaration(TypeDeclaration),
	While(WhileStmt),
	VarAssign(VarAssign),
}

impl Node for Stmt {
	fn location(&self) -> crate::Location {
		match self {
			Stmt::Do(node) => node.location(),
			Stmt::GenericFor(node) => node.location(),
			Stmt::FunctionAssign(node) => node.location(),
			Stmt::If(node) => node.location(),
			Stmt::LocalAssignment(node) => node.location(),
			Stmt::LocalFunction(node) => node.location(),
			Stmt::NumericFor(node) => node.location(),
			Stmt::Repeat(node) => node.location(),
			Stmt::While(node) => node.location(),
			Stmt::Call(node) => node.location(),
			Stmt::VarAssign(node) => node.location(),
			Stmt::TypeDeclaration(node) => node.location(),
		}
	}
}

/// Last statement of Typerd language.
/// It is used to stop parsing the entire block unlike JavaScript.
///
/// | ` 'return' [ <explist> ] `
/// | ` 'break' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt {
	Break((AstToken, Option<AstToken>)),
	Return((ReturnStmt, Option<AstToken>)),
}

impl Node for LastStmt {
	fn location(&self) -> Location {
		match self {
			LastStmt::Break((node, semicolon)) => {
				let end = semicolon
					.as_ref()
					.map(|v| v.location().end)
					.unwrap_or(node.location().end);
				Location::new(node.location().start, end)
			},
			LastStmt::Return((node, semicolon)) => {
				let end = semicolon
					.as_ref()
					.map(|v| v.location().end)
					.unwrap_or(node.location().end);
				Location::new(node.location().start, end)
			},
		}
	}
}

/// A collection of statements with last statement included.
///
/// **Grammar**:
///
/// ` { <stat> [ ';' ] } [ <laststat> [ ';' ] ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Stmts(pub Vec<(Stmt, Option<AstToken>)>, pub Option<LastStmt>);

impl Node for Stmts {
	fn location(&self) -> Location {
		let start = self
			.0
			.first()
			.map(|v| v.0.location().start)
			.unwrap_or_default();
		let end = self.1.as_ref().map(|a| a.location().end).unwrap_or(
			self.0
				.last()
				.map(|b| {
					b.1.as_ref()
						.map(|c| c.location().end)
						.unwrap_or(b.0.location().end)
				})
				.unwrap_or_default(),
		);

		Location::new(start, end)
	}
}

/// A pair of tokens.
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AstTokenPairs(pub AstToken, pub AstToken);

impl Node for AstTokenPairs {
	fn location(&self) -> crate::Location {
		Location::new(self.0.location().start, self.1.location().end)
	}
}

/// A chunk or collection of statements
///
/// **Grammar**:
///
/// ` { <stat> [';'] } [ <laststat> [';'] ]`
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block {
	pub stmts: Stmts,
}

impl Node for Block {
	fn location(&self) -> crate::Location {
		let start = self.stmts.0.first().map(|v| v.0.location().start);
		let end = self.stmts.0.last().map(|v| v.0.location().start);
		Location::new(start.unwrap_or_default(), end.unwrap_or_default())
	}
}
