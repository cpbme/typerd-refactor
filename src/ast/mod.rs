#[cfg(target_feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{AstToken, Location};
mod vars;
pub use vars::*;

mod traits;
pub use traits::*;

mod prefixes;
pub use prefixes::*;

mod exprs;
pub use exprs::*;

mod types;
pub use types::*;

mod stmts;
pub use stmts::*;

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
/// | ` 'enum' <name> '{' <enummemberlist> '}' `
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
	While(WhileStmt),
	VarAssign(VarAssign),
}

/// | ` 'return' [ <explist> ] `
/// | ` 'break' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt {
	Break((AstToken, Option<AstToken>)),
	Return(ReturnStmt),
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
		}
	}
}

/// ` { <stat> [ ';' ] } [ <laststat> [ ';' ] ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Stmts(pub Vec<(Stmt, Option<AstToken>)>, pub Option<LastStmt>);

/// ` <stmts> `
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

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AstTokenPairs(pub AstToken, pub AstToken);

impl Node for AstTokenPairs {
	fn location(&self) -> crate::Location {
		Location::new(self.0.location().start, self.1.location().end)
	}
}

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
			pub fn precedence(&self) -> usize {
				match self {
					$(
						$name::$member => $precedence,
					)*
				}
			}

			pub fn is_right_associative(&self) -> bool {
				$associative_body(self)
			}
		}
	};
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
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
