#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use super::*;
use crate::AstToken;

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum VarAssignName {
	AccessExpr(AccessExpr),
	AccessName(AccessName),
	Name(Name),
	Varargs(AstToken),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionAssignName {
	AccessMethod(AccessMethod),
	AccessName(AccessName),
	Name(Name),
}

impl FunctionAssignName {
	pub fn to_prefix(&self) -> PrefixExpr {
		match self {
			FunctionAssignName::AccessMethod(node) => {
				PrefixExpr::Var(VarExpr::AccessMethod(node.clone()))
			},
			FunctionAssignName::AccessName(node) => {
				PrefixExpr::Var(VarExpr::AccessName(node.clone()))
			},
			FunctionAssignName::Name(node) => PrefixExpr::Var(VarExpr::Name(node.clone())),
		}
	}
}

impl Node for FunctionAssignName {
	fn location(&self) -> Location {
		match self {
			FunctionAssignName::AccessMethod(node) => node.location(),
			FunctionAssignName::AccessName(node) => node.location(),
			FunctionAssignName::Name(node) => node.location(),
		}
	}
}

impl Node for VarAssignName {
	fn location(&self) -> Location {
		match self {
			VarAssignName::AccessExpr(node) => node.location(),
			VarAssignName::AccessName(node) => node.location(),
			VarAssignName::Name(node) => node.location(),
			VarAssignName::Varargs(node) => node.location(),
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct VarAssignNames(pub Vec<(VarAssignName, Option<AstToken>)>);

impl Node for VarAssignNames {
	fn location(&self) -> Location {
		let start = &self.0.first().unwrap().0;
		let end = &self.0.last().unwrap().0;
		Location::new(start.location().start, end.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct VarAssign {
	pub names: VarAssignNames,
	pub equals: AstToken,
	pub exprlist: ExprList,
}

impl Node for VarAssign {
	fn location(&self) -> Location {
		let start = self.names.location().start;
		let end = self.exprlist.location().end;
		Location::new(start, end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeDeclaration {
	pub type_token: AstToken,
	pub name: TypeDeclarationName,
	pub equals_token: AstToken,
	pub value: TypeReference,
}

impl Node for TypeDeclaration {
	fn location(&self) -> Location {
		Location::new(self.name.location().start, self.value.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct DoStmt {
	pub do_token: AstToken,
	pub block: Block,
	pub end_token: AstToken,
}

impl Node for DoStmt {
	fn location(&self) -> Location {
		Location::new(
			self.do_token.location().start,
			self.end_token.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ReturnStmt {
	pub return_token: AstToken,
	pub exprlist: ExprList,
}

impl Node for ReturnStmt {
	fn location(&self) -> Location {
		Location::new(
			self.return_token.location().start,
			self.exprlist.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct WhileStmt {
	pub while_token: AstToken,
	pub condition: Expr,
	pub do_token: AstToken,
	pub block: Block,
	pub end_token: AstToken,
}

impl Node for WhileStmt {
	fn location(&self) -> Location {
		Location::new(
			self.while_token.location().start,
			self.end_token.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct RepeatStmt {
	pub repeat_token: AstToken,
	pub block: Block,
	pub until_token: AstToken,
	pub condition: Expr,
}

impl Node for RepeatStmt {
	fn location(&self) -> Location {
		Location::new(
			self.repeat_token.location().start,
			self.condition.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ElseIfBlock {
	pub else_if_token: AstToken,
	pub condition: Expr,
	pub then_token: AstToken,
	pub block: Block,
}

impl Node for ElseIfBlock {
	fn location(&self) -> Location {
		Location::new(
			self.else_if_token.location().start,
			self.block.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ElseBlock {
	pub else_token: AstToken,
	pub block: Block,
}

impl Node for ElseBlock {
	fn location(&self) -> Location {
		Location::new(self.else_token.location().start, self.block.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct IfStmt {
	pub if_token: AstToken,
	pub condition: Expr,
	pub then_token: AstToken,
	pub block: Block,
	pub elseif_blocks: Vec<ElseIfBlock>,
	pub else_block: Option<ElseBlock>,
	pub end_token: AstToken,
}

impl Node for IfStmt {
	fn location(&self) -> Location {
		Location::new(
			self.if_token.location().start,
			self.end_token.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericFor {
	pub for_token: AstToken,
	pub names: LocalAssignNameList,
	pub in_token: AstToken,
	pub exprlist: ExprList,
	pub do_token: AstToken,
	pub block: Block,
	pub end_token: AstToken,
}

impl Node for GenericFor {
	fn location(&self) -> Location {
		Location::new(
			self.for_token.location().start,
			self.end_token.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct NumericFor {
	pub for_token: AstToken,
	pub name: Box<Name>,
	pub equals_token: AstToken,
	pub start: Box<Expr>,
	pub start_comma: AstToken,
	pub end: Box<Expr>,
	pub end_comma: Option<AstToken>,
	pub step: Option<Expr>,
	pub do_token: AstToken,
	pub block: Block,
	pub end_token: AstToken,
}

impl Node for NumericFor {
	fn location(&self) -> Location {
		Location::new(
			self.for_token.location().start,
			self.end_token.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionAssign {
	pub function_token: AstToken,
	pub name: FunctionAssignName,
	pub body: FunctionBody,
}

impl Node for FunctionAssign {
	fn location(&self) -> Location {
		Location::new(
			self.function_token.location().start,
			self.body.location().end,
		)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalFunction {
	pub local_token: AstToken,
	pub function_token: AstToken,
	pub name: Name,
	pub body: FunctionBody,
}

impl Node for LocalFunction {
	fn location(&self) -> Location {
		Location::new(self.local_token.location().start, self.body.location().end)
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment {
	pub local_token: AstToken,
	pub names: LocalAssignNameList,
	pub equals_token: Option<AstToken>,
	pub exprlist: Option<ExprList>,
}

impl Node for LocalAssignment {
	fn location(&self) -> Location {
		Location::new(
			self.local_token.location().start,
			self.exprlist
				.as_ref()
				.map(|v| v.location().end)
				.unwrap_or(self.names.location().end),
		)
	}
}
