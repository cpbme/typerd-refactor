#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::*;

/// Combines from type to other type from right.
///
/// **Grammar**:
///
/// ` <type> '&' <type> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct IntersectionType {
	pub left: Box<TypeReference>,
	pub operator: AstToken,
	pub right: Box<TypeReference>,
}

impl Node for IntersectionType {
	fn location(&self) -> Location {
		Location::new(self.left.location().start, self.right.location().end)
	}
}

/// It functions as an `or` operator in types.
///
/// **Grammar**:
///
/// ` <type> '|' <type> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct UnionType {
	pub left: Box<TypeReference>,
	pub operator: AstToken,
	pub right: Box<TypeReference>,
}

impl Node for UnionType {
	fn location(&self) -> Location {
		Location::new(self.left.location().start, self.right.location().end)
	}
}

/// Conditional type like IfStmt.
///
/// **Grammar**:
///
/// ` <type> :: <type> `?` <type> ':' <type> `
///
/// Example:
/// ```lua
/// type MyType<A: any[]> = A :: (infer U)[] ? U : never;
/// ```
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ConditionalType {
	pub base_type: Box<TypeReference>,
	pub double_colon: AstToken,
	pub compared_type: Box<TypeReference>,
	pub true_token: AstToken,
	pub true_type: Box<TypeReference>,
	pub else_token: AstToken,
	pub else_type: Box<TypeReference>,
}

impl Node for ConditionalType {
	fn location(&self) -> Location {
		Location::new(
			self.base_type.location().start,
			self.else_type.location().end,
		)
	}
}

/// Type arguments simply as that it requires a single TypeReference object.
///
/// **Grammar**:
///
/// ` <type> { ',' <type> } `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeArguments(pub Vec<(TypeReference, Option<AstToken>)>);

impl Node for TypeArguments {
	fn location(&self) -> Location {
		let start = self.0.first().map(|v| v.0.location().start);
		let end = self.0.last().map(|v| v.0.location().end);
		Location::new(
			start.unwrap_or_else(Position::empty),
			end.unwrap_or_else(Position::empty),
		)
	}
}

/// A simple generic type. It acts as FunctionCall but for generics.
///
/// **Grammar**:
/// ` <name> '<' <args> '>' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericType {
	pub name: Name,
	pub arrows: AstTokenPairs,
	pub args: TypeArguments,
}

impl Node for GenericType {
	fn location(&self) -> Location {
		Location::new(self.name.location().start, self.arrows.1.location().end)
	}
}

/// Actual literal values but it is a type.
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LiteralType {
	Bool(AstToken),
	Number(AstToken),
	Str(AstToken),
	Varargs(AstToken),
}

impl Node for LiteralType {
	fn location(&self) -> Location {
		match self {
			LiteralType::Number(node) => node.location(),
			LiteralType::Str(node) => node.location(),
			LiteralType::Varargs(node) => node.location(),
			LiteralType::Bool(node) => node.location(),
		}
	}
}

/// Generic parameter.
///
/// **Grammar**:
///
/// ` <name> [ ':' <value-type> ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeGenericParameter {
	pub name: Name,
	pub colon: Option<AstToken>,
	pub parameter_type: Option<TypeReference>,
}

impl Node for TypeGenericParameter {
	fn location(&self) -> Location {
		Location::new(
			self.name.location().start,
			self.parameter_type
				.as_ref()
				.map(|v| v.location().end)
				.unwrap_or(self.name.location().end),
		)
	}
}

/// A collection of type generic parameters. Requires one argument or more.
///
/// **Grammar**:
///
/// ` '<' <typegenericparam> { ',' <typegenericparam> } '>' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeGenericParameters {
	pub arrows: AstTokenPairs,
	pub members: Vec<(TypeGenericParameter, Option<AstToken>)>,
}

impl Node for TypeGenericParameters {
	fn location(&self) -> Location {
		self.arrows.location()
	}
}

/// A name for TypeDeclaration statement.
///
/// **Grammar**:
///
/// ` <name> [ <typegenericparams> ] `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TypeDeclarationName {
	pub name: Name,
	pub params: Option<TypeGenericParameters>,
}

impl Node for TypeDeclarationName {
	fn location(&self) -> Location {
		Location::new(
			self.name.location().start,
			self.params
				.as_ref()
				.map(|v| v.arrows.location().end)
				.unwrap_or(self.name.location().end),
		)
	}
}

/// Wraps type in parentheses.
///
/// **Grammar**:
///
/// ` '(' <type> ')' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ParenthesizedType {
	pub parens: AstTokenPairs,
	pub value: Box<TypeReference>,
}

impl Node for ParenthesizedType {
	fn location(&self) -> Location {
		self.parens.location()
	}
}

/// An array type.
///
/// **Grammar**:
///
/// ` <type> '[' ']' `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ArrayType {
	pub brackets: AstTokenPairs,
	pub value: Box<TypeReference>,
}

impl Node for ArrayType {
	fn location(&self) -> Location {
		Location::new(self.value.location().start, self.brackets.location().end)
	}
}

/// Considered as a value type.
///
/// **Grammar**:
///
/// | ` <arraytype> `
/// | ` <type> '::' <type> '?' <type> ':' <type> `
/// | ` <type> '&' <type> `
/// | ` '(' <type> ')' `
/// | ` <literaltype> `
/// | ` <name> `
/// | ` <type> '|' <type> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TypeReference {
	Array(ArrayType),
	Conditional(ConditionalType),
	Generic(GenericType),
	Intersection(IntersectionType),
	Parenthesized(ParenthesizedType),
	Literal(LiteralType),
	Name(Name),
	Union(UnionType),
}

impl Node for TypeReference {
	fn location(&self) -> Location {
		match self {
			TypeReference::Array(node) => node.location(),
			TypeReference::Conditional(node) => node.location(),
			TypeReference::Generic(node) => node.location(),
			TypeReference::Intersection(node) => node.location(),
			TypeReference::Parenthesized(node) => node.location(),
			TypeReference::Literal(node) => node.location(),
			TypeReference::Name(node) => node.location(),
			TypeReference::Union(node) => node.location(),
		}
	}
}
