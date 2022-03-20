#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use crate::{ast, define_parser, expect, optional, parse_either, tokens::*, try_parse};

#[derive(Debug)]
pub struct ParseTypeArguments;
define_parser!(ParseTypeArguments, ast::TypeArguments, |_, state: ParseState<'a>| {
	let (state, result) = ParseLikeArgsRequired(ParseTypeReference, "<type>".to_string()).parse(state)?;
	Ok((state, ast::TypeArguments(result)))
});

#[derive(Debug)]
pub struct ParseTypeGenericParameter;
define_parser!(ParseTypeGenericParameter, ast::TypeGenericParameter, |_, state: ParseState<'a>| {
	let (state, name) = ParseName.parse(state)?;
	let (mut state, colon) = optional!(state, ParseSymbol(SymbolKind::Colon));
	let mut parameter_type = None;
	if colon.is_some() {
		let (ns, new_type) = ParseTypeReference.parse(state)?;
		state = ns;
		parameter_type = Some(new_type);
	}
	Ok((state, ast::TypeGenericParameter {
		name,
		colon,
		parameter_type,
	}))
});

#[derive(Debug)]
pub struct ParseTypeGenericParameters;
define_parser!(ParseTypeGenericParameters, ast::TypeGenericParameters, |_, state: ParseState<'a>| {
	let (state, open) = ParseSymbol(SymbolKind::LessThan).parse(state)?;
	let (state, members) = ParseLikeArgsRequired(ParseTypeGenericParameter, "<type>".into()).parse(state)?;
	let (state, close) = expect!(state, ParseSymbol(SymbolKind::GreaterThan), ">", "ParseTypeGenericParameters");
	Ok((state, ast::TypeGenericParameters {
		members,
		arrows: ast::AstTokenPairs(open, close),
	}))
});

#[derive(Debug)]
pub struct ParseLiteralType;
define_parser!(ParseLiteralType, ast::LiteralType, |_, state: ParseState<'a>|
	parse_either!(state, {
		ParseBool => ast::LiteralType::Bool,
		ParseNumber => ast::LiteralType::Number,
		ParseStr => ast::LiteralType::Str,
		ParseVarargs => ast::LiteralType::Varargs,
	})
);

#[derive(Debug)]
pub struct ParseParenthesizedType;
define_parser!(ParseParenthesizedType, ast::ParenthesizedType, |_, state: ParseState<'a>| {
	let (state, open) = ParseSymbol(SymbolKind::OpenParen).parse(state)?;
	let (state, value) = expect!(state, ParseTypeReference, "<type>", "ParseParenthesizedType");
	let (state, close) = ParseSymbol(SymbolKind::CloseParen).parse(state)?;
	Ok((state, ast::ParenthesizedType {
		parens: ast::AstTokenPairs(open, close),
		value: Box::new(value),
	}))
});

#[derive(Debug)]
pub struct ParseGenericType;
define_parser!(ParseGenericType, ast::GenericType, |_, state: ParseState<'a>| {
	let (state, name) = ParseName.parse(state)?;
	let (state, open_arrow) = ParseSymbol(SymbolKind::LessThan).parse(state)?;
	let (state, args) = ParseTypeArguments.parse(state)?;
	let (state, close_arrow) = ParseSymbol(SymbolKind::GreaterThan).parse(state)?;
	Ok((state, ast::GenericType {
		name,
		arrows: ast::AstTokenPairs(open_arrow, close_arrow),
		args
	}))
});

#[derive(Debug)]
pub struct ParseTypeDeclarationName;
define_parser!(ParseTypeDeclarationName, ast::TypeDeclarationName, |_, state: ParseState<'a>| {
	let (state, name) = ParseName.parse(state)?;
	let (state, params) = optional!(state, ParseTypeGenericParameters);
	Ok((state, ast::TypeDeclarationName {
		name,
		params,
	}))
});

#[derive(Debug)]
pub struct ParseTypeReferenceSimple;
define_parser!(ParseTypeReferenceSimple, ast::TypeReference, |_, state: ParseState<'a>|
	parse_either!(state, {
		ParseGenericType => ast::TypeReference::Generic,
		ParseLiteralType => ast::TypeReference::Literal,
		ParseName => ast::TypeReference::Name,
		ParseParenthesizedType => ast::TypeReference::Parenthesized,
	})
);

#[derive(Debug)]
pub struct ParseTypeReference;
define_parser!(ParseTypeReference, ast::TypeReference, |_, state: ParseState<'a>| {
	let (state, base) = ParseTypeReferenceSimple.parse(state)?;
	if let Ok((ns, double_colon)) = try_parse!(state, ParseSymbol(SymbolKind::DoubleColon)) {
		let (ns, compared_type) = expect!(ns, ParseTypeReference, "<type>", "ParseConditionalType(Compared)");
		let (ns, true_token) = ParseSymbol(SymbolKind::QuestionMark).parse(ns)?;
		let (ns, true_type) = expect!(ns, ParseTypeReference, "<type>", "ParseConditionalType(TrueType)");
		let (ns, else_token) = ParseSymbol(SymbolKind::Colon).parse(ns)?;
		let (ns, else_type) = expect!(ns, ParseTypeReference, "<type>", "ParseConditionalType(ElseType)");
		Ok((ns, ast::TypeReference::Conditional(ast::ConditionalType {
			base_type: Box::new(base),
			double_colon,
			compared_type: Box::new(compared_type),
			true_token,
			true_type: Box::new(true_type),
			else_token,
			else_type: Box::new(else_type),
		})))
	} else if let Ok((ns, operator)) = try_parse!(state, ParseSymbol(SymbolKind::VerticalBar)) {
		let (ns, right) = expect!(ns, ParseTypeReference, "<type>", "ParseUnionType");
		Ok((ns, ast::TypeReference::Union(ast::UnionType {
			left: Box::new(base),
			operator,
			right: Box::new(right),
		})))
	} else if let Ok((ns, operator)) = try_parse!(state, ParseSymbol(SymbolKind::Ampersand)) {
		let (ns, right) = expect!(ns, ParseTypeReference, "<type>", "ParseIntersectionType");
		Ok((ns, ast::TypeReference::Intersection(ast::IntersectionType {
			left: Box::new(base),
			operator,
			right: Box::new(right),
		})))
	} else if let Ok((ns, left)) = try_parse!(state, ParseSymbol(SymbolKind::OpenBracket)) {
		let (ns, right) = expect!(ns, ParseSymbol(SymbolKind::CloseBracket), "]", "ParseArrayType");
		Ok((ns, ast::TypeReference::Array(ast::ArrayType {
			brackets: ast::AstTokenPairs(left, right),
			value: Box::new(base),
		})))
	} else {
		Ok((state, base))
	}
});
