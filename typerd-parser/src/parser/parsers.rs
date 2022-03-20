use super::*;
use crate::prelude::*;
use crate::{define_parser, expect, optional, parse_either, try_parse};
use ast::{self, AstToken, KeywordType, SymbolType, TokenKind, TokenType};

#[derive(Debug)]
pub struct ParseToken(pub TokenType);
define_parser!(
	ParseToken,
	AstToken,
	|this: &ParseToken, state: ParseState<'a>| {
		if let Some(tok) = state.peek() {
			if tok.token_type() == &this.0 {
				return Ok((state.advance(1), tok.clone()));
			}
		}
		Err(ParseError::NoMatch)
	}
);

#[derive(Debug)]
pub struct ParseSymbol(pub SymbolType);
define_parser!(
	ParseSymbol,
	AstToken,
	|this: &ParseSymbol, state: ParseState<'a>| {
		let (state, token) = ParseToken(TokenType::Symbol(this.0)).parse(state)?;
		Ok((state, token))
	}
);

#[derive(Debug)]
pub struct ParseBool;
define_parser!(ParseBool, AstToken, |_, state: ParseState<'a>| {
	if let Some(token) = state.peek() {
		if matches!(
			token.token_type(),
			TokenType::Keyword(KeywordType::True | KeywordType::False)
		) {
			return Ok((state.advance(1), token.clone()));
		}
	}
	Err(ParseError::NoMatch)
});

#[derive(Debug)]
pub struct ParseNumber;
define_parser!(ParseNumber, AstToken, |_, state: ParseState<'a>| {
	if let Some(token) = state.peek() {
		if matches!(token.kind(), TokenKind::Number) {
			return Ok((state.advance(1), token.clone()));
		}
	}
	Err(ParseError::NoMatch)
});

#[derive(Debug)]
pub struct ParseStr;
define_parser!(ParseStr, AstToken, |_, state: ParseState<'a>| {
	if let Some(token) = state.peek() {
		if matches!(token.kind(), TokenKind::String) {
			return Ok((state.advance(1), token.clone()));
		}
	}
	Err(ParseError::NoMatch)
});

#[derive(Debug)]
pub struct ParseNil;
define_parser!(ParseNil, AstToken, |_, state: ParseState<'a>| {
	let (state, token) = ParseToken(TokenType::Keyword(KeywordType::Nil)).parse(state)?;
	Ok((state, token))
});

#[derive(Debug)]
pub struct ParseVarargs;
define_parser!(ParseVarargs, AstToken, |_, state: ParseState<'a>| {
	let (state, token) = ParseToken(TokenType::Symbol(SymbolType::TripleDots)).parse(state)?;
	Ok((state, token))
});

#[derive(Debug)]
pub struct ParseParamKind;
define_parser!(
	ParseParamKind,
	ast::ParamKind,
	|_, state: ParseState<'a>| parse_either!(state, {
		ParseName => ast::ParamKind::Name,
		ParseVarargs => ast::ParamKind::Varargs,
	})
);

pub struct ParseParameter;
define_parser!(
	ParseParameter,
	ast::Parameter,
	|_, state: ParseState<'a>| {
		let (state, kind) = ParseParamKind.parse(state)?;
		let (state, colon) = optional!(state, ParseSymbol(SymbolType::Colon));
		let (state, required_type) = if colon.is_some() {
			let (ns, ty) = expect!(state, ParseTypeReference, "<type>", "ParseParameter");
			(ns, Some(ty))
		} else {
			(state, None)
		};
		let (state, equals) = optional!(state, ParseSymbol(SymbolType::Equal));
		let (state, default_exp) = if equals.is_some() {
			let (ns, exp) = expect!(state, ParseExpr, "<exp>", "ParseParameter");
			(ns, Some(exp))
		} else {
			(state, None)
		};
		Ok((
			state,
			ast::Parameter {
				kind,
				colon,
				required_type,
				equals,
				default_exp,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseParamList;
define_parser!(
	ParseParamList,
	ast::ParamList,
	|_, state: ParseState<'a>| {
		let (state, list) = ParseLikeArgs(ParseParameter, "<id>".into()).parse(state)?;
		Ok((state, ast::ParamList(list)))
	}
);

#[derive(Debug)]
pub struct ParseFunctionBody;
define_parser!(ParseFunctionBody, ast::FunctionBody, |_,
                                                      state: ParseState<
	'a,
>| {
	let (state, type_parameters) = optional!(state, ParseTypeGenericParameters);
	let (state, open) = expect!(
		state,
		ParseSymbol(SymbolType::OpenParen),
		"(",
		"ParseFunctionBody"
	);
	let (state, params) = ParseParamList.parse(state)?;
	let (state, close) = expect!(
		state,
		ParseSymbol(SymbolType::CloseParen),
		")",
		"ParseFunctionBody"
	);
	let (state, arrow_token) = optional!(state, ParseSymbol(SymbolType::SkinnyArrow));
	let (state, return_type) = if arrow_token.is_some() {
		let (ns, ty) = expect!(state, ParseTypeReference, "<type>", "ParseFunctionBody");
		(ns, Some(ty))
	} else {
		(state, None)
	};
	let (state, block) = ParseBlock.parse(state)?;
	let (state, end_token) = expect!(
		state,
		ParseToken(TokenType::Keyword(KeywordType::End)),
		"end",
		"ParseFunctionBody"
	);
	Ok((
		state,
		ast::FunctionBody {
			type_parameters,
			param_parenthesis: ast::AstTokenPairs(open, close),
			params,
			arrow_token,
			return_type,
			block: Box::new(block),
			end_token,
		},
	))
});

#[derive(Debug)]
pub struct ParseFunctionExpr;
define_parser!(ParseFunctionExpr, ast::FunctionExpr, |_,
                                                      state: ParseState<
	'a,
>| {
	let (state, function_token) =
		ParseToken(TokenType::Keyword(KeywordType::Function)).parse(state)?;
	let (state, body) = ParseFunctionBody.parse(state)?;
	Ok((
		state,
		ast::FunctionExpr {
			function_token,
			body: Box::new(body),
		},
	))
});

#[derive(Debug)]
pub struct ParseValue;
define_parser!(
	ParseValue,
	ast::Value,
	|_, state: ParseState<'a>| parse_either!(state, {
		ParseBool => ast::Value::Bool,
		ParseFunctionExpr => ast::Value::Function,
		ParseNil => ast::Value::Nil,
		ParseNumber => ast::Value::Number,
		ParsePrefixExpr => ast::Value::Prefix,
		ParseStr => ast::Value::Str,
		ParseTableCtor => ast::Value::Table,
		ParseVarargs => ast::Value::Varargs,
	})
);

#[derive(Debug)]
pub struct ParsePrefixExpr;
define_parser!(
	ParsePrefixExpr,
	ast::PrefixExpr,
	|_, state: ParseState<'a>| {
		let (state, current) = ParsePrefixBaseExpr.parse(state)?;
		let (state, current) = ParseComplexSuffixes(current).parse(state)?;

		// no method property allowed pls
		if matches!(
			current,
			ast::PrefixExpr::Var(ast::VarExpr::AccessMethod(..))
		) {
			Err(ParseError::NoMatch)
		} else {
			Ok((state, current))
		}
	}
);

#[derive(Debug)]
pub struct ParsePrefixBaseExpr;
#[rustfmt::skip]
define_parser!(ParsePrefixBaseExpr, ast::PrefixExpr, |_, state: ParseState<'a,>|
	parse_either!(state, {
		ParseParen => ast::PrefixExpr::Paren,
		ParseName => |name| ast::PrefixExpr::Var(ast::VarExpr::Name(name)),
	})
);

#[derive(Debug)]
pub struct ParseParen;
define_parser!(ParseParen, ast::Paren, |_, state: ParseState<'a>| {
	let (state, open) = ParseSymbol(SymbolType::OpenParen).parse(state)?;
	let (state, expr) = expect!(state, ParseExpr, "<exp>", "ParseParen");
	let (state, close) = expect!(
		state,
		ParseSymbol(SymbolType::CloseParen),
		")",
		"ParseParen"
	);
	Ok((
		state,
		ast::Paren {
			parens: ast::AstTokenPairs(open, close),
			expr: Box::new(expr),
		},
	))
});

#[derive(Debug)]
pub struct ParseComplexSuffixes(pub ast::PrefixExpr);
define_parser!(
	ParseComplexSuffixes,
	ast::PrefixExpr,
	|this: &ParseComplexSuffixes, state: ParseState<'a>| {
		let mut state = state.advance(0);
		let mut base = this.0.clone();

		while let Some(token) = state.peek() {
			if let Ok((_ns, dot)) = try_parse!(state, ParseSymbol(SymbolType::Dot)) {
				let (_ns, name) = expect!(_ns, ParseName, "<id>", "ParseComplexSuffixes");
				state = _ns;
				base = ast::PrefixExpr::Var(ast::VarExpr::AccessName(ast::AccessName {
					prefix: Box::new(base),
					dot,
					name,
				}));
			} else if let Ok((_ns, open)) = try_parse!(state, ParseSymbol(SymbolType::OpenBracket))
			{
				let (_ns, expr) = expect!(_ns, ParseExpr, "<exp>", "ParseComplexSuffixes");
				let (_ns, close) = expect!(
					_ns,
					ParseSymbol(SymbolType::CloseBracket),
					"]",
					"ParseComplexSuffixes"
				);
				state = _ns;
				base = ast::PrefixExpr::Var(ast::VarExpr::AccessExpr(ast::AccessExpr {
					prefix: Box::new(base),
					brackets: ast::AstTokenPairs(open, close),
					expr: Box::new(expr),
				}));
			} else if let Ok((_ns, colon)) = try_parse!(state, ParseSymbol(SymbolType::Colon)) {
				let (_ns, indexer) = expect!(_ns, ParseName, "<id>", "ParseComplexSuffixes");
				state = _ns;
				base = ast::PrefixExpr::Var(ast::VarExpr::AccessMethod(ast::AccessMethod {
					prefix: Box::new(base),
					colon,
					indexer,
				}));
			} else if matches!(
				token.token_type(),
				TokenType::LineStr { .. }
					| TokenType::MultilinedStr { .. }
					| TokenType::Symbol(SymbolType::OpenCurly)
					| TokenType::Symbol(SymbolType::OpenParen)
			) {
				let (_ns, args) = ParseCallArgs.parse(state)?;
				state = _ns;
				base = ast::PrefixExpr::FunctionCall(ast::FunctionCall {
					prefix: Box::new(base),
					args: Box::new(args),
				});
			} else {
				break;
			}
		}

		Ok((state, base))
	}
);

#[derive(Debug)]
pub struct ParseExprList;
define_parser!(ParseExprList, ast::ExprList, |_, state: ParseState<'a>| {
	let (state, result) = ParseLikeArgs(ParseExpr, "<exp>".into()).parse(state)?;
	Ok((state, ast::ExprList(result)))
});

#[derive(Debug)]
pub struct ParseExprListRequired;
define_parser!(ParseExprListRequired, ast::ExprList, |_,
                                                      state: ParseState<
	'a,
>| {
	let (state, result) = ParseLikeArgsRequired(ParseExpr, "<exp>".into()).parse(state)?;
	Ok((state, ast::ExprList(result)))
});

#[derive(Debug)]
pub struct ParseCallArgs;
define_parser!(ParseCallArgs, ast::CallArgs, |_, state: ParseState<'a>| {
	if let Ok((_ns, open)) = ParseSymbol(SymbolType::OpenParen).parse(state.advance(0)) {
		let (_ns, list) = ParseExprList.parse(_ns)?;
		let (_ns, close) = expect!(
			_ns,
			ParseSymbol(SymbolType::CloseParen),
			")",
			"ParseCallArgs"
		);
		Ok((
			_ns,
			ast::CallArgs::ExprList {
				parens: ast::AstTokenPairs(open, close),
				list,
			},
		))
	} else if ParseSymbol(SymbolType::OpenCurly)
		.parse(state.advance(0))
		.is_ok()
	{
		let (ns, tbl) = ParseTableCtor.parse(state)?;
		Ok((ns, ast::CallArgs::Table(ast::Value::Table(tbl))))
	} else if let Ok((ns, token)) = ParseStr.parse(state.advance(0)) {
		Ok((ns, ast::CallArgs::Str(ast::Value::Str(token))))
	} else {
		Err(ParseError::NoMatch)
	}
});

#[derive(Debug)]
pub struct ParseTableField;
define_parser!(
	ParseTableField,
	ast::TableField,
	|_, state: ParseState<'a>| {
		if let Ok((_ns, open)) = ParseSymbol(SymbolType::OpenBracket).parse(state.advance(0)) {
			let (_ns, index) = expect!(_ns, ParseExpr, "<exp>", "ParseTableField");
			let (_ns, close) = expect!(
				_ns,
				ParseSymbol(SymbolType::CloseBracket),
				"=",
				"ParseTableField"
			);
			let (_ns, equals) =
				expect!(_ns, ParseSymbol(SymbolType::Equal), "=", "ParseTableField");
			let (_ns, value) = expect!(_ns, ParseExpr, "<exp>", "ParseTableField");
			Ok((
				_ns,
				ast::TableField::ExprIndex {
					brackets: ast::AstTokenPairs(open, close),
					index: Box::new(index),
					equals,
					value: Box::new(value),
				},
			))
		} else if let Ok((_ns, name)) = ParseName.parse(state.advance(0)) {
			if let Ok((_ns1, equals)) = ParseSymbol(SymbolType::Equal).parse(_ns.advance(0)) {
				let (_ns1, value) = expect!(_ns1, ParseExpr, "<exp>", "ParseTableField");
				Ok((
					_ns1,
					ast::TableField::IdentIndex {
						name,
						equals,
						value: Box::new(value),
					},
				))
			} else {
				Ok((
					_ns,
					ast::TableField::Expr(Box::new(ast::Expr::Value(Box::new(
						ast::Value::Prefix(ast::PrefixExpr::Var(ast::VarExpr::Name(name))),
					)))),
				))
			}
		} else {
			let (_ns, expr) = ParseExpr.parse(state)?;
			Ok((_ns, ast::TableField::Expr(Box::new(expr))))
		}
	}
);

#[derive(Debug)]
pub struct ParseTableFieldSeparator;
define_parser!(ParseTableFieldSeparator, AstToken, |_,
                                                    state: ParseState<
	'a,
>| parse_either!(state, {
	ParseSymbol(SymbolType::Semicolon) => |e| e,
	ParseSymbol(SymbolType::Comma) => |e| e,
}));

#[derive(Debug)]
pub struct ParseTableFieldListMember;
define_parser!(
	ParseTableFieldListMember,
	(ast::TableField, Option<AstToken>),
	|_, state: ParseState<'a>| {
		let (state, field) = ParseTableField.parse(state)?;
		let (state, sep) = optional!(state, ParseTableFieldSeparator);
		Ok((state, (field, sep)))
	}
);

#[derive(Debug)]
pub struct ParseTableCtor;
define_parser!(
	ParseTableCtor,
	ast::TableCtor,
	|_, state: ParseState<'a>| {
		let (state, open) = ParseSymbol(SymbolType::OpenCurly).parse(state)?;
		let (state, fields) = ParseOptionalOrMore(ParseTableFieldListMember).parse(state)?;
		let (state, close) = expect!(
			state,
			ParseSymbol(SymbolType::CloseCurly),
			"}",
			"ParseTableCtor"
		);
		Ok((
			state,
			ast::TableCtor {
				brackets: ast::AstTokenPairs(open, close),
				fields,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseFunctionCall;
#[rustfmt::skip]
define_parser!(ParseFunctionCall, ast::FunctionCall, |_, state: ParseState<'a,>| {
	let (state, current) = ParsePrefixBaseExpr.parse(state)?;
	let (state, current) = ParseComplexSuffixes(current).parse(state)?;

	// function call is allowed
	use ast::PrefixExpr::*;
	match current {
		FunctionCall(call) => Ok((state, call)),
		_ => Err(ParseError::NoMatch),
	}
});

#[derive(Debug)]
pub struct ParseName;
define_parser!(ParseName, ast::Name, |_, state: ParseState<'a>| {
	// lua compatibility
	if let Some(token) = state.peek() {
		if let TokenType::Name(name) = token.token_type() {
			return Ok((
				state.advance(1),
				ast::Name {
					name: name.to_string(),
					token: token.clone(),
				},
			));
		} else if let TokenType::Keyword(k) = token.token_type() {
			if matches!(k, KeywordType::Type) {
				return Ok((
					state.advance(1),
					ast::Name {
						name: k.str().to_string(),
						token: token.clone(),
					},
				));
			}
		}
	}
	Err(ParseError::NoMatch)
});

#[derive(Debug)]
pub struct ParseSimpleExpr;
define_parser!(
	ParseSimpleExpr,
	ast::Expr,
	|_, state: ParseState<'a>| parse_either!(state, {
		ParseUnary => ast::Expr::Unary,
		ParseValue => |e| ast::Expr::Value(Box::new(e)),
	})
);

#[derive(Debug)]
pub struct ParseBinopKind;
define_parser!(
	ParseBinopKind,
	ast::BinopKind,
	|_, state: ParseState<'a>| {
		if let Some(token) = state.peek() {
			return match token.token_type() {
				TokenType::Symbol(s) => match &s {
					SymbolType::DoubleEqual => Ok((state.advance(0), ast::BinopKind::Equality)),
					SymbolType::NotEqual => Ok((state.advance(0), ast::BinopKind::Inequality)),
					SymbolType::GreaterThan => Ok((state.advance(0), ast::BinopKind::GreaterThan)),
					SymbolType::GreaterEqual => {
						Ok((state.advance(0), ast::BinopKind::GreaterEqual))
					},
					SymbolType::LessThan => Ok((state.advance(0), ast::BinopKind::LessThan)),
					SymbolType::LessEqual => Ok((state.advance(0), ast::BinopKind::LessEqual)),
					SymbolType::Cross => Ok((state.advance(0), ast::BinopKind::Add)),
					SymbolType::Dash => Ok((state.advance(0), ast::BinopKind::Subtract)),
					SymbolType::Asterisk => Ok((state.advance(0), ast::BinopKind::Multiply)),
					SymbolType::Slash => Ok((state.advance(0), ast::BinopKind::Divide)),
					SymbolType::DoubleDots => Ok((state.advance(0), ast::BinopKind::Concat)),
					SymbolType::Caret => Ok((state.advance(0), ast::BinopKind::Exponent)),
					SymbolType::DoubleSlash => {
						Ok((state.advance(0), ast::BinopKind::FloorDivision))
					},
					SymbolType::Percent => Ok((state.advance(0), ast::BinopKind::Modulo)),
					SymbolType::DoubleQuestionMarks => {
						Ok((state.advance(0), ast::BinopKind::NilshCoalescing))
					},
					_ => Err(ParseError::NoMatch),
				},
				TokenType::Keyword(k) => match &k {
					KeywordType::And => Ok((state.advance(0), ast::BinopKind::And)),
					KeywordType::Or => Ok((state.advance(0), ast::BinopKind::Or)),
					_ => Err(ParseError::NoMatch),
				},
				_ => Err(ParseError::NoMatch),
			};
		}
		Err(ParseError::NoMatch)
	}
);

#[derive(Debug)]
pub struct ParseBinop;
define_parser!(ParseBinop, ast::Binop, |_, state: ParseState<'a>| {
	let (state, kind) = ParseBinopKind.parse(state)?;
	let token = state.peek().cloned().unwrap();
	Ok((state.advance(1), ast::Binop { kind, token }))
});

#[derive(Debug)]
pub struct ParseUnopKind;
define_parser!(ParseUnopKind, ast::UnopKind, |_, state: ParseState<'a>| {
	if let Some(token) = state.peek() {
		return match token.token_type() {
			TokenType::Symbol(sy) => match sy {
				SymbolType::Dash => Ok((state.advance(0), ast::UnopKind::Negate)),
				SymbolType::Hash => Ok((state.advance(0), ast::UnopKind::Length)),
				_ => Err(ParseError::NoMatch),
			},
			TokenType::Keyword(KeywordType::Not) => Ok((state.advance(0), ast::UnopKind::Not)),
			_ => Err(ParseError::NoMatch),
		};
	}
	Err(ParseError::NoMatch)
});

#[derive(Debug)]
pub struct ParseUnop;
define_parser!(ParseUnop, ast::Unop, |_, state: ParseState<'a>| {
	let (state, kind) = ParseUnopKind.parse(state)?;
	let token = state.peek().cloned().unwrap();
	Ok((state.advance(1), ast::Unop { kind, token }))
});

#[derive(Debug)]
pub struct ParseUnary;
define_parser!(ParseUnary, ast::Unary, |_, state: ParseState<'a>| {
	let (state, op) = ParseUnop.parse(state)?;
	let (state, expr) = expect!(state, ParseExpr, "<exp>", "ParseUnary");
	Ok((
		state,
		ast::Unary {
			op,
			expr: Box::new(expr),
		},
	))
});

#[derive(Debug)]
pub struct ParseExprWithPrecedence(pub usize);
define_parser!(
	ParseExprWithPrecedence,
	ast::Expr,
	|this: &ParseExprWithPrecedence, state: ParseState<'a>| {
		let (mut state, mut expr) = ParseSimpleExpr.parse(state)?;
		let current_precedence = this.0;

		while let Ok((_ns, op)) = ParseBinop.parse(state.advance(0)) {
			let kind = &op.kind;
			if kind.precedence() <= current_precedence {
				break;
			}

			let passed_precedence = if kind.is_right_associative() {
				current_precedence - 1
			} else {
				current_precedence
			};

			let (_ns, rhs) = expect!(
				_ns,
				ParseExprWithPrecedence(passed_precedence),
				"<exp>",
				"ParseExprWithPrecedence"
			);
			state = _ns;
			expr = ast::Expr::Binary(ast::Binary {
				lhs: Box::new(expr),
				op,
				rhs: Box::new(rhs),
			});
		}

		Ok((state, expr))
	}
);

#[derive(Debug)]
pub struct ParseExpr;
define_parser!(ParseExpr, ast::Expr, |_, state: ParseState<'a>| {
	let (mut state, mut expr) = ParseExprWithPrecedence(0).parse(state)?;
	while let Ok((ns, colons)) = try_parse!(state, ParseToken(TokenType::Keyword(KeywordType::As)))
	{
		let (ns, cast) = expect!(ns, ParseTypeReference, "<type>", "ParseTypeAssertion(Cast)");
		state = ns;
		expr = ast::Expr::TypeAssertion(ast::TypeAssertion {
			expr: Box::new(expr),
			colons,
			cast: Box::new(cast),
		});
	}
	Ok((state, expr))
});

#[derive(Debug)]
pub struct ParseDoStmt;
define_parser!(ParseDoStmt, ast::DoStmt, |_, state: ParseState<'a>| {
	let (state, do_token) = ParseToken(TokenType::Keyword(KeywordType::Do)).parse(state)?;
	let (state, block) = ParseBlock.parse(state)?;
	let (state, end_token) = ParseToken(TokenType::Keyword(KeywordType::End)).parse(state)?;
	Ok((
		state,
		ast::DoStmt {
			do_token,
			block,
			end_token,
		},
	))
});

#[derive(Debug)]
pub struct ParseWhileStmt;
define_parser!(
	ParseWhileStmt,
	ast::WhileStmt,
	|_, state: ParseState<'a>| {
		let (state, while_token) =
			ParseToken(TokenType::Keyword(KeywordType::While)).parse(state)?;
		let (state, condition) = expect!(state, ParseExpr, "<exp>", "ParseWhileStmt");
		let (state, do_token) = ParseToken(TokenType::Keyword(KeywordType::Do)).parse(state)?;
		let (state, block) = ParseBlock.parse(state)?;
		let (state, end_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::End)),
			"end",
			"ParseWhileStmt"
		);
		Ok((
			state,
			ast::WhileStmt {
				while_token,
				condition,
				do_token,
				block,
				end_token,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseRepeatStmt;
define_parser!(
	ParseRepeatStmt,
	ast::RepeatStmt,
	|_, state: ParseState<'a>| {
		let (state, repeat_token) =
			ParseToken(TokenType::Keyword(KeywordType::Repeat)).parse(state)?;
		let (state, block) = ParseBlock.parse(state)?;
		let (state, until_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::Until)),
			"until",
			"ParseRepeatStmt"
		);
		let (state, condition) = expect!(state, ParseExpr, "<exp>", "ParseRepeatStmt");
		Ok((
			state,
			ast::RepeatStmt {
				repeat_token,
				block,
				until_token,
				condition,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseReturnStmt;
define_parser!(
	ParseReturnStmt,
	(ast::ReturnStmt, Option<AstToken>),
	|_, state: ParseState<'a>| {
		let (state, return_token) =
			ParseToken(TokenType::Keyword(KeywordType::Return)).parse(state)?;
		let (state, exprlist) = ParseExprList.parse(state)?;
		let (state, semicolon) = optional!(state, ParseSymbol(SymbolType::Semicolon));
		Ok((
			state,
			(
				ast::ReturnStmt {
					return_token,
					exprlist,
				},
				semicolon,
			),
		))
	}
);

#[derive(Debug)]
pub struct ParseBreakStmt;
define_parser!(
	ParseBreakStmt,
	(AstToken, Option<AstToken>),
	|_, state: ParseState<'a>| {
		let (state, break_token) =
			ParseToken(TokenType::Keyword(KeywordType::Break)).parse(state)?;
		let (state, semicolon) = optional!(state, ParseSymbol(SymbolType::Semicolon));
		Ok((state, (break_token, semicolon)))
	}
);

#[derive(Debug)]
pub struct ParseVarAssignName;
define_parser!(
	ParseVarAssignName,
	ast::VarAssignName,
	|_, state: ParseState<'a>| {
		use ast::{PrefixExpr, VarExpr};

		if let Ok((state, varargs)) = ParseVarargs.parse(state.advance(0)) {
			return Ok((state, ast::VarAssignName::Varargs(varargs)));
		}

		let (state, prefix) = ParsePrefixExpr.parse(state)?;
		match prefix {
			PrefixExpr::Var(var) => match var {
				VarExpr::AccessExpr(node) => Ok((state, ast::VarAssignName::AccessExpr(node))),
				VarExpr::AccessName(node) => Ok((state, ast::VarAssignName::AccessName(node))),
				VarExpr::Name(name) => Ok((state, ast::VarAssignName::Name(name))),
				_ => Err(ParseError::NoMatch),
			},
			_ => Err(ParseError::NoMatch),
		}
	}
);

#[derive(Debug)]
pub struct ParseFunctionAssignName;
define_parser!(
	ParseFunctionAssignName,
	ast::FunctionAssignName,
	|_, state: ParseState<'a>| {
		let (mut state, name) = ParseName.parse(state)?;
		let mut name = ast::FunctionAssignName::Name(name);

		loop {
			if let Ok((_ns, dot)) = ParseSymbol(SymbolType::Dot).parse(state.advance(0)) {
				let (_ns, indexer) = ParseName.parse(_ns)?;
				state = _ns;
				name = ast::FunctionAssignName::AccessName(ast::AccessName {
					prefix: Box::new(name.to_prefix()),
					dot,
					name: indexer,
				});
			} else if let Ok((_ns, colon)) = ParseSymbol(SymbolType::Colon).parse(state.advance(0))
			{
				let (_ns, indexer) = ParseName.parse(_ns)?;
				state = _ns;
				name = ast::FunctionAssignName::AccessMethod(ast::AccessMethod {
					prefix: Box::new(name.to_prefix()),
					colon,
					indexer,
				});
			} else {
				break;
			}
		}

		Ok((state, name))
	}
);

#[derive(Debug)]
pub struct ParseVarAssignNames;
define_parser!(
	ParseVarAssignNames,
	ast::VarAssignNames,
	|_, state: ParseState<'a>| {
		let (state, result) = ParseLikeArgs(ParseVarAssignName, "<id>".to_string()).parse(state)?;
		if result.is_empty() {
			Err(ParseError::NoMatch)
		} else {
			Ok((state, ast::VarAssignNames(result)))
		}
	}
);

#[derive(Debug)]
pub struct ParseVarAssign;
define_parser!(
	ParseVarAssign,
	ast::VarAssign,
	|_, state: ParseState<'a>| {
		let (state, names) = ParseVarAssignNames.parse(state)?;
		let (state, equals) = ParseSymbol(SymbolType::Equal).parse(state)?;
		let (state, exprlist) = ParseExprListRequired.parse(state)?;
		Ok((
			state,
			ast::VarAssign {
				names,
				equals,
				exprlist,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseLocalAssignName;
define_parser!(
	ParseLocalAssignName,
	ast::LocalAssignName,
	|_, state: ParseState<'a>| {
		let (state, name) = ParseName.parse(state)?;
		let (state, colon) = optional!(state, ParseSymbol(SymbolType::Colon));
		let (state, type_ref) = if colon.is_some() {
			let (ns, t) = ParseTypeReference.parse(state)?;
			(ns, Some(t))
		} else {
			(state, None)
		};
		Ok((
			state,
			ast::LocalAssignName {
				name,
				colon,
				type_ref,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseLocalAssignNameList;
define_parser!(
	ParseLocalAssignNameList,
	ast::LocalAssignNameList,
	|_, state: ParseState<'a>| {
		let (state, result) =
			ParseLikeArgsRequired(ParseLocalAssignName, "<id>".to_string()).parse(state)?;
		Ok((state, ast::LocalAssignNameList(result)))
	}
);

#[derive(Debug)]
pub struct ParseLocalAssignment;
define_parser!(
	ParseLocalAssignment,
	ast::LocalAssignment,
	|_, state: ParseState<'a>| {
		let (state, local_token) =
			ParseToken(TokenType::Keyword(KeywordType::Local)).parse(state)?;
		let (state, names) = ParseLocalAssignNameList.parse(state)?;
		let (mut state, equals_token) =
			optional!(state, ParseToken(TokenType::Symbol(SymbolType::Equal)));
		let mut exprlist = None;
		if equals_token.is_some() {
			let (ns, list) = expect!(
				state,
				ParseExprListRequired,
				"<exp>",
				"ParseLocalAsssignment"
			);
			state = ns;
			exprlist = Some(list);
		}
		Ok((
			state,
			ast::LocalAssignment {
				local_token,
				names,
				equals_token,
				exprlist,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseLocalFunction;
define_parser!(
	ParseLocalFunction,
	ast::LocalFunction,
	|_, state: ParseState<'a>| {
		let (state, local_token) =
			ParseToken(TokenType::Keyword(KeywordType::Local)).parse(state)?;
		let (state, function_token) =
			ParseToken(TokenType::Keyword(KeywordType::Function)).parse(state)?;
		let (state, name) = expect!(state, ParseName, "<id>", "ParseLocalFunction");
		let (state, body) = ParseFunctionBody.parse(state)?;
		Ok((
			state,
			ast::LocalFunction {
				local_token,
				function_token,
				name,
				body,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseElseIfBlock;
define_parser!(ParseElseIfBlock, ast::ElseIfBlock, |_,
                                                    state: ParseState<
	'a,
>| {
	let (state, else_if_token) =
		ParseToken(TokenType::Keyword(KeywordType::ElseIf)).parse(state)?;
	let (state, condition) = expect!(state, ParseExpr, "<exp>", "ParseElseIfBlock");
	let (state, then_token) = expect!(
		state,
		ParseToken(TokenType::Keyword(KeywordType::Then)),
		"then",
		"ParseElseIfBlock"
	);
	let (state, block) = ParseBlock.parse(state)?;
	Ok((
		state,
		ast::ElseIfBlock {
			else_if_token,
			condition,
			then_token,
			block,
		},
	))
});

#[derive(Debug)]
pub struct ParseElseBlock;
define_parser!(
	ParseElseBlock,
	ast::ElseBlock,
	|_, state: ParseState<'a>| {
		let (state, else_token) = ParseToken(TokenType::Keyword(KeywordType::Else)).parse(state)?;
		let (state, block) = ParseBlock.parse(state)?;
		Ok((state, ast::ElseBlock { else_token, block }))
	}
);

#[derive(Debug)]
pub struct ParseIfStmt;
define_parser!(ParseIfStmt, ast::IfStmt, |_, state: ParseState<'a>| {
	let (state, if_token) = ParseToken(TokenType::Keyword(KeywordType::If)).parse(state)?;
	let (state, condition) = expect!(state, ParseExpr, "<exp>", "ParseIfStmt");
	let (state, then_token) = expect!(
		state,
		ParseToken(TokenType::Keyword(KeywordType::Then)),
		"then",
		"ParseIfStmt"
	);
	let (state, block) = ParseBlock.parse(state)?;
	let (state, elseif_blocks) = ParseOptionalOrMore(ParseElseIfBlock).parse(state)?;
	let (state, else_block) = optional!(state, ParseElseBlock);
	let (state, end_token) = expect!(
		state,
		ParseToken(TokenType::Keyword(KeywordType::End)),
		"end",
		"ParseIfStmt"
	);
	Ok((
		state,
		ast::IfStmt {
			if_token,
			condition,
			then_token,
			block,
			elseif_blocks,
			else_block,
			end_token,
		},
	))
});

#[derive(Debug)]
pub struct ParseGenericFor;
define_parser!(
	ParseGenericFor,
	ast::GenericFor,
	|_, state: ParseState<'a>| {
		let (state, for_token) = ParseToken(TokenType::Keyword(KeywordType::For)).parse(state)?;
		let (state, names) = ParseLocalAssignNameList.parse(state)?;
		let (state, in_token) = ParseToken(TokenType::Keyword(KeywordType::In)).parse(state)?;
		let (state, exprlist) = ParseExprListRequired.parse(state)?;
		let (state, do_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::Do)),
			"do",
			"ParseGenericFor"
		);
		let (state, block) = ParseBlock.parse(state)?;
		let (state, end_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::End)),
			"end",
			"ParseGenericFor"
		);
		Ok((
			state,
			ast::GenericFor {
				for_token,
				names,
				in_token,
				exprlist,
				do_token,
				block,
				end_token,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseNumericFor;
define_parser!(
	ParseNumericFor,
	ast::NumericFor,
	|_, state: ParseState<'a>| {
		let (state, for_token) = ParseToken(TokenType::Keyword(KeywordType::For)).parse(state)?;
		let (state, name) = ParseName.parse(state)?;
		let (state, equals_token) = ParseSymbol(SymbolType::Equal).parse(state)?;
		let (state, start) = expect!(state, ParseExpr, "<exp>", "ParseNumericFor");
		let (state, start_comma) = expect!(
			state,
			ParseSymbol(SymbolType::Comma),
			",",
			"ParseNumericFor"
		);
		let (state, end) = expect!(state, ParseExpr, "<exp>", "ParseNumericFor");
		let (mut state, end_comma) = optional!(state, ParseSymbol(SymbolType::Comma));
		let mut step = None;

		if end_comma.is_some() {
			let (ns, expr) = expect!(state, ParseExpr, "<expr>", "ParseNumericFor");
			state = ns;
			step = Some(expr);
		}

		let (state, do_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::Do)),
			"do",
			"ParseNumericFor"
		);
		let (state, block) = ParseBlock.parse(state)?;
		let (state, end_token) = expect!(
			state,
			ParseToken(TokenType::Keyword(KeywordType::End)),
			"end",
			"ParseNumericFor"
		);
		Ok((
			state,
			ast::NumericFor {
				for_token,
				name: Box::new(name),
				equals_token,
				start: Box::new(start),
				start_comma,
				end: Box::new(end),
				end_comma,
				step,
				do_token,
				block,
				end_token,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseFunctionAssign;
define_parser!(
	ParseFunctionAssign,
	ast::FunctionAssign,
	|_, state: ParseState<'a>| {
		let (state, function_token) =
			ParseToken(TokenType::Keyword(KeywordType::Function)).parse(state)?;
		let (state, name) = expect!(
			state,
			ParseFunctionAssignName,
			"<id>",
			"ParseFunctionAssign"
		);
		let (state, body) = ParseFunctionBody.parse(state)?;
		Ok((
			state,
			ast::FunctionAssign {
				function_token,
				name,
				body,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseTypeDeclaration;
define_parser!(
	ParseTypeDeclaration,
	ast::TypeDeclaration,
	|_, state: ParseState<'a>| {
		let (state, type_token) = ParseToken(TokenType::Keyword(KeywordType::Type)).parse(state)?;

		// lua compatibility, we don't want to expect a name. it will be horrible!
		let (state, name) = ParseTypeDeclarationName.parse(state)?;
		let (state, equals_token) = expect!(
			state,
			ParseSymbol(SymbolType::Equal),
			"=",
			"ParseTypeDeclaration"
		);
		let (state, value) = expect!(state, ParseTypeReference, "<type>", "ParseTypeDeclaration");
		Ok((
			state,
			ast::TypeDeclaration {
				type_token,
				name,
				equals_token,
				value,
			},
		))
	}
);

#[derive(Debug)]
pub struct ParseStmt;
define_parser!(
	ParseStmt,
	ast::Stmt,
	|_, state: ParseState<'a>| parse_either!(state, {
		ParseFunctionCall => ast::Stmt::Call,
		ParseDoStmt => ast::Stmt::Do,
		ParseGenericFor => ast::Stmt::GenericFor,
		ParseIfStmt => ast::Stmt::If,
		ParseFunctionAssign => ast::Stmt::FunctionAssign,

		// it has problems when parsing the assignment first
		ParseLocalFunction => ast::Stmt::LocalFunction,
		ParseLocalAssignment => ast::Stmt::LocalAssignment,

		ParseNumericFor => ast::Stmt::NumericFor,
		ParseRepeatStmt => ast::Stmt::Repeat,
		ParseWhileStmt => ast::Stmt::While,
		ParseVarAssign => ast::Stmt::VarAssign,

		ParseTypeDeclaration => ast::Stmt::TypeDeclaration,
	})
);

#[derive(Debug)]
pub struct ParseLastStmt;
define_parser!(
	ParseLastStmt,
	ast::LastStmt,
	|_, state: ParseState<'a>| parse_either!(state, {
		ParseBreakStmt => ast::LastStmt::Break,
		ParseReturnStmt => ast::LastStmt::Return,
	})
);

#[derive(Debug)]
pub struct ParseStmts;
define_parser!(ParseStmts, ast::Stmts, |_, state: ParseState<'a>| {
	let mut collection = Vec::new();
	let mut state = state.advance(0);
	loop {
		// it's hard to predict when it's going to be over
		// so I made this condition right down there.
		let (ns, stmt) = match ParseStmt.parse(state.advance(0)) {
			Ok((s, st)) => (s, st),
			Err(ParseError::NoMatch) => break,
			Err(err) => return Err(err),
		};

		// don't forget semicolons!
		let (ns, semicolon) = optional!(ns, ParseSymbol(SymbolType::Semicolon));
		collection.push((stmt, semicolon));
		state = ns;
	}

	let (state, last_stmt) = optional!(state, ParseLastStmt);
	Ok((state, ast::Stmts(collection, last_stmt)))
});

#[derive(Debug)]
pub struct ParseBlock;
define_parser!(ParseBlock, ast::Block, |_, state: ParseState<'a>| {
	let (state, stmts) = ParseStmts.parse(state)?;
	Ok((state, ast::Block { stmts }))
});

pub fn parse_from_tokens(tokens: &'_ [AstToken]) -> Result<ast::Block, ParseError> {
	let state = ParseState::new(tokens);
	match ParseBlock.parse(state.advance(0)) {
		Ok((state, block)) => {
			let kind = state.peek().map(|v| v.token_type().clone());
			if matches!(kind, Some(TokenType::Eof)) {
				Ok(block)
			} else {
				Err(ParseError::UnexpectedToken {
					token: state.peek().unwrap().clone(),
				})
			}
		},
		Err(ParseError::NoMatch) => Err(ParseError::UnexpectedToken {
			token: state.peek().unwrap().clone(),
		}),
		Err(err) => Err(err),
	}
}
