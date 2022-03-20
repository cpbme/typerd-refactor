// I violated the rust styling guide, but I don't care.

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use crate::*;

#[allow(unused_imports)]
use prelude::ast::TokenType;

mod types;

#[macro_export]
macro_rules! test_suite {
	($name:ident, $text:expr, $body:expr) => {
		#[allow(dead_code, unused_assignments)]
		#[test]
		fn $name() {
			let tokens = tokenize($text).expect("Tokenization failed!");
			let state = ParseState::new(&tokens);
			$body(state);
		}
	};
}

test_suite!(parse_number, "1234567890", |state: ParseState| {
	ParseNumber.parse(state).expect("Failed");
});

test_suite!(parse_bool_true, "true", |state: ParseState| {
	ParseBool.parse(state).expect("Failed");
});

test_suite!(parse_bool_false, "false", |state: ParseState| {
	ParseBool.parse(state).expect("Failed");
});

test_suite!(parse_nil, "nil", |state: ParseState| {
	ParseNil.parse(state).expect("Failed");
});

test_suite!(parse_str_single, "'Hi'", |state: ParseState| {
	ParseStr.parse(state).expect("Failed");
});

test_suite!(parse_str_multiline, "[[Wow]]", |state: ParseState| {
	ParseStr.parse(state).expect("Failed");
});

test_suite!(parse_varargs, "...", |state: ParseState| {
	ParseVarargs.parse(state).expect("Failed");
});

test_suite!(parse_binary, "true and false", |state: ParseState| {
	ParseExpr.parse(state).expect("Failed");
});

test_suite!(parse_unary, "-yes", |state: ParseState| {
	ParseUnary.parse(state).expect("Failed");
});

test_suite!(parse_paren, "(-yes and no)", |state: ParseState| {
	ParseParen.parse(state).expect("Failed");
});

test_suite!(parse_access_idents, "yes.that", |state: ParseState| {
	let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_access_multiple_idents,
	"yes.that.lol",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_access_expr, "yes['that']", |state: ParseState| {
	let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_accesses_odd,
	"yes['that'].thing['is working']",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_table_ctor_empty, "{{}}", |state: ParseState| {
	let (state, _) = ParseTableCtor.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_table_ctor_expr_index,
	"{{ [\"Hi\"] = 123 }}",
	|state: ParseState| {
		let (state, _) = ParseTableCtor.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_table_ctor_ident_index,
	"{{ Hi = 123 }}",
	|state: ParseState| {
		let (state, _) = ParseTableCtor.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_table_ctor_array_like,
	"{{ 12345, 67890 }}",
	|state: ParseState| {
		let (state, _) = ParseTableCtor.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_table_ctor_odd,
	"{{ 1234, hi = 1234, [string] = 1234 }}",
	|state: ParseState| {
		let (state, _) = ParseTableCtor.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_table_ctor_with_trailing_separator,
	"{{ 1234, hi = 1234, [string] = 1234, }}",
	|state: ParseState| {
		let (state, _) = ParseTableCtor.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	diag_table_ctor_incomplete_expr_index,
	"{{ [string] }}",
	|state: ParseState| {
		ParseTableCtor.parse(state).expect_err("Failed");
	}
);

test_suite!(
	diag_table_ctor_incomplete_ident_index,
	"{{ hi = }}",
	|state: ParseState| {
		ParseTableCtor.parse(state).expect_err("Failed");
	}
);

test_suite!(parse_call_expr, "hello()", |state: ParseState| {
	let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_call_expr_with_args,
	"hello(1, '2', no)",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_call_expr_with_sugars,
	"yes 'hi' 'no'",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_call_expr_odd_combinations,
	"yes() 'Nah' (12345)",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_prefix_expr_all_combinations,
	"(hello):test(_for).prefix['expr'](combinations).so.that[1].we:donthaveproblems()",
	|state: ParseState| {
		let (state, _) = ParsePrefixExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	diag_call_expr_incomplete_args,
	"yes('why',)",
	|state: ParseState| {
		ParsePrefixExpr.parse(state).expect_err("Failed");
	}
);

test_suite!(
	parse_type_assertion_expr,
	"hi as string",
	|state: ParseState| {
		let (state, _) = ParseExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_type_assertion_expr_nested,
	"((yes) as false as 'nope') as unknown",
	|state: ParseState| {
		let (state, _) = ParseExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_func_expr_empty,
	"function() end",
	|state: ParseState| {
		let (state, _) = ParseFunctionExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_func_expr_with_type_params,
	"function<A, B: string>(name: B) end",
	|state: ParseState| {
		let (state, _) = ParseFunctionExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_func_expr_with_return_type,
	"function(name) -> string end",
	|state: ParseState| {
		let (state, _) = ParseFunctionExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_func_expr_full,
	"function<A, B: string>(name: B) -> string end",
	|state: ParseState| {
		let (state, _) = ParseFunctionExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_func_expr,
	"function() print('Hi'); return; end",
	|state: ParseState| {
		let (state, _) = ParseFunctionExpr.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_call_stmt,
	"yes() 'Nah' (12345)",
	|state: ParseState| {
		let (state, _) = ParseStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	diag_disallow_prefix_expr_method,
	"hello:world",
	|state: ParseState| {
		ParsePrefixExpr.parse(state.advance(0)).expect_err("Failed");
		ParseExpr.parse(state.advance(0)).expect_err("Failed");
	}
);

test_suite!(diag_call_stmt_confusable, "yes.why", |state: ParseState| {
	ParseStmt.parse(state).expect_err("Failed");
});

test_suite!(
	parse_stmts,
	"yes(); return \"yes\";",
	|state: ParseState| {
		let (state, _) = ParseStmts.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	diag_stmts_last_stmt_overflow,
	"yes(); return \"yes\"; yes()",
	|state: ParseState| {
		let (state, _) = ParseStmts.parse(state).expect("Failed");
		assert!(!matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_function_empty,
	"local function yes() end",
	|state: ParseState| {
		let (state, _) = ParseLocalFunction.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_function,
	"local function yes() print('Hi') end",
	|state: ParseState| {
		let (state, _) = ParseLocalFunction.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_assignment_no_initializers,
	"local hi",
	|state: ParseState| {
		let (state, _) = ParseLocalAssignment.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_assignment_no_initializers_with_types,
	"local hi: Lol",
	|state: ParseState| {
		let (state, _) = ParseLocalAssignment.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_assignment,
	"local hi = 'no'",
	|state: ParseState| {
		let (state, _) = ParseLocalAssignment.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_assignment_with_explicit_types,
	"local hi: string = 'no'",
	|state: ParseState| {
		let (state, _) = ParseLocalAssignment.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_repeat_stmt_empty,
	"repeat until true",
	|state: ParseState| {
		let (state, _) = ParseRepeatStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_repeat_stmt,
	"repeat print('Hi') until true",
	|state: ParseState| {
		let (state, _) = ParseRepeatStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_if_stmt_empty,
	"if true then end",
	|state: ParseState| {
		let (state, _) = ParseIfStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_if_stmt,
	"if true then print('Hi'); end",
	|state: ParseState| {
		let (state, _) = ParseIfStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_if_stmt_with_elseif,
	"if true then print('Hi'); elseif true then print('No'); end",
	|state: ParseState| {
		let (state, _) = ParseIfStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_if_stmt_with_else,
	"if true then print('Hi'); else print('No'); end",
	|state: ParseState| {
		let (state, _) = ParseIfStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_if_stmt_complete, "if true then print('Hi'); elseif true then print('Na') elseif true then print('No') else print('No'); end", | state: ParseState | {
	let (state, _) = ParseIfStmt.parse(state).expect("Failed");
	assert!(matches!(state.peek().expect("Failed").token_type(), TokenType::Eof));
});

test_suite!(parse_do_stmt_empty, "do end", |state: ParseState| {
	let (state, _) = ParseDoStmt.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(parse_do_stmt, "do print('Hi'); end", |state: ParseState| {
	let (state, _) = ParseDoStmt.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_while_stmt_empty,
	"while true do end",
	|state: ParseState| {
		let (state, _) = ParseWhileStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_while_stmt,
	"while true do print('Hi'); end",
	|state: ParseState| {
		let (state, _) = ParseWhileStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_var_assign_single_var, "a = b", |state: ParseState| {
	let (state, _) = ParseVarAssign.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_var_assign_multiple_vars,
	"a, b, c = d, e, f",
	|state: ParseState| {
		let (state, _) = ParseVarAssign.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_var_assign_complex_vars,
	"a, b['c'], d.e = b, 1, 2",
	|state: ParseState| {
		let (state, _) = ParseVarAssign.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

// makes sure that it doesn't confuse when parsing LocalAssignment
// and LocalFunction.
test_suite!(
	diag_stmt_declaration_confusable,
	"local function that() end",
	|state: ParseState| {
		let (state, _) = ParseStmt.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_local_assign_name,
	"that: string",
	|state: ParseState| {
		ParseLocalAssignName.parse(state).expect("Failed");
	}
);

#[allow(dead_code)]
static LUA_COMPAT_SOURCE: &str = include_str!("binder.ty");

test_suite!(
	compat_lua_51_sample,
	LUA_COMPAT_SOURCE,
	|state: ParseState| {
		let (state, _) = ParseBlock.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(compat_lua_51_type_call, "type(str)", |state: ParseState| {
	let (state, _) = ParseBlock.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	compat_lua_51_type_declaration,
	"type str<T> = lol",
	|state: ParseState| {
		let (state, _) = ParseBlock.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);
