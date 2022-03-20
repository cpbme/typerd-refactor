// I violated the rust styling guide, but I don't care.

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use crate::*;

#[allow(unused_imports)]
use prelude::ast::TokenType;

test_suite!(parse_type_name, "hello", |state: ParseState| {
	let (state, _) = ParseTypeReference.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(parse_type_declaration_name, "hello", |state: ParseState| {
	let (state, _) = ParseTypeDeclarationName.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_type_declaration_name_with_params,
	"hello<A, B: string>",
	|state: ParseState| {
		let (state, _) = ParseTypeDeclarationName.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_generic_type, "hello<T>", |state: ParseState| {
	let (state, _) = ParseTypeReference.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(parse_union_type, "hello | 123", |state: ParseState| {
	let (state, _) = ParseTypeReference.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_intersection_type,
	"hello & 123",
	|state: ParseState| {
		let (state, _) = ParseTypeReference.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_parenthesized_type,
	"(hello & 123)",
	|state: ParseState| {
		let (state, _) = ParseTypeReference.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(parse_bool_true_type, "true", |state: ParseState| {
	let (state, _) = ParseTypeReference.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(parse_bool_false_type, "false", |state: ParseState| {
	let (state, _) = ParseTypeReference.parse(state).expect("Failed");
	assert!(matches!(
		state.peek().expect("Failed").token_type(),
		TokenType::Eof
	));
});

test_suite!(
	parse_conditional_type,
	"T :: string ? 1 : 2",
	|state: ParseState| {
		let (state, _) = ParseTypeReference.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);

test_suite!(
	parse_value_nested_types,
	"(hello | 123 & ...) :: true ? false : number",
	|state: ParseState| {
		let (state, _) = ParseTypeReference.parse(state).expect("Failed");
		assert!(matches!(
			state.peek().expect("Failed").token_type(),
			TokenType::Eof
		));
	}
);
