use super::*;

macro_rules! init_tokenizer {
	($text:expr) => {{
		let input = Input::new($text);
		let tokenizer = Tokenizer::new(input);
		tokenizer
	}};
}

macro_rules! cmp_advance_token {
	($tokenizer:expr, $value:expr) => {
		assert_eq!($tokenizer.advance_token().unwrap(), $value);
	};
}

macro_rules! cmp_advance_tokens {
	($tokenizer:expr, { $( $value:expr, )* }) => {
		$(
			cmp_advance_token!(
				$tokenizer,
				Token::new(
					Location::default(),
					$value
				)
			);
			$tokenizer.advance_token().unwrap();
		)*
	};
}

#[test]
fn tokenize_symbols() {
	// nice hack, thanks!
	let all_symbols = SymbolType::str_entries()
		.iter()
		.fold(String::new(), |mut initial, sym| {
			// it's a string slice
			initial.push_str(sym);
			initial.push(' ');
			initial
		});

	println!("Total symbols: `{}`", all_symbols);

	let mut tokenizer = init_tokenizer!(&all_symbols);
	let symbols = SymbolType::entries();
	for symbol in symbols.iter() {
		let token = tokenizer.advance_token().unwrap();
		println!("Comparing: {:#?} to {:#?}", &token, &symbol);
		assert!(token.token_type() == &TokenType::Symbol(symbol.clone()));
		tokenizer.advance_token().unwrap();
	}
}

#[test]
fn tokenize_string_multiple_lines() {
	let mut tokenizer =
		init_tokenizer!("[[alo\n123\"]] [==[\nalo\n123\"]==] [==[this is confusable]=]==]");
	cmp_advance_tokens!(tokenizer, {
		TokenType::MultilinedStr {
			equals: 0,
			value: "alo\n123\"".into(),
		},
		TokenType::MultilinedStr {
			equals: 2,
			value: "\nalo\n123\"".into(),
		},
		TokenType::MultilinedStr {
			equals: 2,
			value: "this is confusable]=".into(),
		},
	});
}

#[test]
fn tokenize_multiline_comment_confusables() {
	let mut tokenizer = init_tokenizer!("--[==incomplete commento\n--[[lol]]\n--[==[hi]==]");
	cmp_advance_tokens!(tokenizer, {
		TokenType::LineComment("[==incomplete commento".into()),
		TokenType::MultilinedComment {
			equals: 0,
			value: "lol".into(),
		},
		TokenType::MultilinedComment {
			equals: 2,
			value: "hi".into(),
		},
	});
}

#[test]
fn tokenize_multiline_comment() {
	let mut tokenizer =
		init_tokenizer!("--[[alo\n123\"]] --[==[\nalo\n123\"]==] --[==[this is confusable]=]==]");
	cmp_advance_tokens!(tokenizer, {
		TokenType::MultilinedComment {
			equals: 0,
			value: "alo\n123\"".into(),
		},
		TokenType::MultilinedComment {
			equals: 2,
			value: "\nalo\n123\"".into(),
		},
		TokenType::MultilinedComment {
			equals: 2,
			value: "this is confusable]=".into(),
		},
	});
}

#[test]
fn tokenize_name_and_keywords() {
	let mut tokenizer = init_tokenizer!("hello _ _H _123 _A1 local");
	cmp_advance_tokens!(tokenizer, {
		TokenType::Name("hello".into()),
		TokenType::Name("_".into()),
		TokenType::Name("_H".into()),
		TokenType::Name("_123".into()),
		TokenType::Name("_A1".into()),
		TokenType::Keyword(KeywordType::Local),
	});
}

#[test]
fn tokenize_comment_confusables() {
	let mut tokenizer = init_tokenizer!("#!/usr/bin/env node\n-- hi ");
	cmp_advance_tokens!(tokenizer, {
		TokenType::Shebang("/usr/bin/env node".into()),
		TokenType::LineComment(" hi ".into()),
		TokenType::Eof,
	});
}

#[test]
fn tokenize_string_single_line() {
	use ast::QuoteKind;

	let mut tokenizer =
		init_tokenizer!("\"String one\" 'String two' 'Escape test\\'' \"AnotherOne\\n\" ");
	cmp_advance_tokens!(tokenizer, {
		TokenType::LineStr {
			quote_kind: QuoteKind::Double,
			value: "String one".into(),
		},
		TokenType::LineStr {
			quote_kind: QuoteKind::Single,
			value: "String two".into(),
		},
		TokenType::LineStr {
			quote_kind: QuoteKind::Single,
			value: "Escape test'".into(),
		},
		TokenType::LineStr {
			quote_kind: QuoteKind::Double,
			value: "AnotherOne\n".into(),
		},
		TokenType::Eof,
	});
}

#[test]
fn tokenize_shebang() {
	let mut tokenizer = init_tokenizer!("#!/usr/bin/env node\n# ");
	cmp_advance_tokens!(tokenizer, {
		TokenType::Shebang("/usr/bin/env node".into()),
		TokenType::Symbol(SymbolType::Hash),
		TokenType::Eof,
	});
}

#[test]
fn tokenize_number() {
	let mut tokenizer = init_tokenizer!("3 3.0 3.1416 314.16e-2 0.31416E1 0xfF 0x56");
	cmp_advance_tokens!(tokenizer, {
		TokenType::Number("3".into()),
		TokenType::Number("3.0".into()),
		TokenType::Number("3.1416".into()),
		TokenType::Number("314.16e-2".into()),
		TokenType::Number("0.31416E1".into()),
		TokenType::Number("0xfF".into()),
		TokenType::Number("0x56".into()),
	});
}
