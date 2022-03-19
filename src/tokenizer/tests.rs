use super::*;
use crate::Location;
use crate::SymbolKind;

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
	let all_symbols = SymbolKind::str_entries()
		.iter()
		.fold(String::new(), |mut initial, sym| {
			// it's a string slice
			initial.push_str(sym);
			initial.push(' ');
			initial
		});

	println!("Total symbols: `{}`", all_symbols);

	let mut tokenizer = init_tokenizer!(&all_symbols);
	let symbols = SymbolKind::entries();
	for symbol in symbols.iter() {
		let token = tokenizer.advance_token().unwrap();
		println!("Comparing: {:#?} to {:#?}", &token, &symbol);
		assert!(token.kind == TokenKind::Symbol(symbol.clone()));
		tokenizer.advance_token().unwrap();
	}
}

#[test]
fn tokenize_string_multiple_lines() {
	let mut tokenizer =
		init_tokenizer!("[[alo\n123\"]] [==[\nalo\n123\"]==] [==[this is confusable]=]==]");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::MultilineStr {
			equals: 0,
			value: "alo\n123\"".into(),
		},
		TokenKind::MultilineStr {
			equals: 2,
			value: "\nalo\n123\"".into(),
		},
		TokenKind::MultilineStr {
			equals: 2,
			value: "this is confusable]=".into(),
		},
	});
}

#[test]
fn tokenize_name_and_keywords() {
	let mut tokenizer = init_tokenizer!("hello _ _H _123 _A1 local");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::Name("hello".into()),
		TokenKind::Name("_".into()),
		TokenKind::Name("_H".into()),
		TokenKind::Name("_123".into()),
		TokenKind::Name("_A1".into()),
		TokenKind::Keyword(KeywordKind::Local),
	});
}

#[test]
fn tokenize_comment_confusables() {
	let mut tokenizer = init_tokenizer!("#!/usr/bin/env node\n-- hi ");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::Shebang("/usr/bin/env node".into()),
		TokenKind::LineComment(" hi ".into()),
		TokenKind::Eof,
	});
}

#[test]
fn tokenize_string_single_line() {
	use crate::QuoteKind;

	let mut tokenizer =
		init_tokenizer!("\"String one\" 'String two' 'Escape test\\'' \"AnotherOne\\n\" ");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::LineStr {
			quote_kind: QuoteKind::Double,
			value: "String one".into(),
		},
		TokenKind::LineStr {
			quote_kind: QuoteKind::Single,
			value: "String two".into(),
		},
		TokenKind::LineStr {
			quote_kind: QuoteKind::Single,
			value: "Escape test'".into(),
		},
		TokenKind::LineStr {
			quote_kind: QuoteKind::Double,
			value: "AnotherOne\n".into(),
		},
		TokenKind::Eof,
	});
}

#[test]
fn tokenize_shebang() {
	let mut tokenizer = init_tokenizer!("#!/usr/bin/env node\n# ");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::Shebang("/usr/bin/env node".into()),
		TokenKind::Symbol(SymbolKind::Hash),
		TokenKind::Eof,
	});
}

#[test]
fn tokenize_number() {
	let mut tokenizer = init_tokenizer!("3 3.0 3.1416 314.16e-2 0.31416E1 0xfF 0x56");
	cmp_advance_tokens!(tokenizer, {
		TokenKind::Number("3".into()),
		TokenKind::Number("3.0".into()),
		TokenKind::Number("3.1416".into()),
		TokenKind::Number("314.16e-2".into()),
		TokenKind::Number("0.31416E1".into()),
		TokenKind::Number("0xfF".into()),
		TokenKind::Number("0x56".into()),
	});
}
