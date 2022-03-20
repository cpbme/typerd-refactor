#[allow(unused_imports)]
#[allow(unused_macros)]
mod tests;

use crate::{prelude::*, Input};
use either::{Either, Left, Right};

use ast::{AstToken, KeywordType, Location, Position, SymbolType, Token, TokenType};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenizeError {
	UnknownCharacter {
		location: Location,
		character: char,
	},
	MalformedNumber {
		location: Location,
		nearest: char,
	},
	MalformedString {
		location: Location,
		nearest: char,
	},
	MalformedComment {
		location: Location,
		nearest: char,
	},
	MalformedShebang {
		location: Location,
		contents: String,
		nearest: char,
	},
}

impl TokenizeError {
	pub fn location(&self) -> &Location {
		use TokenizeError::*;
		match self {
			UnknownCharacter { location, .. } => location,
			MalformedNumber { location, .. } => location,
			MalformedString { location, .. } => location,
			MalformedShebang { location, .. } => location,
			MalformedComment { location, .. } => location,
		}
	}

	pub fn message(&self) -> String {
		use TokenizeError::*;
		match self {
			UnknownCharacter { character, .. } => {
				format!("Unknown character: `{}`", character.escape_default())
			},
			MalformedNumber { nearest, .. } => {
				format!("Malformed number near `{}`", nearest.escape_default())
			},
			MalformedString { nearest: quote, .. } => {
				format!("Malformed string near `{}`", quote.escape_default())
			},
			MalformedShebang {
				contents, nearest, ..
			} => {
				format!(
					"Malformed shebang: `{}` near `{}`. Did you forget to add a new line?",
					contents,
					nearest.escape_default()
				)
			},
			MalformedComment { nearest, .. } => {
				format!("Malformed comment near `{}`", nearest.escape_default())
			},
		}
	}
}

impl std::fmt::Display for TokenizeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}: {}", self.location().start, self.message())
	}
}

impl std::error::Error for TokenizeError {
	fn description(&self) -> &str {
		"lex error"
	}
}

pub type TokenizeResult<T = Token> = Result<T, TokenizeError>;

macro_rules! ret_token {
	($self:expr, $kind:expr) => {{
		let start = $self.input.position();
		$self.input.bump();
		Ok(Token::new(
			Location::new(start, $self.input.position()),
			$kind,
		))
	}};
	($self:expr, $kind:expr, $times:expr) => {{
		let start = $self.input.position();
		$self.input.skip($times);
		Ok(Token::new(
			Location::new(start, $self.input.position()),
			$kind,
		))
	}};
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Tokenizer<'a> {
	input: Input<'a>,
}

impl<'a> Tokenizer<'a> {
	pub fn new(input: Input<'a>) -> Self {
		Self { input }
	}

	fn multiline_comment(&mut self, start: Position) -> TokenizeResult<Either<Token, String>> {
		debug_assert!(matches!(self.input.current(), '['));
		let mut equals_raw = String::new();
		equals_raw.push(self.input.current());

		self.input.skip(1);

		let mut initial_equals: usize = 0;
		let mut value = String::new();

		// this is where multiline string complexity COMES IN
		while self.input.current() == '=' {
			initial_equals += 1;
			equals_raw.push(self.input.current());
			self.input.bump();
		}

		// there's sort kind of last square bracket before
		// entering to the comment paradise
		let nearest = self.input.current();
		if nearest != '[' {
			return Ok(Right(equals_raw));
		}
		self.input.bump();

		loop {
			let ch = self.input.current();
			if ch == ']' {
				let mut dump_str = String::new();
				dump_str.push(']');

				self.input.bump();

				// we need to check amount of equals' before the string terminates
				let mut equals: usize = 0;
				while self.input.current() == '=' {
					equals += 1;
					dump_str.push('=');
					self.input.bump();
				}

				// compare amount of equals to the initial one
				if initial_equals == equals {
					break;
				}
				value.push_str(&dump_str);
			} else if ch == '\0' {
				break;
			} else {
				value.push(ch);
				self.input.bump();
			}
		}

		let nearest = self.input.current();
		if nearest != ']' {
			let location = Location::new(start, self.input.position());
			Err(TokenizeError::MalformedComment { location, nearest })
		} else {
			self.input.bump();

			let location = Location::new(start, self.input.position());
			Ok(Left(Token::new(
				location,
				TokenType::MultilinedComment {
					equals: initial_equals,
					value,
				},
			)))
		}
	}

	fn multiline_str(&mut self) -> TokenizeResult {
		debug_assert!(matches!(self.input.current(), '['));

		let mut initial_equals: usize = 0;
		let mut value = String::new();

		let start = self.input.position();
		self.input.bump();

		// this is where multiline string complexity COMES IN
		while self.input.current() == '=' {
			initial_equals += 1;
			self.input.bump();
		}

		// there's sort kind of last square bracket before
		// entering to the string paradise
		let nearest = self.input.current();
		if nearest != '[' {
			let location = Location::new(start, self.input.position());
			return Err(TokenizeError::MalformedString { location, nearest });
		}
		self.input.bump();

		loop {
			let ch = self.input.current();
			if ch == ']' {
				let mut dump_str = String::new();
				dump_str.push(']');

				self.input.bump();

				// we need to check amount of equals' before the string terminates
				let mut equals: usize = 0;
				while self.input.current() == '=' {
					equals += 1;
					dump_str.push('=');
					self.input.bump();
				}

				// compare amount of equals to the initial one
				if initial_equals == equals {
					break;
				}
				value.push_str(&dump_str);
			} else if ch == '\0' {
				break;
			} else {
				value.push(ch);
				self.input.bump();
			}
		}

		let nearest = self.input.current();
		if nearest != ']' {
			let location = Location::new(start, self.input.position());
			Err(TokenizeError::MalformedString { location, nearest })
		} else {
			self.input.bump();

			let location = Location::new(start, self.input.position());
			Ok(Token::new(
				location,
				TokenType::MultilinedStr {
					equals: initial_equals,
					value,
				},
			))
		}
	}

	fn line_str(&mut self) -> TokenizeResult {
		use ast::QuoteKind;

		debug_assert!(matches!(self.input.current(), '\'' | '"'));

		let mut value = String::new();

		let start = self.input.position();
		let delimiter = self.input.current();
		let quote_kind = match delimiter {
			'"' => QuoteKind::Double,
			'\'' => QuoteKind::Single,
			_ => panic!("Expected `'` or `\"'"),
		};
		self.input.bump();

		loop {
			let ch = self.input.current();
			if ch == delimiter || matches!(ch, '\n' | '\0') {
				break;
			} else if ch == '\\' {
				// unescape characters, if it can
				let escape_char = self.input.bump();
				value.push(match escape_char {
					'n' => '\n',
					't' => '\t',
					c => c,
				});
				self.input.bump();
			} else {
				value.push(ch);
				self.input.bump();
			}
		}

		let location = Location::new(start, self.input.position());
		let ch = self.input.current();
		if ch != delimiter {
			Err(TokenizeError::MalformedString {
				location,
				nearest: ch,
			})
		} else {
			self.input.bump();
			Ok(Token::new(
				location,
				TokenType::LineStr { quote_kind, value },
			))
		}
	}

	fn is_number(&self) -> bool {
		matches!(self.input.current(), '0'..='9')
	}

	fn digits_with_separator(&mut self) -> String {
		let mut value = String::new();
		loop {
			if self.is_number() {
				value.push(self.input.current());
				self.input.bump();
			} else {
				break value;
			}
		}
	}

	fn sci_notation(&mut self, start: &Position) -> TokenizeResult<String> {
		debug_assert!(matches!(self.input.current(), 'e' | 'E'));

		let mut result = String::new();
		result.push(self.input.current());

		let ch = self.input.bump();
		if matches!(ch, '-' | '+') {
			result.push(ch);
			self.input.bump();
		}

		if !self.is_number() {
			Err(TokenizeError::MalformedNumber {
				location: Location::new(*start, self.input.position()),
				nearest: self.input.current(),
			})
		} else {
			result.push_str(&self.digits_with_separator());
			Ok(result)
		}
	}

	fn basic_number(&mut self, start: &Position) -> TokenizeResult<String> {
		let mut digits = self.digits_with_separator();
		if self.input.current() == '.' {
			digits.push('.');

			self.input.bump();
			digits.push_str(&self.digits_with_separator());
		}
		let ch = self.input.current();
		if matches!(ch, 'e' | 'E') {
			digits.push_str(&self.sci_notation(start)?)
		}
		Ok(digits)
	}

	fn no_int_fractional_number(&mut self, start: &Position) -> TokenizeResult<String> {
		debug_assert!(self.input.current() == '.');

		let mut result = String::from(self.input.current());
		self.input.bump();

		result.push_str(&self.sci_notation(start)?);
		Ok(result)
	}

	fn hex_number(&mut self) -> String {
		let mut result = String::from("0x");
		self.input.bump();
		self.input.bump();

		loop {
			let ch = self.input.current();
			if ch.is_ascii_hexdigit() {
				result.push(ch);
				self.input.bump();
			} else {
				break result;
			}
		}
	}

	fn number(&mut self) -> TokenizeResult {
		let start = self.input.position();
		let current = self.input.current();
		let lookahead = self.input.lookahead();
		let value = if current == '0' && lookahead == Some(&'x') {
			self.hex_number()
		} else if current == '.' {
			self.no_int_fractional_number(&start)?
		} else {
			self.basic_number(&start)?
		};

		Ok(Token::new(
			Location::new(start, self.input.position()),
			TokenType::Number(value),
		))
	}

	fn name(&mut self) -> TokenizeResult {
		debug_assert!(matches!(self.input.current(), 'a'..='z'|'A'..='Z'|'_'));

		let mut name = String::new();
		let start = self.input.position();

		loop {
			let ch = self.input.current();
			if matches!(ch, 'a'..='z'|'A'..='Z'|'0'..='9'|'_') {
				name.push(ch);
				self.input.bump();
			} else {
				break;
			}
		}

		let location = Location::new(start, self.input.position());
		if let Some(keyword) = KeywordType::parse(&name) {
			Ok(Token::new(location, TokenType::Keyword(keyword)))
		} else {
			Ok(Token::new(location, TokenType::Name(name)))
		}
	}

	fn whitespace(&mut self) -> TokenizeResult {
		debug_assert!(
			self.input.current().is_ascii_whitespace(),
			"Expected whitespace character!"
		);

		let mut chars = String::new();
		let start = self.input.position();

		loop {
			let ch = self.input.current();
			if ch.is_whitespace() {
				chars.push(ch);
				self.input.bump();
			} else {
				break;
			}
		}

		Ok(Token::new(
			Location::new(start, self.input.position()),
			TokenType::Whitespace(chars),
		))
	}

	pub fn tokenize(&mut self) -> TokenizeResult<Vec<AstToken>> {
		let mut tokens: Vec<AstToken> = Vec::new();
		let mut leadings = Vec::new();
		loop {
			let raw_token = self.advance_token()?;
			let is_eof = raw_token.is_eof();
			if raw_token.is_trivia() {
				leadings.push(raw_token);
			} else {
				let token = AstToken::new(leadings, raw_token);
				leadings = Vec::new();
				tokens.push(token);
			}
			if is_eof {
				break;
			}
		}
		Ok(tokens)
	}

	fn comment_like(&mut self, start: Position) -> TokenizeResult {
		let is_shebang = self.input.current() == '#';

		let mut contents = String::new();
		self.input.skip(2);

		// multiline comment check
		if self.input.current() == '[' && matches!(self.input.lookahead(), Some(&'=' | '[')) {
			match self.multiline_comment(start) {
				Ok(Left(comment)) => return Ok(comment),
				Ok(Right(str)) => contents.push_str(str.as_str()),
				Err(err) => return Err(err),
			};
		}

		loop {
			let ch = self.input.current();
			if !matches!(ch, '\n' | '\0') {
				contents.push(ch);
				self.input.bump();
			} else {
				break;
			}
		}

		// shebang must have newline after that
		if is_shebang {
			let nearest = self.input.current();
			if nearest != '\n' {
				return Err(TokenizeError::MalformedShebang {
					location: Location::new(start, self.input.position()),
					nearest,
					contents,
				});
			}
		}

		Ok(Token::new(
			Location::new(start, self.input.position()),
			if is_shebang {
				TokenType::Shebang(contents)
			} else {
				TokenType::LineComment(contents)
			},
		))
	}

	pub fn advance_token(&mut self) -> TokenizeResult {
		let lookahead = self.input.lookahead().copied().unwrap_or('\0');
		match self.input.current() {
			'#' if self.input.is_first_line() && lookahead == '!' => {
				self.comment_like(self.input.position())
			},
			'-' if lookahead == '-' => self.comment_like(self.input.position()),

			c if c.is_ascii_whitespace() => self.whitespace(),

			'a'..='z' | 'A'..='Z' | '_' => self.name(),
			'0'..='9' => self.number(),

			'[' if lookahead == '[' => self.multiline_str(),
			'[' if lookahead == '=' => self.multiline_str(),

			'\'' | '"' => self.line_str(),

			'.' if matches!(lookahead, '0'..='9') => self.number(),

			'~' if lookahead == '=' => ret_token!(self, TokenType::Symbol(SymbolType::NotEqual), 2),
			'=' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::DoubleEqual), 2)
			},
			'=' => ret_token!(self, TokenType::Symbol(SymbolType::Equal)),

			'@' => ret_token!(self, TokenType::Symbol(SymbolType::At)),
			'#' => ret_token!(self, TokenType::Symbol(SymbolType::Hash)),

			'$' => ret_token!(self, TokenType::Symbol(SymbolType::Dollar)),

			';' => ret_token!(self, TokenType::Symbol(SymbolType::Semicolon)),

			':' if lookahead == ':' => {
				ret_token!(self, TokenType::Symbol(SymbolType::DoubleColon), 2)
			},
			':' => ret_token!(self, TokenType::Symbol(SymbolType::Colon)),

			',' => ret_token!(self, TokenType::Symbol(SymbolType::Comma)),

			'{' => ret_token!(self, TokenType::Symbol(SymbolType::OpenCurly)),
			'}' => ret_token!(self, TokenType::Symbol(SymbolType::CloseCurly)),

			'[' => ret_token!(self, TokenType::Symbol(SymbolType::OpenBracket)),
			']' => ret_token!(self, TokenType::Symbol(SymbolType::CloseBracket)),

			'(' => ret_token!(self, TokenType::Symbol(SymbolType::OpenParen)),
			')' => ret_token!(self, TokenType::Symbol(SymbolType::CloseParen)),

			'>' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::GreaterEqual), 2)
			},
			'>' => ret_token!(self, TokenType::Symbol(SymbolType::GreaterThan)),

			'<' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::LessEqual), 2)
			},
			'<' => ret_token!(self, TokenType::Symbol(SymbolType::LessThan)),

			'+' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::CrossEqual), 2)
			},
			'+' => ret_token!(self, TokenType::Symbol(SymbolType::Cross)),

			'-' if lookahead == '>' => {
				ret_token!(self, TokenType::Symbol(SymbolType::SkinnyArrow), 2)
			},
			'-' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::DashEqual), 2)
			},
			'-' => ret_token!(self, TokenType::Symbol(SymbolType::Dash)),

			'*' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::AsteriskEqual), 2)
			},
			'*' => ret_token!(self, TokenType::Symbol(SymbolType::Asterisk)),

			'^' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::CaretEqual), 2)
			},
			'^' => ret_token!(self, TokenType::Symbol(SymbolType::Caret)),

			'/' if lookahead == '=' => {
				ret_token!(self, TokenType::Symbol(SymbolType::SlashEqual), 2)
			},
			'/' if lookahead == '/' => {
				ret_token!(self, TokenType::Symbol(SymbolType::DoubleSlash), 2)
			},
			'/' => ret_token!(self, TokenType::Symbol(SymbolType::Slash)),

			'%' => ret_token!(self, TokenType::Symbol(SymbolType::Percent)),

			'.' if lookahead == '.' => {
				// not so fast yet...
				self.input.bump();

				let lookahead = self.input.lookahead().cloned().unwrap_or('\0');
				match lookahead {
					'.' => ret_token!(self, TokenType::Symbol(SymbolType::TripleDots), 2),
					'=' => ret_token!(self, TokenType::Symbol(SymbolType::ConcatEqual), 2),
					_ => ret_token!(self, TokenType::Symbol(SymbolType::DoubleDots)),
				}
			},
			'.' => ret_token!(self, TokenType::Symbol(SymbolType::Dot)),

			'?' if lookahead == '?' => {
				ret_token!(self, TokenType::Symbol(SymbolType::DoubleQuestionMarks), 2)
			},
			'?' => ret_token!(self, TokenType::Symbol(SymbolType::QuestionMark)),

			'&' => ret_token!(self, TokenType::Symbol(SymbolType::Ampersand)),
			'|' => ret_token!(self, TokenType::Symbol(SymbolType::VerticalBar)),

			'\0' => ret_token!(self, TokenType::Eof),
			_ => {
				let start = self.input.position();
				let last_char = self.input.current();
				self.input.bump();
				Err(TokenizeError::UnknownCharacter {
					location: Location::new(start, self.input.position()),
					character: last_char,
				})
			},
		}
	}
}
