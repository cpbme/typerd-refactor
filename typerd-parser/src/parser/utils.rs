use super::*;
use crate::prelude::*;
use ast::{AstToken, Location, SymbolType};

#[derive(Debug, PartialEq)]
pub enum ParseError {
	ExpectedToken {
		token: AstToken,
		expected: String,
		source: String,
	},
	UnexpectedToken {
		token: AstToken,
	},
	NoMatch,
}

impl ParseError {
	pub fn location(&self) -> Location {
		use ParseError::*;
		match self {
			ExpectedToken { token, .. } => token.location(),
			UnexpectedToken { token } => token.location(),
			NoMatch => panic!("No token found (internal ast error)"),
		}
	}

	pub fn message(&self) -> String {
		use ParseError::*;
		match self {
			NoMatch => "Internal ast error".into(),
			UnexpectedToken { token } => {
				format!("Unexpected token: {}", token.token().token_type())
			},
			ExpectedToken {
				token, expected, ..
			} => format!(
				"Expected token `{}` got `{}`",
				expected,
				token.token().token_type()
			),
		}
	}
}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use ParseError::*;
		match self {
			NoMatch => write!(f, "{}", self.message()),
			_ => write!(f, "{}: {}", self.location().start, self.message()),
		}
	}
}

pub type ParseResult<'a, T> = Result<(ParseState<'a>, T), ParseError>;

pub trait Parser<'a> {
	type Item: 'a;

	fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item>;
}

pub struct ParseOptionalOrMore<T>(pub T);
impl<'a, T> Parser<'a> for ParseOptionalOrMore<T>
where
	T: Parser<'a>,
{
	type Item = Vec<T::Item>;

	fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item> {
		let mut state = state.advance(0);
		let mut collection = Vec::new();
		loop {
			match self.0.parse(state.advance(0)) {
				Ok((ns, node)) => {
					state = ns;
					collection.push(node);
				},
				Err(ParseError::NoMatch) => break,
				Err(err) => return Err(err),
			};
		}
		Ok((state, collection))
	}
}

#[macro_export]
macro_rules! define_parser {
	($name:ty, $rt:ty, $body:expr) => {
		impl<'a> Parser<'a> for $name {
			type Item = $rt;

			fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item> {
				$body(self, state)
			}
		}
	};
}

#[macro_export]
macro_rules! optional {
	($state:expr, $parser:expr) => {{
		let new_state = $state.advance(0);
		match $parser.parse(new_state) {
			Ok((state, node)) => (state, Some(node)),
			Err(ParseError::NoMatch) => ($state, None),
			Err(err) => return Err(err),
		}
	}};
}

#[macro_export]
macro_rules! expect {
	($state:expr, $parser:expr, $expected:expr, $source:expr) => {{
		let new_state = $state.advance(0);
		match $parser.parse($state) {
			Ok((state, node)) => (state, node),
			Err(ParseError::NoMatch) => {
				return Err(ParseError::ExpectedToken {
					token: new_state.advance(0).get_or_last_token().clone(),
					expected: $expected.into(),
					source: $source.into(),
				})
			},
			Err(err) => return Err(err),
		}
	}};
}

#[macro_export]
macro_rules! try_parse {
	($state:expr, $parser:expr) => {
		$parser.parse($state.advance(0))
	};
}

#[macro_export]
macro_rules! parse_either {
	(
		$state:expr,
		{
			$(
				$parser:expr => $callback:expr,
			)*
		}
	) => {
		{
			$(
				#[allow(unreachable_patterns)]
				match $parser.parse($state.advance(0)) {
					Ok((state, node)) => return Ok((state, $callback(node))),
					Err(ParseError::NoMatch) => {},
					Err(err) => return Err(err),
				}
			)*
			Err(ParseError::NoMatch)
		}
	};
}

pub struct ParseLikeArgsRequired<T>(pub T, pub String);
impl<'a, T> Parser<'a> for ParseLikeArgsRequired<T>
where
	T: Parser<'a>,
{
	type Item = Vec<(T::Item, Option<AstToken>)>;

	fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item> {
		let mut collection = Vec::new();
		let (mut state, mut last_parsed) =
			expect!(state, self.0, self.1.to_string(), "ParseLikeArgsRequired");
		loop {
			let (ns, comma) = optional!(state.advance(0), ParseSymbol(SymbolType::Comma));
			if comma.is_none() {
				collection.push((last_parsed, None));
				break;
			}

			collection.push((last_parsed, comma));

			let (ns, parsed) = expect!(ns, self.0, self.1.to_string(), "ParseLikeArgsRequired");
			last_parsed = parsed;
			state = ns;
		}
		Ok((state, collection))
	}
}

/// what a horrible name
///
/// (pub T: Parser, pub String: expected name)
pub struct ParseLikeArgs<T>(pub T, pub String);
impl<'a, T> Parser<'a> for ParseLikeArgs<T>
where
	T: Parser<'a>,
{
	type Item = Vec<(T::Item, Option<AstToken>)>;

	fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item> {
		let mut collection: Vec<(T::Item, Option<AstToken>)> = Vec::new();
		let (mut state, mut last_parsed) = optional!(state, self.0);
		if last_parsed.is_some() {
			loop {
				let (ns, comma) = optional!(state.advance(0), ParseSymbol(SymbolType::Comma));
				if comma.is_none() {
					collection.push((last_parsed.unwrap(), None));
					break;
				}

				let parsed = last_parsed.unwrap();
				collection.push((parsed, comma));

				let (ns, parsed) = expect!(ns, self.0, self.1.to_string(), "ParseLikeArgs");
				last_parsed = Some(parsed);
				state = ns;
			}
		}
		Ok((state, collection))
	}
}

/// (pub T: Parser, pub String: expected name)
pub struct ParseOneOrMore<T>(pub T, pub String);
impl<'a, T> Parser<'a> for ParseOneOrMore<T>
where
	T: Parser<'a>,
{
	type Item = Vec<T::Item>;

	fn parse(&self, state: ParseState<'a>) -> ParseResult<'a, Self::Item> {
		let mut state = state;
		let mut collection = Vec::new();
		match self.0.parse(state.advance(0)) {
			Ok((ns, node)) => {
				state = ns;
				collection.push(node);
			},
			Err(ParseError::NoMatch) => {
				return Err(ParseError::ExpectedToken {
					token: state.peek().unwrap().clone(),
					expected: self.1.to_string(),
					source: "ParseOneOrMore".into(),
				})
			},
			Err(err) => return Err(err),
		};
		loop {
			let (ns, result) = optional!(state, self.0);
			state = ns;
			match result {
				Some(node) => collection.push(node),
				None => break,
			};
		}
		Ok((state, collection))
	}
}
