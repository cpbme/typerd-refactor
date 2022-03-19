#[cfg(target_feature = "serde")]
use serde::{Deserialize, Serialize};

mod types;
pub use types::*;

use crate::Location;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum QuoteKind {
	Single,
	Double,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenKind {
	Eof,
	Keyword(KeywordKind),
	LineComment(String),
	LineStr {
		quote_kind: QuoteKind,
		value: String,
	},
	MultilineStr {
		equals: usize,
		value: String,
	},
	Name(String),
	Number(String),
	Shebang(String),
	Symbol(SymbolKind),
	Whitespace(String),
}

impl TokenKind {
	pub fn is_eof(&self) -> bool {
		matches!(self, TokenKind::Eof)
	}

	pub fn is_trivia(&self) -> bool {
		matches!(
			self,
			TokenKind::LineComment(..) | TokenKind::Shebang(..) | TokenKind::Whitespace(..)
		)
	}

	pub fn spaces(count: usize) -> Self {
		TokenKind::Whitespace(" ".repeat(count))
	}

	pub fn tabs(count: usize) -> Self {
		TokenKind::Whitespace("\t".repeat(count))
	}

	pub fn value(&self) -> String {
		match self {
			TokenKind::Eof => "<eof>".into(),
			TokenKind::Keyword(k) => k.str().to_string(),
			TokenKind::LineComment(value) => format!("--{}", value),
			TokenKind::LineStr { quote_kind, value } => {
				use QuoteKind::*;
				format!(
					"{0}{1}{0}",
					match quote_kind {
						Single => "'",
						Double => "\"",
					},
					value
				)
			},
			TokenKind::MultilineStr { equals, value } => {
				format!("[{1}[{0}]{1}]", value, ".".repeat(*equals))
			},
			TokenKind::Name(n) => n.to_string(),
			TokenKind::Number(n) => n.to_string(),
			TokenKind::Shebang(s) => format!("#!{}", s),
			TokenKind::Symbol(s) => s.str().to_string(),
			TokenKind::Whitespace(w) => w.escape_default().to_string(),
		}
	}
}

impl std::fmt::Display for TokenKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Token {
	pub(crate) location: Location,
	pub(crate) kind: TokenKind,
}

impl Token {
	pub fn new(location: Location, kind: TokenKind) -> Self {
		Token { location, kind }
	}

	pub fn is_trivia(&self) -> bool {
		self.kind.is_trivia()
	}

	pub fn is_eof(&self) -> bool {
		self.kind.is_eof()
	}

	pub fn location(&self) -> Location {
		self.location
	}

	pub fn kind(&self) -> &TokenKind {
		&self.kind
	}
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		self.kind == other.kind
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Token({},`{}`)", self.location, self.kind)
	}
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AstToken {
	pub(crate) leadings: Vec<Token>,
	pub(crate) token: Token,
}

impl AstToken {
	pub fn new(leadings: Vec<Token>, token: Token) -> Self {
		Self { leadings, token }
	}

	pub fn kind(&self) -> &TokenKind {
		&self.token.kind
	}

	pub fn location(&self) -> Location {
		self.token.location
	}

	pub fn token(&self) -> &Token {
		&self.token
	}

	pub fn leadings(&self) -> impl Iterator<Item = &Token> {
		self.leadings.iter()
	}
}

impl PartialEq for AstToken {
	fn eq(&self, other: &Self) -> bool {
		self.leadings == other.leadings && self.token == other.token
	}
}

impl std::fmt::Display for AstToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for leading in &self.leadings {
			f.write_str(&leading.to_string())?;
			f.write_str("\n")?;
		}

		f.write_str(&self.token.to_string())?;

		Ok(())
	}
}
