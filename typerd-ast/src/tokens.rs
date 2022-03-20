#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Location;

macro_rules! enum_list_ctor {
	(
		$(
			#[$enum_meta:meta]
		)*
		$enum_name:ident,
		{
		$(
			$name:ident => $value:expr,
		)*
		}
	) => {
		$(#[$enum_meta])*
		#[derive(Copy, Clone, Debug, PartialEq, Eq)]
		#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
		pub enum $enum_name {
			$(
				$name,
			)*
		}

		impl $enum_name {
			/// Converts an object to a static string.
			pub fn str(&self) -> &'static str {
				match self {
					$(
						$enum_name::$name => $value,
					)*
				}
			}

			/// Parses the string argument and converts it to te desired enum.
			pub fn parse(input: &'_ str) -> Option<$enum_name> {
				match input {
					$(
						$value => Some($enum_name::$name),
					)*
					_ => None,
				}
			}

			/// Gets all of the entries of the enum.
			pub fn entries() -> Vec<$enum_name> {
				vec![$($enum_name::$name,)*]
			}

			/// Gets all of the string entries of the enum.
			pub fn str_entries() -> Vec<&'static str> {
				vec![$($value,)*]
			}
		}

		impl std::fmt::Display for $enum_name {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				write!(f, "{}", self.str())
			}
		}
	};
}

enum_list_ctor!(
	#[doc =
"
All of the keywords found in the typerd language.
Few keywords are added such as `class`, `type` and `enum`
	"]
	KeywordType,
	{
		And => "and",
		Break => "break",
		Do => "do",
		Else => "else",
		ElseIf => "elseif",
		End => "end",
		False => "false",
		For => "for",
		Function => "function",
		If => "if",
		In => "in",
		Local => "local",
		Nil => "nil",
		Not => "not",
		Or => "or",
		Repeat => "repeat",
		Return => "return",
		Then => "then",
		True => "true",
		Until => "until",
		While => "while",

		As => "as",
		Class => "class",
		Enum => "enum",
		Type => "type",
	}
);

enum_list_ctor!(
	#[doc = "All of the symbols in typerd language. Most of them are from Lua."]
	SymbolType,
	{
		NotEqual => "~=",
		DoubleEqual => "==",
		Equal => "=",

		At => "@",
		Hash => "#",

		Dollar => "$",

		Semicolon => ";",
		Colon => ":",

		OpenCurly => "{",
		CloseCurly => "}",

		OpenBracket => "[",
		CloseBracket => "]",

		Comma => ",",

		OpenParen => "(",
		CloseParen => ")",

		GreaterEqual => ">=",
		GreaterThan => ">",

		LessEqual => "<=",
		LessThan => "<",

		CrossEqual => "+=",
		Cross => "+",

		DashEqual => "-=",
		Dash => "-",

		AsteriskEqual => "*=",
		Asterisk => "*",

		CaretEqual => "^=",
		Caret => "^",

		SlashEqual => "/=",
		DoubleSlash => "//",
		Slash => "/",

		Percent => "%",

		ConcatEqual => "..=",
		TripleDots => "...",
		DoubleDots => "..",
		Dot => ".",

		DoubleQuestionMarks => "??",
		QuestionMark => "?",

		Ampersand => "&",
		VerticalBar => "|",
		DoubleColon => "::",
		SkinnyArrow => "->",
	}
);

/// A string quote type.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum QuoteKind {
	Single,
	Double,
}

pub enum TokenKind {
	Eof,
	Comment,
	Keyword,
	Name,
	Number,
	Shebang,
	String,
	Symbol,
	Whitespace,
}

/// A different kinds of token can be tokenized in Typerd.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenType {
	/// End of file, must have it at the end of the text
	Eof,

	/// Identifier like kind but it is targetted a specific words.
	Keyword(KeywordType),

	/// It ignores when parsing all of the possible tokens. It usually starts in `--`
	LineComment(String),

	/// A single line string. Any letters and escaped characters are allowed, expect for its quote.
	LineStr {
		/// Quote kind that Lua can support. These are: `'` and `"`
		quote_kind: QuoteKind,

		/// A value of the string
		value: String,
	},

	/// Simple principle as a single lined comments but it is multlined.
	MultilinedComment {
		/// How many equals to terminate the comment.
		equals: usize,

		/// The contents of the comment
		value: String,
	},

	/// A multiple lined string. Same principle as string but it starts like
	/// a multiline comment (excepting `---`).
	MultilinedStr {
		/// Total amount of equals to close the string
		equals: usize,

		/// A value of the string
		value: String,
	},

	/// A valid Lua identifier or name.
	///
	/// Regex: `[a-zA-Z_][a-zA-Z0-9_]`
	Name(String),

	/// A valid Lua number
	Number(String),

	/// A Linux shebang line to identify which programs will be used.
	/// Usually starts in the first line of the text.
	Shebang(String),
	Symbol(SymbolType),

	/// A useless character does not needed it, but useful for formatters.
	Whitespace(String),
}

impl TokenType {
	/// Checks if the token kind is an end of line.
	pub fn is_eof(&self) -> bool {
		matches!(self, TokenType::Eof)
	}

	/// Checks if the token kind is a trivia. (Not neccessary tokens)
	pub fn is_trivia(&self) -> bool {
		matches!(
			self,
			TokenType::LineComment(..)
				| TokenType::MultilinedComment { .. }
				| TokenType::Shebang(..)
				| TokenType::Whitespace(..)
		)
	}

	pub fn value(&self) -> String {
		match self {
			TokenType::Eof => "<eof>".into(),
			TokenType::Keyword(k) => k.str().to_string(),
			TokenType::LineComment(contents) => format!("--{}", contents),
			TokenType::LineStr { quote_kind, value } => {
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
			TokenType::MultilinedComment { equals, value } => {
				format!("--[{1}[{0}]{1}]", value, ".".repeat(*equals))
			},
			TokenType::MultilinedStr { equals, value } => {
				format!("[{1}[{0}]{1}]", value, ".".repeat(*equals))
			},
			TokenType::Name(n) => n.to_string(),
			TokenType::Number(n) => n.to_string(),
			TokenType::Shebang(s) => format!("#!{}", s),
			TokenType::Symbol(s) => s.str().to_string(),
			TokenType::Whitespace(w) => w.escape_default().to_string(),
		}
	}
}

impl std::fmt::Display for TokenType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Token {
	pub(crate) location: Location,
	pub(crate) token_type: TokenType,
}

impl Token {
	/// It creates a new token
	pub fn new(location: Location, token_type: TokenType) -> Self {
		Token {
			location,
			token_type,
		}
	}

	/// Gets the location of the token
	pub fn location(&self) -> Location {
		self.location
	}

	/// Gets the token's kind based on the `token_type` property
	pub fn token_kind(&self) -> TokenKind {
		match &self.token_type {
			TokenType::Eof => TokenKind::Eof,
			TokenType::Keyword(_) => TokenKind::Keyword,
			TokenType::LineComment(_) => TokenKind::Comment,
			TokenType::LineStr { .. } => TokenKind::String,
			TokenType::MultilinedComment { .. } => TokenKind::Comment,
			TokenType::MultilinedStr { .. } => TokenKind::String,
			TokenType::Name(_) => TokenKind::Name,
			TokenType::Number(_) => TokenKind::Number,
			TokenType::Shebang(_) => TokenKind::Shebang,
			TokenType::Symbol(_) => TokenKind::Symbol,
			TokenType::Whitespace(_) => TokenKind::Whitespace,
		}
	}

	/// Gets the current token type
	pub fn token_type(&self) -> &TokenType {
		&self.token_type
	}

	/// Checks if the token kind is an end of line.
	pub fn is_eof(&self) -> bool {
		self.token_type.is_eof()
	}

	/// Checks if the token kind is a trivia. (Not neccessary tokens)
	pub fn is_trivia(&self) -> bool {
		self.token_type.is_trivia()
	}
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		self.token_type == other.token_type
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.token_type)
	}
}

/// A finalized Token object but it has leadings on it.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AstToken {
	pub(crate) leadings: Vec<Token>,
	pub(crate) token: Token,
}

impl AstToken {
	/// Creates a new AstToken object
	pub fn new(leadings: Vec<Token>, token: Token) -> Self {
		AstToken { leadings, token }
	}

	/// Gets the token kind based on the token type
	pub fn kind(&self) -> TokenKind {
		self.token.token_kind()
	}

	/// Gets the token type
	pub fn token_type(&self) -> &TokenType {
		&self.token.token_type
	}

	/// Gets the location of the token
	pub fn location(&self) -> Location {
		self.token.location
	}

	/// Gets the original token from the object
	pub fn token(&self) -> &Token {
		&self.token
	}

	/// Gets the entire leadings of the object
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

		f.write_str(&self.token.to_string())
	}
}
