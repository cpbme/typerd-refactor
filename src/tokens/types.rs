#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

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
			pub fn str(&self) -> &'static str {
				match self {
					$(
						$enum_name::$name => $value,
					)*
				}
			}

			pub fn parse(input: &'_ str) -> Option<$enum_name> {
				match input {
					$(
						$value => Some($enum_name::$name),
					)*
					_ => None,
				}
			}

			pub fn entries() -> Vec<$enum_name> {
				vec![$($enum_name::$name,)*]
			}

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
	#[doc = "All of the keywords in typerd language. Few keywords are added such as `class` and `enum`"]
	KeywordKind,
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
	}
);

enum_list_ctor!(
	#[doc = "All of the symbols in typerd language. Most of them are from Lua."]
	SymbolKind,
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
