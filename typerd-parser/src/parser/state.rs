use crate::prelude::ast::AstToken;

pub struct ParseState<'a> {
	offset: usize,
	tokens: &'a [AstToken],
}

impl<'a> ParseState<'a> {
	pub fn new(tokens: &'a [AstToken]) -> Self {
		Self { offset: 0, tokens }
	}

	pub fn peek(&self) -> Option<&'a AstToken> {
		self.tokens.get(self.offset)
	}

	pub fn get_or_last_token(&self) -> &'a AstToken {
		self.peek().unwrap_or_else(|| self.tokens.last().unwrap())
	}

	pub fn advance(&self, amount: usize) -> ParseState<'a> {
		ParseState {
			offset: self.offset + amount,
			tokens: self.tokens,
		}
	}
}

impl std::fmt::Debug for ParseState<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"ParseState {{ offset: {}, current: {:?} }}",
			self.offset,
			self.peek()
		)
	}
}
