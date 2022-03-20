mod input;
mod prelude;

pub use input::*;
pub mod parser;
pub mod tokenizer;

use prelude::*;

#[derive(Debug)]
pub enum Error {
	ParseError(parser::ParseError),
	TokenizeError(tokenizer::TokenizeError),
}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::ParseError(err) => write!(f, "{}", err),
			Error::TokenizeError(err) => write!(f, "{}", err),
		}
	}
}

impl std::error::Error for Error {
	fn description(&self) -> &str {
		"parse error"
	}
}

pub fn tokenize(input: &'_ str) -> Result<Vec<ast::AstToken>, tokenizer::TokenizeError> {
	tokenizer::Tokenizer::new(Input::new(input)).tokenize()
}

pub fn parse(input: &'_ str) -> Result<ast::Block, Error> {
	let tokens = tokenize(input).map_err(Error::TokenizeError)?;
	parser::parse_from_tokens(&tokens).map_err(Error::ParseError)
}
