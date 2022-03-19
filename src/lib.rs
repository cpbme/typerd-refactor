mod input;
pub use input::*;

mod position;
pub use position::*;

mod tokenizer;
pub use tokenizer::*;

mod tokens;
pub use tokens::*;

pub mod ast;

mod parser;
pub use parser::*;

#[derive(Debug)]
pub enum Error {
	ParseError(ParseError),
	TokenizeError(TokenizeError),
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

pub fn tokenize(input: &'_ str) -> Result<Vec<AstToken>, TokenizeError> {
	Tokenizer::new(Input::new(input)).tokenize()
}

pub fn parse(input: &'_ str) -> Result<ast::Block, Error> {
	let tokens = tokenize(input).map_err(Error::TokenizeError)?;
	parse_from_tokens(&tokens).map_err(Error::ParseError)
}
