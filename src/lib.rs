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

pub fn tokenize(input: &'_ str) -> Result<Vec<AstToken>, TokenizeError> {
	Tokenizer::new(Input::new(input)).tokenize()
}
