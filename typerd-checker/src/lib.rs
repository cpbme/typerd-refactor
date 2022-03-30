#[allow(dead_code, unused)]
mod analyzer;
mod config;
mod diagnostics;
mod module;
mod scope;
mod types;

use id_arena::{Arena, Id};
use typerd_ast as ast;

pub use analyzer::*;
pub use config::*;
pub use diagnostics::*;
pub use module::*;
pub use scope::*;
pub use types::*;

#[derive(Debug)]
pub struct Variable {
	pub name: String,
	pub scope: Id<Scope>,
	pub type_id: TypeId,
}
