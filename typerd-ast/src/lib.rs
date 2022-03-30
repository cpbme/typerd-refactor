#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

mod ast;
mod tokens;
mod visitors;

pub use ast::*;
pub use tokens::*;
pub use visitors::*;

/// The location of source code in any object such as node
/// or token.
#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Position {
	pub column: usize,
	pub line: usize,
}

impl Position {
	pub fn new(line: usize, column: usize) -> Self {
		Position { column, line }
	}

	/// Creates an empty Position with all entries are set to 0
	#[inline]
	pub fn empty() -> Self {
		Self::new(0, 0)
	}
}

impl Default for Position {
	/// Creates a new default Position with all entries are set to 1
	fn default() -> Self {
		Self { column: 1, line: 1 }
	}
}

impl std::fmt::Display for Position {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.column)
	}
}

/// A position range from start to end.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Location {
	pub start: Position,
	pub end: Position,
}

impl Location {
	pub fn new(start: Position, end: Position) -> Self {
		Location { start, end }
	}

	/// Creates a new empty Location with all entries are called using `Position::empty()`
	pub fn empty() -> Self {
		Location {
			start: Position::empty(),
			end: Position::empty(),
		}
	}

	/// Creates a new Location with the same position entries.
	pub fn same_position(position: Position) -> Self {
		Location {
			start: position,
			end: position,
		}
	}
}

impl std::fmt::Display for Location {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} - {}", self.start, self.end)
	}
}
