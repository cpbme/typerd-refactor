#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

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

	#[inline]
	pub fn empty() -> Self {
		Self::new(0, 0)
	}

	pub fn error_str(&self) -> String {
		format!("{}:{}", self.line, self.column)
	}
}

impl Default for Position {
	fn default() -> Self {
		Self { column: 1, line: 1 }
	}
}

impl std::fmt::Display for Position {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.column)
	}
}

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

	pub fn empty() -> Self {
		Location {
			start: Position::empty(),
			end: Position::empty(),
		}
	}

	pub fn same_position(position: Position) -> Self {
		Location {
			start: position,
			end: position,
		}
	}

	pub fn error_str(&self) -> String {
		self.start.error_str()
	}
}

impl std::fmt::Display for Location {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} - {}", self.start.error_str(), self.end.error_str())
	}
}
