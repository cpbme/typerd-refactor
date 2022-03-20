use crate::prelude::ast::Position;
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Input<'a> {
	chars: Peekable<Chars<'a>>,
	current: char,

	column: usize,
	line: usize,
	offset: usize,
}

impl<'a> Input<'a> {
	pub fn new(input: &'a str) -> Self {
		let mut input = Input {
			chars: input.chars().peekable(),
			current: '\0',

			line: 1,
			column: 0,
			offset: 0,
		};
		input.bump();
		input
	}

	pub fn skip(&mut self, mut skips: usize) {
		debug_assert!(skips > 0, "Attempt to input skips with 0");
		while skips > 0 {
			skips -= 1;
			self.bump();
		}
	}

	pub fn is_first_line(&self) -> bool {
		self.line == 1
	}

	pub fn position(&self) -> Position {
		Position {
			column: self.column,
			line: self.line,
		}
	}

	pub fn current(&self) -> char {
		self.current
	}

	pub fn lookahead(&mut self) -> Option<&char> {
		self.chars.peek()
	}

	pub fn bump(&mut self) -> char {
		let last_char = self.current;
		self.current = self.chars.next().unwrap_or('\0');

		match last_char {
			'\n' => {
				self.line += 1;
				self.column = 1;
			},
			_ => self.column += 1,
		}

		self.offset += 1;
		self.current
	}
}

#[allow(unused_imports)]
mod tests {
	use crate::{prelude::*, Input};
	use ast::Position;

	#[test]
	pub fn current() {
		let mut input = Input::new("Hello");
		assert_eq!(input.current(), 'H');
		input.bump();
		assert_eq!(input.current(), 'e');
		input.bump();
		assert_eq!(input.current(), 'l');
		input.bump();
		assert_eq!(input.current(), 'l');
		input.bump();
		assert_eq!(input.current(), 'o');
		input.bump();
		assert_eq!(input.current(), '\0');
	}

	#[test]
	pub fn position() {
		let mut input = Input::new("H\nello");
		assert_eq!(input.position(), Position::new(1, 1));
		input.bump();
		assert_eq!(input.position(), Position::new(1, 2));
		input.bump();
		assert_eq!(input.position(), Position::new(2, 1));
		input.bump();
		assert_eq!(input.position(), Position::new(2, 2));
		input.bump();
		assert_eq!(input.position(), Position::new(2, 3));
		input.bump();
		assert_eq!(input.position(), Position::new(2, 4));
		input.bump();
		assert_eq!(input.position(), Position::new(2, 5));
		input.bump();
		assert_eq!(input.current(), '\0');
	}

	#[test]
	pub fn bump() {
		let mut input = Input::new("Hello");
		assert_eq!(input.bump(), 'e');
		assert_eq!(input.bump(), 'l');
		assert_eq!(input.bump(), 'l');
		assert_eq!(input.bump(), 'o');
		assert_eq!(input.bump(), '\0');
	}
}
