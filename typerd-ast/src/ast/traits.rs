use crate::Location;

/// A node trait.
pub trait Node {
	fn location(&self) -> Location;
}
