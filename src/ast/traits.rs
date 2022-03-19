use crate::Location;

pub trait Node {
	fn location(&self) -> Location;
}
