#[derive(Debug, Clone, PartialEq)]
pub struct CheckConfig {
	/// Treats any value as a literal type.
	pub value_as_literal_type: bool,
}

impl Default for CheckConfig {
	fn default() -> Self {
		Self {
			value_as_literal_type: true,
		}
	}
}
