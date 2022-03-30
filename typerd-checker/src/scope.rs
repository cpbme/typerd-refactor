use super::*;

#[derive(Debug)]
pub struct Scope {
	pub analyzer: *mut FileAnalyzer,
	pub types: Vec<TypeId>,
	pub variables: Vec<Id<Variable>>,
}

// these methods are messy. >:(
impl Scope {
	#[rustfmt::skip]
	pub fn find_type(&self, id: TypeId) -> Option<TypeId> {
		for typeid in self.types.iter().rev() { unsafe {
			if let Some(tt) = (*self.analyzer).types.get(*typeid) {
				if tt.id == id { return Some(*typeid); }
			}
		} }
		None
	}

	#[rustfmt::skip]
	pub fn find_type_with_name(&self, name: &'_ str) -> Option<TypeId> {
		for typeid in self.types.iter().rev() { unsafe {
			if let Some(tt) = (*self.analyzer).types.get(*typeid) {
				if tt.name == Some(name.to_string()) { return Some(*typeid); }
			}
		} }
		None
	}

	#[rustfmt::skip]
	pub fn find_variable(&self, name: &'_ str) -> Option<Id<Variable>> {
		for varid in self.variables.iter().rev() { unsafe {
			if let Some(variable) = (*self.analyzer).variables.get(*varid) {
				if variable.name == name { return Some(*varid); }
			}
		} }
		None
	}
}
