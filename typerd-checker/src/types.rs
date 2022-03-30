pub type TypeId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeLiteral {
	Boolean(bool),
	Number(f64),
	String(String),
}

impl TypeLiteral {
	pub fn description(&self) -> String {
		match self {
			TypeLiteral::Boolean(value) => format!("{}", value),
			TypeLiteral::Number(value) => format!("{}", value),
			TypeLiteral::String(value) => value.to_string(),
		}
	}

	pub fn to_non_literal(&self) -> TypeKind {
		match self {
			TypeLiteral::Boolean(..) => TypeKind::Boolean,
			TypeLiteral::Number(..) => TypeKind::Number,
			TypeLiteral::String(..) => TypeKind::String,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
	Boolean,
	Literal(TypeLiteral),
	Nil,
	Number,
	Reference(TypeId),
	String,
	Tuple(Vec<TypeId>),
	Unknown,
}

impl TypeKind {
	pub fn description(&self) -> Option<String> {
		match self {
			TypeKind::Boolean => Some("bool".into()),
			TypeKind::Literal(tt) => Some(tt.description()),
			TypeKind::Nil => Some("nil".into()),
			TypeKind::Number => Some("number".into()),
			TypeKind::String => Some("string".into()),
			TypeKind::Tuple(_) => None,
			TypeKind::Unknown => Some("unknown".into()),
			_ => None,
		}
	}

	pub fn convert_if_single_tuple(&self) -> TypeKind {
		match self {
			TypeKind::Tuple(members) => {
				if members.is_empty() {
					TypeKind::Nil
				} else if members.len() == 1 {
					TypeKind::Reference(*members.first().unwrap())
				} else {
					self.clone()
				}
			},
			_ => self.clone(),
		}
	}

	pub fn to_non_literal(&self) -> TypeKind {
		match self {
			TypeKind::Literal(l) => l.to_non_literal(),
			_ => self.clone(),
		}
	}

	pub fn get_literal(&self) -> Option<TypeLiteral> {
		if let TypeKind::Literal(l) = self {
			Some(l.clone())
		} else {
			None
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnot {
	pub kind: TypeKind,
	pub id: TypeId,
	pub literal: Option<TypeLiteral>,
	pub name: Option<String>,
}

impl TypeAnnot {
	pub fn to_const_type(&self) -> TypeKind {
		self.literal
			.as_ref()
			.map(|v| TypeKind::Literal(v.clone()))
			.unwrap()
	}
}
