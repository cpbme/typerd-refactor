use typerd_ast::Location;

#[derive(Debug)]
pub enum DiagnosticKind {
	DuplicatedType { name: String },
	DuplicatedVariable { name: String },
	InternalError(String),
	UndeclaredVariable { name: String },
	TypesNotMatch { left: String, right: String },
}

impl std::fmt::Display for DiagnosticKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DiagnosticKind::DuplicatedType { name } => format!("Duplicated type `{}`", name),
			DiagnosticKind::DuplicatedVariable { name } => {
				format!("Duplicated variable `{}`", name)
			},
			DiagnosticKind::InternalError(err) => format!("Internal typechecking error: {}", err),
			DiagnosticKind::UndeclaredVariable { name } => {
				format!("Undeclared variable `{}`", name)
			},
			DiagnosticKind::TypesNotMatch { left, right } => {
				format!("{} does not match with {}", left, right)
			},
		}
		.fmt(f)
	}
}

#[derive(Debug)]
pub struct Diagnostic {
	pub kind: DiagnosticKind,
	pub location: Location,
}

impl std::fmt::Display for Diagnostic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}: {}", self.location.start, self.kind)
	}
}

impl std::error::Error for Diagnostic {}
