use super::*;

#[derive(Debug)]
pub struct ImportInfo {}

#[derive(Debug)]
pub struct ReturnInfo {
	pub type_kind: TypeKind,
}

#[derive(Debug)]
pub struct ModuleInfo<'a> {
	pub ast: &'a ast::Block,
	pub analyzer: FileAnalyzer,
	pub imports: Vec<ImportInfo>,
	pub returns: ReturnInfo,
}

impl<'a> ModuleInfo<'a> {
	pub fn new(ast: &'a ast::Block) -> Result<Self, Diagnostic> {
		Ok(ModuleInfo {
			ast,
			analyzer: FileAnalyzer::new(ast)?,
			imports: Vec::new(),
			returns: ReturnInfo {
				type_kind: TypeKind::Unknown,
			},
		})
	}
}
