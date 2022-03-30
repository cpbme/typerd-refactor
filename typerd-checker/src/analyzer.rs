use std::borrow::Borrow;

use ast::{AstVisitor, Node};

use super::*;

#[derive(Debug)]
pub struct FileAnalyzer {
	pub retinfo: ReturnInfo,
	pub scopes: Arena<Scope>,
	pub stack: Vec<Id<Scope>>,
	pub types: Vec<TypeAnnot>,
	pub variables: Arena<Variable>,
}

impl FileAnalyzer {
	pub fn new(ast: &ast::Block) -> Result<FileAnalyzer, Diagnostic> {
		let mut analyzer = FileAnalyzer {
			retinfo: ReturnInfo {
				type_kind: TypeKind::Nil,
			},
			scopes: Arena::new(),
			stack: Vec::new(),
			types: Vec::new(),
			variables: Arena::new(),
		};
		analyzer.load_std();
		analyzer.visit_block(ast).map(|v| {
			analyzer.retinfo.type_kind = analyzer.get_type(v);
			analyzer
		})
	}

	fn load_std(&mut self) {
		// create a new scope for std
		self.push_scope();
		self.register_type(
			ast::Location::empty(),
			TypeKind::String,
			Some("string".into()),
		);
		self.register_type(
			ast::Location::empty(),
			TypeKind::Number,
			Some("number".into()),
		);
		self.register_type(ast::Location::empty(), TypeKind::Nil, Some("nil".into()));
		self.register_type(
			ast::Location::empty(),
			TypeKind::Boolean,
			Some("bool".into()),
		);
	}

	fn current_scope_id(&self) -> Id<Scope> {
		*self.stack.last().unwrap()
	}

	fn current_scope_static(&self) -> &Scope {
		self.scopes.get(self.current_scope_id()).unwrap()
	}

	fn current_scope(&mut self) -> &mut Scope {
		self.scopes.get_mut(self.current_scope_id()).unwrap()
	}

	fn push_scope(&mut self) {
		let new_scope = Scope {
			analyzer: self,
			types: Vec::new(),
			variables: Vec::new(),
		};
		let scope_id = self.scopes.alloc(new_scope);
		self.stack.push(scope_id);
	}

	fn pop_scope(&mut self) {
		assert!(!self.stack.is_empty(), "Cannot pop the empty scope stack!");
		self.stack.pop();
	}

	pub fn type_description_from_kind(&self, kind: &TypeKind) -> String {
		return match kind.description() {
			Some(des) => des,
			None => match kind {
				TypeKind::Reference(rid) => {
					// get the type and return as variable name if possible
					if let Some(annot) = self.types.get(*rid) {
						if let Some(n) = &annot.name {
							return n.to_string();
						}
					}
					self.type_description(*rid)
				},
				TypeKind::Tuple(types) => {
					let mut result = String::from("(");
					let types_max_id = types.len() - 1;
					for (id, tt) in types.iter().enumerate() {
						result.push_str(&self.type_description(*tt));
						if types_max_id != id {
							result.push(')');
						}
					}
					result
				},
				_ => "unknown".into(),
			},
		};
	}

	pub fn type_description(&self, id: TypeId) -> String {
		if let Some(tt) = self.types.get(id) {
			let kind = &tt.kind;
			self.type_description_from_kind(kind)
		} else {
			"unknown".into()
		}
	}

	fn find_variable(&self, name: &'_ str) -> Option<Id<Variable>> {
		for scope_id in self.stack.iter().rev() {
			let scope = self.scopes.get(*scope_id).unwrap();
			if let Some(var_id) = scope.find_variable(name) {
				return Some(var_id);
			}
		}
		None
	}

	fn find_type_from_name(&self, name: &'_ str) -> Option<usize> {
		for scope_id in self.stack.iter().rev() {
			let scope = self.scopes.get(*scope_id).unwrap();
			if let Some(type_id) = scope.find_type_with_name(name) {
				return Some(type_id);
			}
		}
		None
	}

	fn get_type_downwards(&self, id: usize) -> TypeKind {
		let mut current_type = self.get_type(id);
		loop {
			current_type = match current_type {
				TypeKind::Reference(id) => self.get_type(id),
				_ => break,
			}
		}
		current_type
	}

	fn get_type_from_name(&self, name: &'_ str) -> TypeKind {
		self.find_type_from_name(name)
			.map(|v| self.types.get(v).unwrap().kind.clone())
			.unwrap_or(TypeKind::Unknown)
	}

	fn get_type(&self, id: usize) -> TypeKind {
		self.types.get(id).unwrap().kind.clone()
	}

	fn assert_type_extendable_id(
		&self,
		location: ast::Location,
		left: usize,
		right: usize,
	) -> Result<(), Diagnostic> {
		let left = self.types.get(left).unwrap();
		let right = self.types.get(right).unwrap();
		self.assert_type_extenable(location, &left.kind, &right.kind)
	}

	fn assert_type_extenable(
		&self,
		location: ast::Location,
		left: &TypeKind,
		right: &TypeKind,
	) -> Result<(), Diagnostic> {
		macro_rules! types_not_match {
			($a:expr, $b:expr, $location:ident) => {
				Err(Diagnostic {
					$location,
					kind: DiagnosticKind::TypesNotMatch {
						left: $a,
						right: $b,
					},
				})
			};
		}

		let leftd = self.type_description_from_kind(left);
		let rightd = self.type_description_from_kind(right);

		match (left, right) {
			(TypeKind::Boolean, TypeKind::Boolean) => Ok(()),
			(TypeKind::Boolean, TypeKind::Literal(TypeLiteral::Boolean(_))) => Ok(()),
			(TypeKind::Nil, TypeKind::Nil) => Ok(()),
			(TypeKind::Number, TypeKind::Literal(TypeLiteral::Number(..))) => Ok(()),
			(TypeKind::Number, TypeKind::Number) => Ok(()),
			(TypeKind::String, TypeKind::Literal(TypeLiteral::String(..))) => Ok(()),
			(TypeKind::String, TypeKind::String) => Ok(()),
			(TypeKind::Literal(a), TypeKind::Literal(b)) => {
				if a == b {
					Ok(())
				} else {
					types_not_match!(leftd, rightd, location)
				}
			},
			(TypeKind::Tuple(a), TypeKind::Tuple(b)) => {
				for (id, a_id) in a.iter().enumerate() {
					if let Some(b_id) = b.get(id) {
						self.assert_type_extendable_id(location, *a_id, *b_id)?;
					} else {
						break;
					}
				}
				types_not_match!(leftd, rightd, location)
			},
			_ => {
				types_not_match!(leftd, rightd, location)
			},
		}
	}

	fn register_variable(
		&mut self,
		location: ast::Location,
		type_id: TypeId,
		name: &'_ str,
	) -> Result<Id<Variable>, Diagnostic> {
		// check for variable duplication if it has a name for it
		if self.find_variable(name).is_some() {
			return Err(Diagnostic {
				location,
				kind: DiagnosticKind::DuplicatedVariable {
					name: name.to_string(),
				},
			});
		}

		let var = Variable {
			name: name.to_string(),
			scope: self.current_scope_id(),
			type_id,
		};
		let var_id = self.variables.alloc(var);
		self.current_scope().variables.push(var_id);

		Ok(var_id)
	}

	fn register_type(
		&mut self,
		location: ast::Location,
		kind: TypeKind,
		name: Option<String>,
	) -> Result<TypeId, Diagnostic> {
		// check for type duplication if it has a name for it
		if let Some(ref name) = name {
			if self.find_type_from_name(name).is_some() {
				return Err(Diagnostic {
					location,
					kind: DiagnosticKind::DuplicatedType {
						name: name.to_string(),
					},
				});
			}
		}

		let type_id = self.types.len();
		let annot = TypeAnnot {
			kind: kind.clone(),
			id: self.types.len(),
			literal: kind.get_literal(),
			name,
		};
		self.current_scope().types.push(type_id);
		self.types.push(annot);

		Ok(type_id)
	}

	fn visit_expr_cfg(&mut self, node: &ast::Expr) -> Result<TypeId, Diagnostic> {
		// check for literal types config
		let ty = self.visit_expr(node)?;

		// register a type
		self.register_type(node.location(), ty, None)
	}

	fn exprlist_in_assign(&mut self, node: &ast::ExprList) -> Result<Vec<TypeId>, Diagnostic> {
		let mut types = Vec::new();
		for expr in node.0.iter() {
			// spread out if it is a tuple
			let type_id = self.visit_expr_cfg(&expr.0)?;
			match &self.types.get(type_id).unwrap().kind {
				TypeKind::Tuple(members) => {
					for member in members.iter() {
						types.push(*member);
					}
				},
				_ => types.push(type_id),
			}
		}
		Ok(types)
	}

	fn visit_exprlist(&mut self, node: &ast::ExprList) -> Result<TypeId, Diagnostic> {
		let types = self.exprlist_in_assign(node)?;

		let start = node
			.0
			.first()
			.map(|v| v.0.location().start)
			.unwrap_or_default();

		let end = node
			.0
			.last()
			.map(|v| v.0.location().end)
			.unwrap_or_default();

		let exprtype = if types.is_empty() {
			TypeKind::Nil
		} else if types.len() == 1 {
			TypeKind::Reference(*types.first().unwrap())
		} else {
			TypeKind::Tuple(types)
		};

		self.register_type(ast::Location::new(start, end), exprtype, None)
	}
}

impl ast::ExprVisitor<'_> for FileAnalyzer {
	type ExprResult = Result<TypeKind, Diagnostic>;

	fn visit_binary(&mut self, node: &ast::Binary) -> Self::ExprResult {
		todo!()
	}

	fn visit_bool(&mut self, node: &ast::AstToken) -> Self::ExprResult {
		use ast::{KeywordType, TokenType};
		let value = match node.token_type() {
			TokenType::Keyword(KeywordType::True) => true,
			TokenType::Keyword(KeywordType::False) => false,
			_ => {
				return Err(Diagnostic {
					location: node.location(),
					kind: DiagnosticKind::InternalError("Expected bool".into()),
				})
			},
		};
		Ok(TypeKind::Literal(TypeLiteral::Boolean(value)))
	}

	fn visit_function_expr(&mut self, node: &ast::FunctionExpr) -> Self::ExprResult {
		todo!()
	}

	fn visit_function_call(&mut self, node: &ast::FunctionCall) -> Self::ExprResult {
		todo!()
	}

	fn visit_nil(&mut self, _: &ast::AstToken) -> Self::ExprResult {
		Ok(TypeKind::Nil)
	}

	fn visit_number(&mut self, node: &ast::AstToken) -> Self::ExprResult {
		use ast::TokenType;
		let value = match node.token_type() {
			TokenType::Number(num) => num.parse::<f64>().map_err(|v| Diagnostic {
				kind: DiagnosticKind::InternalError(format!("{}", v)),
				location: node.location(),
			})?,
			_ => {
				return Err(Diagnostic {
					location: node.location(),
					kind: DiagnosticKind::InternalError("Expected number".into()),
				})
			},
		};
		Ok(TypeKind::Literal(TypeLiteral::Number(value)))
	}

	fn visit_paren(&mut self, node: &ast::Paren) -> Self::ExprResult {
		self.visit_expr(node.expr.borrow())
	}

	fn visit_str(&mut self, node: &ast::AstToken) -> Self::ExprResult {
		use ast::TokenType;
		let value = match node.token_type() {
			TokenType::LineStr { value, .. } => value.to_string(),
			TokenType::MultilinedStr { value, .. } => value.to_string(),
			_ => {
				return Err(Diagnostic {
					location: node.location(),
					kind: DiagnosticKind::InternalError("Expected bool".into()),
				})
			},
		};
		Ok(TypeKind::Literal(TypeLiteral::String(value)))
	}

	fn visit_table_ctor(&mut self, node: &ast::TableCtor) -> Self::ExprResult {
		todo!()
	}

	fn visit_type_assertion(&mut self, node: &ast::TypeAssertion) -> Self::ExprResult {
		todo!()
	}

	fn visit_varargs(&mut self, node: &ast::AstToken) -> Self::ExprResult {
		todo!()
	}

	fn visit_unary(&mut self, node: &ast::Unary) -> Self::ExprResult {
		todo!()
	}

	fn visit_access_expr(&mut self, node: &ast::AccessExpr) -> Self::ExprResult {
		todo!()
	}

	fn visit_access_method(&mut self, node: &ast::AccessMethod) -> Self::ExprResult {
		todo!()
	}

	fn visit_access_name(&mut self, node: &ast::AccessName) -> Self::ExprResult {
		todo!()
	}

	fn visit_name(&mut self, node: &ast::Name) -> Self::ExprResult {
		if let Some(varid) = self.find_variable(&node.name) {
			let var = self.variables.get(varid).unwrap();
			Ok(TypeKind::Reference(var.type_id))
		} else {
			Err(Diagnostic {
				kind: DiagnosticKind::UndeclaredVariable {
					name: node.name.to_string(),
				},
				location: node.location(),
			})
		}
	}
}

impl ast::StmtVisitor<'_> for FileAnalyzer {
	type StmtResult = Result<Option<TypeId>, Diagnostic>;

	fn visit_call_stmt(&mut self, node: &ast::FunctionCall) -> Self::StmtResult {
		todo!()
	}

	fn visit_do_stmt(&mut self, node: &ast::DoStmt) -> Self::StmtResult {
		self.visit_block(&node.block).map(Some)
	}

	fn visit_generic_for(&mut self, node: &ast::GenericFor) -> Self::StmtResult {
		todo!()
	}

	fn visit_function_assign(&mut self, node: &ast::FunctionAssign) -> Self::StmtResult {
		todo!()
	}

	fn visit_if(&mut self, node: &ast::IfStmt) -> Self::StmtResult {
		todo!()
	}

	fn visit_local_assign(&mut self, node: &ast::LocalAssignment) -> Self::StmtResult {
		// extract all of the expression types
		let types = if let Some(exprlist) = &node.exprlist {
			self.exprlist_in_assign(exprlist)?
		} else {
			let mut res = Vec::new();
			for _ in 1..node.names.0.len() {
				res.push(self.register_type(ast::Location::empty(), TypeKind::Nil, None)?);
			}
			res
		};

		for (id, name_tuple) in node.names.0.iter().enumerate() {
			let name_part = &name_tuple.0;
			let location = name_part.location();
			let variable = name_part.name.name.to_string();

			let expr_tt = types
				.get(id)
				.copied()
				.unwrap_or({ self.register_type(ast::Location::empty(), TypeKind::Nil, None)? });

			let tt_id = if let Some(tt) = &name_part.type_ref {
				let type_kind = self.visit_type_reference(tt)?;
				self.register_type(ast::Location::empty(), type_kind, None)?
			} else {
				expr_tt
			};

			self.assert_type_extendable_id(location, tt_id, expr_tt)?;
			self.register_variable(location, tt_id, &variable)?;
		}

		Ok(None)
	}

	fn visit_local_function(&mut self, node: &ast::LocalFunction) -> Self::StmtResult {
		todo!()
	}

	fn visit_numeric_for(&mut self, node: &ast::NumericFor) -> Self::StmtResult {
		todo!()
	}

	fn visit_repeat(&mut self, node: &ast::RepeatStmt) -> Self::StmtResult {
		todo!()
	}

	fn visit_type_declaration(&mut self, node: &ast::TypeDeclaration) -> Self::StmtResult {
		todo!()
	}

	fn visit_while(&mut self, node: &ast::WhileStmt) -> Self::StmtResult {
		todo!()
	}

	fn visit_var_assign(&mut self, node: &ast::VarAssign) -> Self::StmtResult {
		todo!()
	}
}

impl ast::TypeVisitor<'_> for FileAnalyzer {
	type TypeResult = Result<TypeKind, Diagnostic>;

	fn visit_array_type(&mut self, node: &ast::ArrayType) -> Self::TypeResult {
		todo!()
	}

	fn visit_conditional_type(&mut self, node: &ast::ConditionalType) -> Self::TypeResult {
		todo!()
	}

	fn visit_generic_type(&mut self, node: &ast::GenericType) -> Self::TypeResult {
		todo!()
	}

	fn visit_intersection_type(&mut self, node: &ast::IntersectionType) -> Self::TypeResult {
		todo!()
	}

	fn visit_parenthesized_type(&mut self, node: &ast::ParenthesizedType) -> Self::TypeResult {
		todo!()
	}

	fn visit_literal_type(&mut self, node: &ast::LiteralType) -> Self::TypeResult {
		todo!()
	}

	fn visit_name_type(&mut self, node: &ast::Name) -> Self::TypeResult {
		Ok(self.get_type_from_name(&node.name))
	}

	fn visit_union_type(&mut self, node: &ast::UnionType) -> Self::TypeResult {
		todo!()
	}
}

impl ast::LastStmtVisitor<'_> for FileAnalyzer {
	type LastStmtResult = Result<Option<TypeId>, Diagnostic>;

	fn visit_return(&mut self, node: &ast::ReturnStmt) -> Self::LastStmtResult {
		Ok(Some(self.visit_exprlist(&node.exprlist)?))
	}

	fn visit_break(&mut self, _: &ast::AstToken) -> Self::LastStmtResult {
		Ok(None)
	}
}

impl ast::AstVisitor<'_> for FileAnalyzer {
	type BlockResult = Result<TypeId, Diagnostic>;
	type StmtsResult = Result<TypeId, Diagnostic>;

	fn visit_block(&mut self, node: &ast::Block) -> Self::BlockResult {
		self.push_scope();
		let res = self.visit_stmts(&node.stmts);
		self.pop_scope();

		res
	}

	fn visit_stmts(&mut self, node: &ast::Stmts) -> Self::StmtsResult {
		for stmt in node.0.iter() {
			self.visit_stmt(&stmt.0)?;
		}
		if let Some(node) = &node.1 {
			Ok(self.visit_last_stmt(node)?.unwrap_or(self.register_type(
				node.location(),
				TypeKind::Nil,
				None,
			)?))
		} else {
			Ok(self.register_type(node.location(), TypeKind::Nil, None)?)
		}
	}
}
