use super::*;

macro_rules! visitor {
	{
		name = $name:ident,
		result = $result_name:ident,
		$(
			$node_name:ident => $ast_ty:ty,
		)*
	} => {
		pub trait $name<'a> {
			type $result_name: 'a;

			$(
				fn $node_name(&mut self, node: &$ast_ty) -> Self::$result_name;
			)*
		}
	};
}

visitor! {
	name = ExprVisitor,
	result = ExprResult,

	visit_binary => Binary,
	visit_bool => AstToken,
	visit_function_expr => FunctionExpr,
	visit_function_call => FunctionCall,
	visit_nil => AstToken,
	visit_number => AstToken,
	visit_paren => Paren,
	visit_str => AstToken,
	visit_table_ctor => TableCtor,
	visit_type_assertion => TypeAssertion,
	visit_varargs => AstToken,
	visit_unary => Unary,

	visit_access_expr => AccessExpr,
	visit_access_method => AccessMethod,
	visit_access_name => AccessName,
	visit_name => Name,
}

visitor! {
	name = StmtVisitor,
	result = StmtResult,

	visit_call_stmt => FunctionCall,
	visit_do_stmt => DoStmt,
	visit_generic_for => GenericFor,
	visit_function_assign => FunctionAssign,
	visit_if => IfStmt,
	visit_local_assign => LocalAssignment,
	visit_local_function => LocalFunction,
	visit_numeric_for => NumericFor,
	visit_repeat => RepeatStmt,
	visit_type_declaration => TypeDeclaration,
	visit_while => WhileStmt,
	visit_var_assign => VarAssign,
}

visitor! {
	name = LastStmtVisitor,
	result = LastStmtResult,

	visit_return => ReturnStmt,
	visit_break => AstToken,
}

visitor! {
	name = TypeVisitor,
	result = TypeResult,

	visit_array_type => ArrayType,
	visit_conditional_type => ConditionalType,
	visit_generic_type => GenericType,
	visit_intersection_type => IntersectionType,
	visit_parenthesized_type => ParenthesizedType,
	visit_literal_type => LiteralType,
	visit_name_type => Name,
	visit_union_type => UnionType,
}

pub trait AstVisitor<'a>:
	ExprVisitor<'a> + StmtVisitor<'a> + LastStmtVisitor<'a> + TypeVisitor<'a>
{
	type BlockResult: 'a;
	type StmtsResult: 'a;

	fn visit_type_reference(&mut self, node: &TypeReference) -> Self::TypeResult {
		match node {
			TypeReference::Array(node) => self.visit_array_type(node),
			TypeReference::Conditional(node) => self.visit_conditional_type(node),
			TypeReference::Generic(node) => self.visit_generic_type(node),
			TypeReference::Intersection(node) => self.visit_intersection_type(node),
			TypeReference::Parenthesized(node) => self.visit_parenthesized_type(node),
			TypeReference::Literal(node) => self.visit_literal_type(node),
			TypeReference::Name(node) => self.visit_name_type(node),
			TypeReference::Union(node) => self.visit_union_type(node),
		}
	}

	fn visit_last_stmt(&mut self, node: &LastStmt) -> Self::LastStmtResult {
		match node {
			LastStmt::Break(node) => self.visit_break(&node.0),
			LastStmt::Return(node) => self.visit_return(&node.0),
		}
	}

	fn visit_block(&mut self, node: &Block) -> Self::BlockResult;
	fn visit_stmts(&mut self, node: &Stmts) -> Self::StmtsResult;

	fn visit_var_expr(&mut self, node: &VarExpr) -> Self::ExprResult {
		match node {
			VarExpr::AccessExpr(node) => self.visit_access_expr(node),
			VarExpr::AccessName(node) => self.visit_access_name(node),
			VarExpr::AccessMethod(node) => self.visit_access_method(node),
			VarExpr::Name(node) => self.visit_name(node),
		}
	}

	fn visit_prefix_expr(&mut self, node: &PrefixExpr) -> Self::ExprResult {
		match node {
			PrefixExpr::FunctionCall(node) => self.visit_function_call(node),
			PrefixExpr::Paren(node) => self.visit_paren(node),
			PrefixExpr::Var(node) => self.visit_var_expr(node),
		}
	}

	fn visit_value(&mut self, node: &Value) -> Self::ExprResult {
		match node {
			Value::Bool(node) => self.visit_bool(node),
			Value::Function(node) => self.visit_function_expr(node),
			Value::Nil(node) => self.visit_nil(node),
			Value::Number(node) => self.visit_number(node),
			Value::Prefix(node) => self.visit_prefix_expr(node),
			Value::Str(node) => self.visit_str(node),
			Value::Table(node) => self.visit_table_ctor(node),
			Value::Varargs(node) => self.visit_varargs(node),
		}
	}

	fn visit_expr(&mut self, node: &Expr) -> Self::ExprResult {
		match node {
			Expr::Binary(node) => self.visit_binary(node),
			Expr::Unary(node) => self.visit_unary(node),
			Expr::TypeAssertion(node) => self.visit_type_assertion(node),
			Expr::Value(node) => self.visit_value(node),
		}
	}

	fn visit_stmt(&mut self, node: &Stmt) -> Self::StmtResult {
		match node {
			Stmt::Call(node) => self.visit_call_stmt(node),
			Stmt::Do(node) => self.visit_do_stmt(node),
			Stmt::GenericFor(node) => self.visit_generic_for(node),
			Stmt::FunctionAssign(node) => self.visit_function_assign(node),
			Stmt::If(node) => self.visit_if(node),
			Stmt::LocalAssignment(node) => self.visit_local_assign(node),
			Stmt::LocalFunction(node) => self.visit_local_function(node),
			Stmt::NumericFor(node) => self.visit_numeric_for(node),
			Stmt::Repeat(node) => self.visit_repeat(node),
			Stmt::TypeDeclaration(node) => self.visit_type_declaration(node),
			Stmt::While(node) => self.visit_while(node),
			Stmt::VarAssign(node) => self.visit_var_assign(node),
		}
	}
}
