#[cfg(target_feature = "serde")]
use serde::{Deserialize, Serialize};

use super::*;
use crate::Position;

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum VarExpr {
	AccessExpr(AccessExpr),
	AccessName(AccessName),
	AccessMethod(AccessMethod),
	Name(Name),
}

impl Node for VarExpr {
	fn location(&self) -> Location {
		match self {
			VarExpr::AccessExpr(node) => node.location(),
			VarExpr::AccessName(node) => node.location(),
			VarExpr::AccessMethod(node) => node.location(),
			VarExpr::Name(node) => node.location(),
		}
	}
}

/// ` <name> ':' <valuetype> `
#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignName {
	pub name: Name,
	pub colon: Option<AstToken>,
	pub type_ref: Option<TypeReference>,
}

impl Node for LocalAssignName {
    fn location(&self) -> Location {
        Location::new(
			self.name.location().start,
			self.type_ref.as_ref()
				.map(|v| v.location().end)
				.unwrap_or(self.name.location().end)
		)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignNameList(pub Vec<(LocalAssignName, Option<AstToken>)>);

impl Node for LocalAssignNameList {
	fn location(&self) -> Location {
		let start = self.0.first().map(|v| v.0.location().start);
		let end = self.0.last().map(|v| v.0.location().end);
		Location::new(
			start.unwrap_or_else(Position::empty),
			end.unwrap_or_else(Position::empty),
		)
	}
}
