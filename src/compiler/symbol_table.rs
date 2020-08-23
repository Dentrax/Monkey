use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolScope {
	GLOBAL
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymbolTable {
	store: HashMap<String, Symbol>,
	num_definitions: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		SymbolTable {
			store: HashMap::new(),
			num_definitions: 0,
		}
	}

	pub fn define(&mut self, name: &str) -> Symbol {
		let symbol = Symbol {
			name: name.to_string(),
			scope: SymbolScope::GLOBAL,
			index: self.num_definitions,
		};

		self.store.insert(name.to_string(), symbol.clone()); //FIXME: clone?
		self.num_definitions += 1;

		symbol
	}

	pub fn resolve(&self, name: &str) -> Option<&Symbol> {
		match self.store.get(name) {
			Some(v) => Some(v),
			None => None
		}
	}
}
