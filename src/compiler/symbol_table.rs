use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolScope {
	GLOBAL,
	LOCAL,
	BUILTIN
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymbolTable {
	pub outer: Option<Box<SymbolTable>>,

	store: HashMap<String, Symbol>,
	pub num_definitions: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		SymbolTable {
			outer: None,
			store: HashMap::new(),
			num_definitions: 0,
		}
	}

	pub fn new_enclosed(outer: SymbolTable) -> Self {
		SymbolTable {
			outer: Some(Box::new(outer)),
			store: HashMap::new(),
			num_definitions: 0
		}
	}

	pub fn define(&mut self, name: &str) -> Symbol {
		let scope = match &self.outer {
			Some(s) => SymbolScope::LOCAL,
			None => SymbolScope::GLOBAL,
		};

		let symbol = Symbol {
			name: name.to_string(),
			scope,
			index: self.num_definitions,
		};

		self.store.insert(name.to_string(), symbol.clone()); //FIXME: clone?
		self.num_definitions += 1;

		symbol
	}

	pub fn define_builtin(&mut self, index: usize, name: String) -> Symbol {
		let symbol = Symbol{
			name: name.clone(),
			scope: SymbolScope::BUILTIN,
			index
		};

		self.store.insert(name, symbol.clone()); //TODO: rc

		symbol
	}

	pub fn resolve(&self, name: &str) -> Option<&Symbol> {
		match self.store.get(name) {
			Some(v) => Some(v),
			None => {
				match &self.outer {
					Some(o) => {
						return o.resolve(name)
					}
					None => None
				}
			}
		}
	}
}
