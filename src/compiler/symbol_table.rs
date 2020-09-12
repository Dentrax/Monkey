use std::collections::HashMap;
use std::mem;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolScope {
	GLOBAL,
	LOCAL,
	BUILTIN,
	FREE
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
	pub name: String,
	pub scope: SymbolScope,
	pub index: usize,
}

impl Default for Symbol {
	fn default() -> Self {
		Symbol {
			name: String::from(""),
			scope: SymbolScope::GLOBAL,
			index: 0
		}
	}
}

impl Symbol {
	pub fn new(name: &str, scope: SymbolScope, index: usize) -> Self {
		Symbol {
			name: String::from(name),
			scope,
			index,
		}
	}
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymbolTable {
	pub outer: Option<Box<SymbolTable>>,
	pub free_symbols: Vec<Symbol>,

	store: HashMap<String, Symbol>,
	pub num_definitions: usize,
}

impl SymbolTable {
	pub fn new() -> Self {
		SymbolTable {
			outer: None,
			free_symbols: vec![],
			store: HashMap::new(),
			num_definitions: 0,
		}
	}

	pub fn new_enclosed(outer: SymbolTable) -> Self {
		SymbolTable {
			outer: Some(Box::new(outer)),
			free_symbols: vec![],
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

	pub fn define_free(&mut self, name: &String, original: &Symbol) -> Symbol {
		self.free_symbols.push(original.clone());

		let symbol = Symbol{
			name: name.clone(),
			scope: SymbolScope::FREE,
			index: self.free_symbols.len() - 1
		};

		self.store.insert(name.to_owned(), symbol.clone()); //TODO: rc

		symbol
	}

	pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
		match self.store.get(name) {
			Some(v) => Some(v.clone()),
			None => {
				match &mut self.outer {
					Some(o) => {
						match o.resolve(name) {
							Some(obj) => {
								match obj.scope {
									SymbolScope::GLOBAL | SymbolScope::BUILTIN => {
										Some(obj)
									}
									_ => {
										let sym = self.define_free(&obj.name, &obj);
										Some(sym)
									}
								}
							},
							None => None
						}
					}
					None => None
				}
			}
		}
	}
}
