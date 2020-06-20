use crate::types::object::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
	store: HashMap<String, Object>,
	outer: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
	fn default() -> Environment {
		Environment { store: HashMap::new(), outer: None }
	}
}

impl Environment {
	pub fn new() -> Self {
		return Environment::default();
	}

	pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
		let mut env = Environment::default();
		env.outer = Some(outer);
		env
	}

	pub fn get(&self, key: &str) -> Option<Object> {
		match self.store.get(key) {
			Some(obj) => {
				Some(obj.clone())
			}
			None => match self.outer {
				Some(ref outer) => outer.borrow_mut().get(key),
				None => None
			},
		}
	}

	pub fn set(&mut self, key: String, obj: &Object) {
		self.store.insert(key, obj.clone());
	}
}