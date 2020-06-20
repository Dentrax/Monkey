use crate::types::object::*;

use std::fmt;
use std::fmt::{Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
	pub elements: Vec<Object>,
}

impl fmt::Display for Array {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let elems = self.elements.iter().map(|o| format!("{}", o)).collect::<Vec<String>>();
		write!(f, "[{}]", elems.join(", "))
	}
}