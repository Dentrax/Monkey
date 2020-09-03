use crate::types::object::{Object, OBJ_NULL, CompiledFunction};
use crate::code::code::Instructions;

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
	pub cf: CompiledFunction,
	pub ip: usize,
}

impl Frame {
	pub fn new(cf: CompiledFunction) -> Self {
		Frame {
			cf,
			ip: 0,
		}
	}

	pub fn instructions(&self) -> &Instructions {
		&self.cf.instructions
	}
}