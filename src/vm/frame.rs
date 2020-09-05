use crate::types::object::{Object, OBJ_NULL, CompiledFunction};
use crate::code::code::Instructions;

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
	pub cf: CompiledFunction,
	pub ip: usize,
	pub bp: usize, //base pointer (a.k.a frame pointer)
}

impl Frame {
	pub fn new(cf: CompiledFunction, bp: usize) -> Self {
		Frame {
			cf,
			ip: 0,
			bp
		}
	}

	pub fn instructions(&self) -> &Instructions {
		&self.cf.instructions
	}
}