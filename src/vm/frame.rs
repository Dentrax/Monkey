use crate::types::object::{Object, OBJ_NULL, CompiledFunction, Closure};
use crate::code::code::Instructions;

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
	pub cl: Closure,
	pub ip: usize,
	pub bp: usize, //base pointer (a.k.a frame pointer)
}

impl Frame {
	pub fn new(cf: Closure, bp: usize) -> Self {
		Frame {
			cl: cf,
			ip: 0,
			bp
		}
	}

	pub fn instructions(&self) -> &Instructions {
		&self.cl.cf.instructions
	}
}