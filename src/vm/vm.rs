use crate::types::object::{Object, OBJ_NULL};
use crate::code::code::{Instructions, OpCodeType, read_uint16};
use crate::compiler::compiler::Bytecode;

const STACK_SIZE: usize = 2048;

pub struct VM<'a> {
	constants: &'a Vec<Object>,
	instructions: &'a Instructions,

	stack: Vec<Object>,
	sp: usize
}

impl<'a> VM<'a> {
	pub fn new(bytecode: &'a Bytecode) -> VM<'a> {
		let mut stack = Vec::with_capacity(STACK_SIZE);
		stack.resize(STACK_SIZE, OBJ_NULL);

		VM {
			constants: bytecode.constants,
			instructions: bytecode.instructions,

			stack, //TODO: resize with null obj
			sp: 0
		}
	}

	pub fn stack_top(&self) -> Option<&Object> {
		if self.sp == 0 {
			return None
		}

		self.stack.get(self.sp - 1)
	}

	pub fn run(&mut self) {
		let mut ip = 0;

		while ip < self.instructions.len() {
			let op = self.instructions[ip];

			match OpCodeType::from(op) {
				OpCodeType::CONSTANT => {
					let const_index = read_uint16(&self.instructions[ip + 1..]);
					ip += 2;

					self.push(self.constants[const_index].clone()); //FIXME: do not clone?
				}
			}

			ip += 1;
		};
	}

	fn push(&mut self, o: Object) {
		if self.sp >= STACK_SIZE {
			panic!("stack overflow")
		}

		self.stack[self.sp] = o;
		self.sp += 1;
	}
}