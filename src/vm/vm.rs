use crate::types::object::{Object, OBJ_NULL, OBJ_TRUE, OBJ_FALSE};
use crate::code::code::{Instructions, OpCodeType, read_uint16};
use crate::compiler::compiler::Bytecode;
use std::borrow::Borrow;

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
			let op_code = self.instructions[ip];
			let op = OpCodeType::from(op_code);

			match op {
				OpCodeType::CONSTANT => {
					let const_index = read_uint16(&self.instructions[ip + 1..]);
					ip += 2;

					self.push(self.constants[const_index].clone()); //FIXME: do not clone?
				},
				OpCodeType::POP => {
					self.pop();
				},
				OpCodeType::ADD | OpCodeType::SUB | OpCodeType::MUL | OpCodeType::DIV => {
					self.execute_binary_operation(op);
				},
				OpCodeType::TRUE => {
					self.push(OBJ_TRUE);
				},
				OpCodeType::FALSE => {
					self.push(OBJ_FALSE);
				},
				OpCodeType::EQ | OpCodeType::NEQ | OpCodeType::GT => {
					self.execute_comparison(op);
				},
				OpCodeType::BANG => {
					self.execute_operator_bang();
				},
				OpCodeType::MINUS => {
					self.execute_operator_minus();
				},
				OpCodeType::JMP => {
					let pos = read_uint16(&self.instructions[ip + 1..]);
					ip = pos - 1;
				},
				OpCodeType::JMPNT => {
					let pos = read_uint16(&self.instructions[ip + 1..]);
					ip += 2; //skip over the two bytes of the operand in the next cycle

					let condition = self.pop();

					if !condition.is_truthy() {
						ip = pos - 1;
					}
				},
				OpCodeType::NULL => {
					self.push(OBJ_NULL);
				},
				_ => panic!("unexpected OpCodeType: {:?}", op)
			}

			ip += 1;
		};
	}

	fn execute_binary_operation(&mut self, op: OpCodeType) { //TODO: return err
		let right = self.pop().to_owned(); //FIXME: cloning
		let left = self.pop().to_owned();

		match (right.borrow(), left.borrow()) {
			(Object::INTEGER(r), Object::INTEGER(l)) => {
				self.execute_binary_operation_integer(op, *l, *r);
			}
			_ => panic!("wrong object types for OpCodeType::ADD. R: {}, L: {}", right, left)
		}
	}

	fn execute_binary_operation_integer(&mut self, op: OpCodeType, left: isize, right: isize) {
		let result = match op {
			OpCodeType::ADD => left + right,
			OpCodeType::SUB => left - right,
			OpCodeType::MUL => left * right,
			OpCodeType::DIV => left / right,
			_ => panic!("unknown integer operator: {:?}", op)
		};

		self.push(Object::INTEGER(result));
	}

	fn execute_comparison(&mut self, op: OpCodeType) { //TODO: return err
		let right = self.pop().to_owned(); //FIXME: cloning
		let left = self.pop().to_owned();

		match (right.borrow(), left.borrow()) {
			(Object::INTEGER(r), Object::INTEGER(l)) => {
				self.execute_comparison_integer(op, *l, *r);
			},
			(Object::BOOLEAN(r), Object::BOOLEAN(l)) => {
				self.execute_comparison_boolean(op, *l, *r);
			},
			_ => panic!("wrong object types for OpCodeType::ADD. R: {}, L: {}", right, left)
		}
	}

	fn execute_comparison_integer(&mut self, op: OpCodeType, left: isize, right: isize) {
		let result = match op {
			OpCodeType::EQ => left == right,
			OpCodeType::NEQ => left != right,
			OpCodeType::GT => left > right,
			_ => panic!("unknown integer comparison operator: {:?}", op)
		};

		self.push(Object::BOOLEAN(result));
	}

	fn execute_comparison_boolean(&mut self, op: OpCodeType, left: bool, right: bool) {
		let result = match op {
			OpCodeType::EQ => left == right,
			OpCodeType::NEQ => left != right,
			_ => panic!("unknown boolean comparison operator: {:?}", op)
		};

		self.push(Object::BOOLEAN(result));
	}

	fn execute_operator_bang(&mut self) {
		let operand = self.pop().to_owned();

		let result = match operand {
			Object::BOOLEAN(b) => {
				if b {
					self.push(OBJ_FALSE)
				} else {
					self.push(OBJ_TRUE)
				}
			},
			Object::NULL => {
				self.push(OBJ_TRUE)
			}
			_ => self.push(OBJ_FALSE)
		};
	}

	fn execute_operator_minus(&mut self) {
		let operand = self.pop().to_owned();

		let result = match operand {
			Object::INTEGER(i) => {
				self.push(Object::INTEGER(-i))
			},
			_ => panic!("unsupported type for negation: {:?}", operand)
		};
	}

	fn push(&mut self, o: Object) {
		if self.sp >= STACK_SIZE {
			panic!("stack overflow")
		}

		self.stack[self.sp] = o;
		self.sp += 1;
	}

	fn pop(&mut self) -> &Object {
		let o = &self.stack[self.sp - 1];
		self.sp -= 1;
		o
	}

	pub fn last_popped_stack_elem(&self) -> Option<&Object> {
		self.stack.get(self.sp)
	}
}