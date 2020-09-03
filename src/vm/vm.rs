use crate::types::object::{Object, OBJ_NULL, OBJ_TRUE, OBJ_FALSE, CompiledFunction};
use crate::code::code::{Instructions, OpCodeType, read_uint16};
use crate::compiler::compiler::Bytecode;
use std::borrow::Borrow;
use crate::types::array::Array;
use std::collections::HashMap;
use crate::types::hashable::{Hash, Hashable};
use crate::vm::frame::Frame;

const STACK_SIZE: usize = 2048;
const GLOBAL_SIZE: usize = 65536;
const FRAMES_SIZE: usize = 1024;

pub struct VM<'a> {
	constants: &'a Vec<Object>,

	pub globals: Vec<Object>,

	stack: Vec<Object>,
	sp: usize,

	frames: Vec<Frame>,
	frames_index: usize,
}

impl<'a> VM<'a> {
	pub fn new(bytecode: &'a Bytecode) -> VM<'a> {
		let mut stack = Vec::with_capacity(STACK_SIZE);
		stack.resize(STACK_SIZE, OBJ_NULL);

		VM {
			constants: bytecode.constants,

			stack, //TODO: resize with null obj
			sp: 0,

			globals: VM::new_globals(),

			frames: VM::new_frames(bytecode.instructions.to_owned()),
			frames_index: 1,
		}
	}

	pub fn new_with_global_store(bytecode: &'a Bytecode, globals: Vec<Object>) -> VM<'a> {
		let mut vm = VM::new(bytecode);
		vm.globals = globals;
		return vm;
	}

	pub fn new_globals() -> Vec<Object> {
		let mut globals = Vec::with_capacity(GLOBAL_SIZE);
		globals.resize(GLOBAL_SIZE, OBJ_NULL);
		return globals;
	}

	fn new_frames(instructions: Instructions) -> Vec<Frame> {
		let mut frames = Vec::with_capacity(FRAMES_SIZE);

		let main_fn = CompiledFunction { instructions };
		let main_frame = Frame::new(main_fn);

		frames.push(main_frame);

		frames
	}

	pub fn stack_top(&self) -> Option<&Object> {
		if self.sp == 0 {
			return None;
		}

		self.stack.get(self.sp - 1)
	}

	fn set_ip(&mut self, ip: usize) {
		self.frames[self.frames_index].ip = ip;
	}

	//FIXME: remove clone()s
	pub fn run(&mut self) {
		while self.current_frame().ip < self.current_frame().instructions().len() {
			let ip = self.current_frame().ip;
			let ins = self.current_frame().instructions();
			let op = OpCodeType::from(ins[ip]);

			match op {
				OpCodeType::CONSTANT => {
					let const_index = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip = ip + 2;

					self.push(self.constants[const_index].clone());
				}
				OpCodeType::POP => {
					self.pop();
				}
				OpCodeType::ADD | OpCodeType::SUB | OpCodeType::MUL | OpCodeType::DIV => {
					self.execute_binary_operation(op);
				}
				OpCodeType::TRUE => {
					self.push(OBJ_TRUE);
				}
				OpCodeType::FALSE => {
					self.push(OBJ_FALSE);
				}
				OpCodeType::EQ | OpCodeType::NEQ | OpCodeType::GT => {
					self.execute_comparison(op);
				}
				OpCodeType::BANG => {
					self.execute_operator_bang();
				}
				OpCodeType::MINUS => {
					self.execute_operator_minus();
				}
				OpCodeType::JMP => {
					let pos = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip = pos - 1;
				}
				OpCodeType::JMPNT => {
					let pos = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip += 2;

					let condition = self.pop();

					if !condition.is_truthy() {
						self.current_frame().ip = pos - 1;
					}
				}
				OpCodeType::GS => {
					let global_index = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip += 2;

					self.globals[global_index] = self.pop().clone();
				}
				OpCodeType::GG => {
					let global_index = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip += 2;

					self.push(self.globals[global_index].clone());
				}
				OpCodeType::ARR => {
					let global_index = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip += 2;

					let array = self.build_array(self.sp - global_index, self.sp);
					self.sp -= global_index;

					self.push(array);
				}
				OpCodeType::HASH => {
					let global_index = read_uint16(&ins[ip + 1..]);
					self.current_frame().ip += 2;

					let hash = self.build_hash(self.sp - global_index, self.sp);
					self.sp -= global_index;

					self.push(hash);
				}
				OpCodeType::ID => {
					let index = self.pop().to_owned();
					let left = self.pop().to_owned();

					self.execute_expression_index(left, index)
				}
				OpCodeType::NULL => {
					self.push(OBJ_NULL);
				}
				OpCodeType::CALL => {
					let frame = match self.stack[self.sp - 1].clone() {
						Object::COMPILED_FUNCTION(cf) => cf,
						_ => panic!("not a compiled function received")
					};
					self.push_frame(Frame::new(frame));
					//prevent to increment the frame ip
					continue;
				}
				OpCodeType::RETV => {
					let returned = self.pop().to_owned();

					self.pop_frame();
					self.pop();

					self.push(returned);
				}
				OpCodeType::RET => {
					self.pop_frame();
					self.pop();

					self.push(OBJ_NULL);
				}
				_ => panic!("unexpected OpCodeType: {:?}", op)
			}
			self.current_frame().ip += 1;
		};
	}

	fn read_op(&self, ip: usize) -> OpCodeType {
		let ins = &self.frames[self.frames_index].cf.instructions;
		OpCodeType::from(ins[ip])
	}

	fn execute_binary_operation(&mut self, op: OpCodeType) { //TODO: return err
		let right = self.pop().to_owned(); //FIXME: cloning
		let left = self.pop().to_owned();

		match (right.borrow(), left.borrow()) {
			(Object::INTEGER(r), Object::INTEGER(l)) => {
				self.execute_binary_operation_integer(op, *l, *r);
			}
			(Object::STRING(r), Object::STRING(l)) => {
				self.execute_binary_operation_string(op, l.to_string(), r.to_string());
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

	fn execute_binary_operation_string(&mut self, op: OpCodeType, left: String, right: String) {
		if op != OpCodeType::ADD {
			panic!("unknown string operator: {:?}", op)
		}

		let mut result = left.clone();
		result.push_str(&right);

		self.push(Object::STRING(result));
	}

	fn execute_comparison(&mut self, op: OpCodeType) { //TODO: return err
		let right = self.pop().to_owned(); //FIXME: cloning
		let left = self.pop().to_owned();

		match (right.borrow(), left.borrow()) {
			(Object::INTEGER(r), Object::INTEGER(l)) => {
				self.execute_comparison_integer(op, *l, *r);
			}
			(Object::BOOLEAN(r), Object::BOOLEAN(l)) => {
				self.execute_comparison_boolean(op, *l, *r);
			}
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
			}
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
			}
			_ => panic!("unsupported type for negation: {:?}", operand)
		};
	}

	fn execute_expression_index(&mut self, left: Object, index: Object) {
		match (left.borrow(), index.borrow()) {
			(Object::ARRAY(arr), Object::INTEGER(i)) => {
				self.execute_expression_index_array(arr, *i)
			}
			(Object::HASH(hash), _) => {
				self.execute_expression_index_hash(hash, index);
			}
			_ => panic!("unsupported type for index: {:#?} {:#?}", left.clone(), index.clone())
		}
	}

	fn execute_expression_index_array(&mut self, array: &Array, index: isize) {
		if index < 0 || index > array.elements.len() as isize {
			self.push(OBJ_NULL);
			return;
		}

		match array.elements.get(index as usize) {
			Some(e) => self.push(e.clone()),
			None => self.push(OBJ_NULL),
		}
	}

	fn execute_expression_index_hash(&mut self, hash: &Hash, index: Object) {
		let key = Hashable::lookup(index);

		match hash.pairs.get(&key) {
			Some(e) => self.push(e.clone()),
			None => self.push(OBJ_NULL),
		}
	}

	fn build_array(&self, index_start: usize, index_end: usize) -> Object {
		let mut elements = Vec::with_capacity(index_end - index_start);
		elements.resize(index_end - index_start, OBJ_NULL);

		for i in index_start..index_end {
			elements[i - index_start] = self.stack[i].clone();
		}

		Object::ARRAY(Array { elements })
	}

	fn build_hash(&self, index_start: usize, index_end: usize) -> Object {
		let mut pairs = HashMap::new(); //TODO: cap, remove clones

		for i in (index_start..index_end).step_by(2) {
			let key = Hashable::lookup(self.stack[i].clone());
			let value = self.stack[i + 1].clone();

			pairs.insert(key, value);
		}

		Object::HASH(Hash { pairs })
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

	fn current_frame(&mut self) -> &mut Frame {
		&mut self.frames[self.frames_index - 1]
	}

	fn push_frame(&mut self, frame: Frame) {
		self.frames.push(frame);
		self.frames_index += 1;
	}

	fn pop_frame(&mut self) -> Frame {
		self.frames_index -= 1;
		self.frames.pop().expect("empty frames")
	}
}