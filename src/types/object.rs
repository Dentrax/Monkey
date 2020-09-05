use crate::ast::ast::BlockStatement;

use crate::types::array::*;
use crate::types::hashable::*;
use crate::types::builtins::*;
use crate::types::env::*;

use std::{fmt};
use std::fmt::{Formatter};
use std::rc::Rc;
use std::cell::RefCell;
use crate::code::code::Instructions;

pub const STR_FUNCTION: &'static str = "FUNCTION";
pub const STR_COMPILED_FUNCTION: &'static str = "COMPILED_FUNCTION";
pub const STR_BUILTIN: &'static str = "BUILTIN";
pub const STR_ARRAY: &'static str = "ARRAY";
pub const STR_HASH: &'static str = "HASH";
pub const STR_INTEGER: &'static str = "INTEGER";
pub const STR_BOOLEAN: &'static str = "BOOLEAN";
pub const STR_STRING: &'static str = "STRING";
pub const STR_RETURN: &'static str = "RETURN";
pub const STR_ERROR: &'static str = "ERROR";
pub const STR_NULL: &'static str = "NULL";

pub const OBJ_NULL: Object = Object::NULL;
pub const OBJ_TRUE: Object = Object::BOOLEAN(true);
pub const OBJ_FALSE: Object = Object::BOOLEAN(false);

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	pub parameters: Vec<String>,
	pub body: BlockStatement,
	pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let parameters = self.parameters.join(", ");

		write!(f, "<fn({})> {{\n{}\n}}", parameters, self.body)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledFunction {
	pub instructions: Instructions,
	pub num_locals: usize,
	pub num_params: usize,
}

impl fmt::Display for CompiledFunction {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "<compiled fn()> {{\n{:#?}\n}}, num_locals: {}", self.instructions, self.num_locals)
	}
}

impl Default for CompiledFunction {
	fn default() -> Self {
		CompiledFunction {
			instructions: vec![],
			num_locals: 0,
			num_params: 0
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
	FUNCTION(Function),
	COMPILED_FUNCTION(CompiledFunction),
	BUILTIN(Builtin),
	ARRAY(Array),
	HASH(Hash),
	INTEGER(isize),
	BOOLEAN(bool),
	STRING(String),
	RETURN(Box<Object>),
	ERROR(String),
	NULL,
}

impl Object {
	pub fn is_function(&self) -> bool {
		self.get_type() == STR_FUNCTION
	}

	pub fn is_compiled_function(&self) -> bool {
		self.get_type() == STR_COMPILED_FUNCTION
	}

	pub fn is_builtin(&self) -> bool {
		self.get_type() == STR_BUILTIN
	}

	pub fn is_array(&self) -> bool {
		self.get_type() == STR_ARRAY
	}

	pub fn is_hash(&self) -> bool {
		self.get_type() == STR_HASH
	}

	pub fn is_integer(&self) -> bool {
		self.get_type() == STR_INTEGER
	}

	pub fn is_boolean(&self) -> bool {
		self.get_type() == STR_BOOLEAN
	}

	pub fn is_string(&self) -> bool {
		self.get_type() == STR_STRING
	}

	pub fn is_return(&self) -> bool {
		self.get_type() == STR_RETURN
	}

	pub fn is_error(&self) -> bool {
		self.get_type() == STR_ERROR
	}

	pub fn is_null(&self) -> bool {
		self.get_type() == STR_NULL
	}

	pub fn get_type(&self) -> &str {
		match self {
			Object::FUNCTION(_) => STR_FUNCTION,
			Object::COMPILED_FUNCTION(_) => STR_COMPILED_FUNCTION,
			Object::BUILTIN(_) => STR_BUILTIN,
			Object::ARRAY(_) => STR_ARRAY,
			Object::HASH(_) => STR_HASH,
			Object::INTEGER(_) => STR_INTEGER,
			Object::BOOLEAN(_) => STR_BOOLEAN,
			Object::STRING(_) => STR_STRING,
			Object::RETURN(_) => STR_RETURN,
			Object::ERROR(_) => STR_ERROR,
			Object::NULL => STR_NULL,
		}
	}

	pub fn is_truthy(&self) -> bool {
		match self {
			Object::BOOLEAN(b) => *b,
			Object::NULL => false,
			_ => true
		}
	}
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Object::FUNCTION(func) => func.fmt(f),
			Object::COMPILED_FUNCTION(func) => func.fmt(f),
			Object::BUILTIN(b) => b.fmt(f),
			Object::ARRAY(a) => a.fmt(f),
			Object::HASH(h) => h.fmt(f),
			Object::INTEGER(i) => write!(f, "{}", i),
			Object::BOOLEAN(i) => write!(f, "{}", i),
			Object::STRING(s) => write!(f, "{}", s),
			Object::RETURN(r) => write!(f, "{}", r),
			Object::ERROR(e) => write!(f, "{}", e),
			Object::NULL => write!(f, ""),
		}
	}
}
