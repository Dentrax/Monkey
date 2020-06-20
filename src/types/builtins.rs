use crate::types::object::*;
use crate::types::array::*;
use crate::eval::eval::*;

use std::{fmt};
use std::fmt::{Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
	LEN,
	FIRST,
	LAST,
	REST,
	REVERSE,
	PUSH,
	PUTS,
}

impl fmt::Display for Builtin {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Builtin::LEN => write!(f, "len"),
			Builtin::FIRST => write!(f, "first"),
			Builtin::LAST => write!(f, "last"),
			Builtin::REST => write!(f, "rest"),
			Builtin::REVERSE => write!(f, "reverse"),
			Builtin::PUSH => write!(f, "push"),
			Builtin::PUTS => write!(f, "puts"),
		}
	}
}

impl Builtin {
	pub fn lookup(builtin: String) -> Option<Self> {
		match builtin.as_str() {
			"len" => Some(Builtin::LEN),
			"first" => Some(Builtin::FIRST),
			"last" => Some(Builtin::LAST),
			"rest" => Some(Builtin::REST),
			"reverse" => Some(Builtin::REVERSE),
			"push" => Some(Builtin::PUSH),
			"puts" => Some(Builtin::PUTS),
			_ => None
		}
	}

	pub fn apply(&self, args: Vec<Object>) -> Result<Object, EvalError> {
		match self {
			Builtin::LEN => builtin_len(&args),
			Builtin::FIRST => builtin_first(&args),
			Builtin::LAST => builtin_last(&args),
			Builtin::REST => builtin_rest(&args),
			Builtin::REVERSE => builtin_reverse(&args),
			Builtin::PUSH => builtin_push(&args),
			Builtin::PUTS => builtin_puts(&args),
		}
	}
}

fn builtin_len(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::STRING(str) => Ok(Object::INTEGER(str.len() as isize)),
		Object::ARRAY(a) => Ok(Object::INTEGER(a.elements.len() as isize)),
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::LEN, args[0].clone()))
	}
}

fn builtin_first(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(a.elements.first().unwrap().clone())
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::FIRST, args[0].clone()))
	}
}

fn builtin_last(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(a.elements.last().unwrap().clone())
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::LAST, args[0].clone()))
	}
}

fn builtin_rest(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(Object::ARRAY(Array{elements: a.elements.iter().skip(1).cloned().collect()}))
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::REST, args[0].clone()))
	}
}

fn builtin_reverse(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(Object::ARRAY(Array{elements: a.elements.clone().into_iter().rev().collect()}))
			} else {
				Ok(OBJ_NULL)
			}
		},
		Object::STRING(str) => {
			Ok(Object::STRING(str.chars().rev().collect::<String>()))
		}
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::REVERSE, args[0].clone()))
	}
}

fn builtin_push(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 2 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match (&args[0], &args[1]) {
		(Object::ARRAY(a), Object::INTEGER(i)) | (Object::INTEGER(i), Object::ARRAY(a)) => {
			let mut result = a.elements.clone();
			result.push(Object::INTEGER(*i));

			Ok(Object::ARRAY(Array{elements: result.clone()}))
		}
		(Object::ARRAY(l), Object::ARRAY(r)) => {
			let mut result = r.elements.clone();
			result.push(args[0].clone());

			Ok(Object::ARRAY(Array{elements: result.clone()}))
		}
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::PUSH, args[0].clone()))
	}
}

fn builtin_puts(args: &[Object]) -> Result<Object, EvalError> {
	for arg in args {
		println!("{}", arg);
	}
	Ok(OBJ_NULL)
}