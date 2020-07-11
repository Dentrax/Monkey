extern crate byteorder;
use byteorder::{BigEndian, WriteBytesExt};
use std::error::Error;
use std::{fmt, convert};

pub type Instructions = Vec<u8>;

#[derive(Debug, Eq, PartialEq)]
pub enum CodeError {
	WRITE_OPERAND(u16),
	UNEXPECTED_WIDTH(u8),
}

impl fmt::Display for CodeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			CodeError::WRITE_OPERAND(v) => write!(f, "Err while writing operand: {}", v),
			CodeError::UNEXPECTED_WIDTH(v) => write!(f, "Unexpected width: {}", v),
		}
	}
}

#[derive(Clone, Copy)]
pub enum OpCodeType {
	CONSTANT = 0,
}

impl From<u8> for OpCodeType {
	fn from(v: u8) -> Self {
		match v {
			0 => OpCodeType::CONSTANT,
			_ => panic!("unhandled u8 to Opcode conversion: {}", v),
		}
	}
}

#[derive(Debug)]
struct Definition<'a> {
	name: &'a str,
	operandWidths: Vec<u8>,
}

fn lookup<'a>(op: OpCodeType) -> Definition<'a> {
	match op {
		OpCodeType::CONSTANT => Definition {
			name: "OpConstant",
			operandWidths: vec![2],
		}
	}
}

pub fn make(op: OpCodeType, operands: Vec<usize>) -> Result<Vec<u8>, CodeError> {
	let definition = lookup(op);

	let capacity: usize = definition.operandWidths.iter().map(|w| *w as usize).sum::<usize>(); //usize

	let mut instructions = Vec::with_capacity(1 + capacity);
	instructions.push(op as u8);

	let mut offset = 1;
	for (i, operand) in operands.iter().enumerate() {
		let width = definition.operandWidths[i];

		match width {
			2 => match instructions.write_u16::<BigEndian>(*operand as u16) {
				Err(e) => {
					return Err(CodeError::WRITE_OPERAND(*operand as u16))
				}
				_ => {}
			}
			_ => {
				return Err(CodeError::UNEXPECTED_WIDTH(width))
			}
		}

		offset += width;
	}

	Ok(instructions)
}