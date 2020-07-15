extern crate byteorder;
use byteorder::{BigEndian, WriteBytesExt};
use std::error::Error;
use std::{fmt, convert};
use self::byteorder::ByteOrder;

pub type Instructions = Vec<u8>;

pub trait IInstructions {
	fn string(&self) -> String;
	fn fmt_instruction(&self, def: &Definition, operands: &Vec<usize>) -> String;
}

impl IInstructions for Instructions {
	fn string(&self) -> String {
		let mut out = String::new();
		let mut i = 0;

		while i < self.len() {
			if let Some(v) = self.get(i) {
				let def = lookup(OpCodeType::from(*v));

				let (operands, read) = read_operands(&def, &self[i + 1..]);

				let fmt = self.fmt_instruction(&def, &operands);

				out.push_str(&format!("{:04} {}\n", i, fmt));

				i += 1 + read;
			}
		}

		out
	}

	fn fmt_instruction(&self, def: &Definition, operands: &Vec<usize>) -> String {
		match def.operandWidths.len() {
			0 => format!("{}", def.name),
			1 => format!("{} {}", def.name, operands[0]),
			_ => panic!("unexpected operandWidths len: {}", def.operandWidths.len())
		}
	}
}

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
	ADD = 1,
}

impl From<u8> for OpCodeType {
	fn from(v: u8) -> Self {
		match v {
			0 => OpCodeType::CONSTANT,
			1 => OpCodeType::ADD,
			_ => panic!("unhandled u8 to OpCodeType conversion: {}", v),
		}
	}
}

#[derive(Debug)]
pub struct Definition<'a> {
	name: &'a str,
	operandWidths: Vec<u8>,
}

pub fn lookup<'a>(op: OpCodeType) -> Definition<'a> {
	match op {
		OpCodeType::CONSTANT => Definition {
			name: "OpConstant",
			operandWidths: vec![2],
		},
		OpCodeType::ADD => Definition {
			name: "OpAdd",
			operandWidths: vec![],
		}
	}
}

pub fn make(op: OpCodeType, operands: &Vec<usize>) -> Result<Vec<u8>, CodeError> {
	let definition = lookup(op);
	let capacity: usize = definition.operandWidths.iter().map(|w| *w as usize).sum::<usize>(); //usize

	let mut instructions = Vec::with_capacity(1 + capacity);
	instructions.push(op as u8);

	for (operand, width) in operands.into_iter().zip(definition.operandWidths) {
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
	}

	Ok(instructions)
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<usize>, usize) {
	let mut operands: Vec<usize> = Vec::with_capacity(def.operandWidths.len());
	let mut offset: usize = 0;

	for (i, width) in def.operandWidths.iter().enumerate() {
		match width {
			2 => {
				let value = BigEndian::read_u16(&ins[offset..]);
				operands.push(value as usize);
			}
			_ => panic!("unexpected width {}", width)
		}
		offset += *width as usize;
	}

	(operands, offset)
}

pub fn read_uint16(ins: &[u8]) -> usize {
	BigEndian::read_u16(ins) as usize
}