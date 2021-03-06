extern crate byteorder;

use byteorder::{BigEndian, WriteBytesExt};
use std::error::Error;
use std::{fmt, convert};
use self::byteorder::{ByteOrder, ReadBytesExt};

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
		match def.operand_widths.len() {
			0 => format!("{}", def.name),
			1 => format!("{} {}", def.name, operands[0]),
			2 => format!("{} {} {}", def.name, operands[0], operands[1]),
			_ => panic!("unexpected operand_widths len: {}", def.operand_widths.len())
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCodeType {
	CONSTANT = 0,
	POP = 1,
	ADD = 2,
	SUB = 3,
	MUL = 4,
	DIV = 5,
	TRUE = 6,
	FALSE = 7,
	EQ = 8,
	NEQ = 9,
	GT = 10,
	MINUS = 11,
	BANG = 12,
	JMP = 13,
	JMPNT = 14,
	NULL = 15,
	GG = 16,
	GS = 17,
	ARR = 18,
	HASH = 19,
	ID = 20,
	CALL = 21,
	RETV = 22,
	RET = 23,
	LG = 24,
	LS = 25,
	BG = 26,
	CL = 27,
	FREE = 28,
}

impl From<u8> for OpCodeType {
	fn from(v: u8) -> Self {
		match v {
			0 => OpCodeType::CONSTANT,
			1 => OpCodeType::POP,
			2 => OpCodeType::ADD,
			3 => OpCodeType::SUB,
			4 => OpCodeType::MUL,
			5 => OpCodeType::DIV,
			6 => OpCodeType::TRUE,
			7 => OpCodeType::FALSE,
			8 => OpCodeType::EQ,
			9 => OpCodeType::NEQ,
			10 => OpCodeType::GT,
			11 => OpCodeType::MINUS,
			12 => OpCodeType::BANG,
			13 => OpCodeType::JMP,
			14 => OpCodeType::JMPNT,
			15 => OpCodeType::NULL,
			16 => OpCodeType::GG,
			17 => OpCodeType::GS,
			18 => OpCodeType::ARR,
			19 => OpCodeType::HASH,
			20 => OpCodeType::ID,
			21 => OpCodeType::CALL,
			22 => OpCodeType::RETV,
			23 => OpCodeType::RET,
			24 => OpCodeType::LG,
			25 => OpCodeType::LS,
			26 => OpCodeType::BG,
			27 => OpCodeType::CL,
			28 => OpCodeType::FREE,
			_ => panic!("unhandled u8 to OpCodeType conversion: {}", v),
		}
	}
}

#[derive(Debug)]
pub struct Definition<'a> {
	name: &'a str,
	operand_widths: Vec<u8>,
}

pub fn lookup<'a>(op: OpCodeType) -> Definition<'a> {
	match op {
		OpCodeType::CONSTANT => Definition {
			name: "OpConstant",
			operand_widths: vec![2],
		},
		OpCodeType::POP => Definition {
			name: "OpPop",
			operand_widths: vec![],
		},
		OpCodeType::ADD => Definition {
			name: "OpAdd",
			operand_widths: vec![],
		},
		OpCodeType::SUB => Definition {
			name: "OpSub",
			operand_widths: vec![],
		},
		OpCodeType::MUL => Definition {
			name: "OpMul",
			operand_widths: vec![],
		},
		OpCodeType::DIV => Definition {
			name: "OpDiv",
			operand_widths: vec![],
		},
		OpCodeType::TRUE => Definition {
			name: "OpTrue",
			operand_widths: vec![],
		},
		OpCodeType::FALSE => Definition {
			name: "OpFalse",
			operand_widths: vec![],
		},
		OpCodeType::EQ => Definition {
			name: "OpEqual",
			operand_widths: vec![],
		},
		OpCodeType::NEQ => Definition {
			name: "OpNotEqual",
			operand_widths: vec![],
		},
		OpCodeType::GT => Definition {
			name: "OpGreaterThan",
			operand_widths: vec![],
		},
		OpCodeType::MINUS => Definition {
			name: "OpMinus",
			operand_widths: vec![],
		},
		OpCodeType::BANG => Definition {
			name: "OpBang",
			operand_widths: vec![],
		},
		OpCodeType::JMP => Definition {
			name: "OpJump",
			operand_widths: vec![2],
		},
		OpCodeType::JMPNT => Definition {
			name: "OpJumpNotTruthy",
			operand_widths: vec![2],
		},
		OpCodeType::NULL => Definition {
			name: "OpNull",
			operand_widths: vec![],
		},
		OpCodeType::GG => Definition {
			name: "OpGlobalGet",
			operand_widths: vec![2],
		},
		OpCodeType::GS => Definition {
			name: "OpGlobalSet",
			operand_widths: vec![2],
		},
		OpCodeType::ARR => Definition {
			name: "OpArray",
			operand_widths: vec![2],
		},
		OpCodeType::HASH => Definition {
			name: "OpHash",
			operand_widths: vec![2],
		},
		OpCodeType::ID => Definition {
			name: "OpIndex",
			operand_widths: vec![],
		},
		OpCodeType::CALL => Definition {
			name: "OpCall",
			operand_widths: vec![1],
		},
		OpCodeType::RETV => Definition {
			name: "OpReturnValue",
			operand_widths: vec![],
		},
		OpCodeType::RET => Definition {
			name: "OpReturn",
			operand_widths: vec![],
		},
		OpCodeType::LG => Definition {
			name: "OpGetLocal",
			operand_widths: vec![1],
		},
		OpCodeType::LS => Definition {
			name: "OpSetLocal",
			operand_widths: vec![1],
		},
		OpCodeType::BG => Definition {
			name: "OpGetBuiltin",
			operand_widths: vec![1],
		},
		OpCodeType::CL => Definition {
			name: "OpClosure",
			operand_widths: vec![2, 1],
		},
		OpCodeType::FREE => Definition {
			name: "OpFree",
			operand_widths: vec![1],
		}
	}
}

pub fn make(op: OpCodeType, operands: &Vec<usize>) -> Result<Vec<u8>, CodeError> {
	let definition = lookup(op);
	let capacity: usize = definition.operand_widths.iter().map(|w| *w as usize).sum::<usize>(); //usize

	let mut instructions = Vec::with_capacity(1 + capacity);
	instructions.push(op as u8);

	for (operand, width) in operands.into_iter().zip(definition.operand_widths) {
		match width {
			2 => match instructions.write_u16::<BigEndian>(*operand as u16) {
				Err(e) => {
					return Err(CodeError::WRITE_OPERAND(*operand as u16));
				}
				_ => {}
			},
			1 => match instructions.write_u8(*operand as u8) {
				Err(e) => {
					return Err(CodeError::WRITE_OPERAND(*operand as u8 as u16));
				}
				_ => {}
			},
			_ => {
				return Err(CodeError::UNEXPECTED_WIDTH(width));
			}
		}
	}

	Ok(instructions)
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<usize>, usize) {
	let mut operands: Vec<usize> = Vec::with_capacity(def.operand_widths.len());
	let mut offset: usize = 0;

	for (i, width) in def.operand_widths.iter().enumerate() {
		match width {
			2 => {
				let value = BigEndian::read_u16(&ins[offset..]);
				operands.push(value as usize);
			},
			1 => {
				operands.push(ins[offset] as usize);
			},
			_ => panic!("unexpected width {}", width)
		}
		offset += *width as usize;
	}

	(operands, offset)
}

pub fn read_uint16(ins: &[u8]) -> usize {
	BigEndian::read_u16(ins) as usize
}
