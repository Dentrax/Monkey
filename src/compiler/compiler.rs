use crate::types::object::Object;
use crate::code::code::{Instructions, OpCodeType, make};
use std::error::Error;
use crate::ast::ast;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::ast::{Program, Node, InfixType, Literal, Expression};

pub struct Compiler {
	instructions: Instructions,
	constants: Vec<Object>,
}

#[derive(Debug, PartialEq)]
pub enum CompilerError {
	WRONG_CONSTANTS_TYPE { want: Object, got: Object },
	WRONG_NUMBER_OF_CONSTANTS { want: usize, got: usize },
	WRONG_CONSTANTS_INTEGER_EQUALITY{ want: isize, got: isize },
	WRONG_INSTRUCTION_AT { at: usize, want: Vec<u8>, got: Vec<u8> },
	WRONG_INSTRUCTIONS_LENGTH { want: String, got: String },
	UNKNOWN_INFIX_OPERATOR(InfixType),
	UNKNOWN_EXPRESSION_LITERAL(Literal),
	UNKNOWN_EXPRESSION(Expression),
}

impl fmt::Display for CompilerError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			CompilerError::WRONG_CONSTANTS_TYPE { want, got } => {
				write!(f, "different types: got: {} want: {}", got, want)
			},
			CompilerError::WRONG_NUMBER_OF_CONSTANTS { want, got } => {
				write!(f, "wrong number of constants: got: {} want: {}", got, want)
			},
			CompilerError::WRONG_CONSTANTS_INTEGER_EQUALITY { want, got } => {
				write!(f, "wrong constants equality (integer): got: {} want: {}", got, want)
			},
			CompilerError::WRONG_INSTRUCTION_AT { at, want, got } => {
				write!(f, "wrong instruction: at: {} got: {:#?} want: {:#?}", at, got, want)
			},
			CompilerError::WRONG_INSTRUCTIONS_LENGTH { want, got } => {
				write!(f, "wrong instructions length:\ngot:\n{:#?}\nwant:\n{:#?}", got, want)
			},
			CompilerError::UNKNOWN_INFIX_OPERATOR(t) => {
				write!(f, "unknown infix operator: {}", t.to_string())
			},
			CompilerError::UNKNOWN_EXPRESSION_LITERAL(l) => {
				write!(f, "unknown expression literal: {}", l.to_string())
			},
			CompilerError::UNKNOWN_EXPRESSION(e) => {
				write!(f, "unknown expression: {}", e.to_string())
			},
		}
	}
}

pub struct Bytecode<'a> {
	pub(crate) instructions: &'a Instructions,
	pub(crate) constants: &'a Vec<Object>,
}

impl Compiler {

	pub fn new() -> Self {
		Compiler {
			instructions: vec![],
			constants: vec![],
		}
	}

	pub fn compile(&mut self, node: Node) -> Result<Bytecode, CompilerError> {
		match node {
			Node::PROGRAM(p) => {
				self.compile_program(&p)?;
			},
			Node::EXPRESSION(e) => {
				self.compile_expression(&e)?;
			}
			Node::STATEMENT(s) => {
				self.compile_statement(&s)?;
			}
		}
		Ok(self.bytecode())
	}

	fn compile_program(&mut self, p: &ast::Program) -> Result<(), CompilerError> {
		for s in &p.statements {
			self.compile_statement(s)?;
		}

		Ok(())
	}

	fn compile_statement(&mut self, stmt: &ast::Statement) -> Result<(), CompilerError> {
		match stmt {
			ast::Statement::EXPRESSION(e) => {
				self.compile_expression(e)?;
				self.emit(OpCodeType::POP, &vec![]);
			}
			_ => panic!("[compile::statement]: Unexpected statement: {}", stmt.to_string())
		}
		Ok(())
	}

	fn compile_expression(&mut self, expr: &ast::Expression) -> Result<(), CompilerError> {
		match expr {
			ast::Expression::INFIX(i) => {
				if i.operator == InfixType::LT {
					self.compile_expression(&i.right)?;
					self.compile_expression(&i.left)?;
					self.emit(OpCodeType::GT, &vec![]);
					return Ok(())
				}

				self.compile_expression(&i.left)?;
				self.compile_expression(&i.right)?;

				match i.operator {
					InfixType::PLUS => {
						self.emit(OpCodeType::ADD, &vec![]);
					},
					InfixType::MINUS => {
						self.emit(OpCodeType::SUB, &vec![]);
					},
					InfixType::DIVISION => {
						self.emit(OpCodeType::DIV, &vec![]);
					},
					InfixType::MULTIPLICATION => {
						self.emit(OpCodeType::MUL, &vec![]);
					},
					InfixType::EQ => {
						self.emit(OpCodeType::EQ, &vec![]);
					},
					InfixType::NEQ => {
						self.emit(OpCodeType::NEQ, &vec![]);
					},
					InfixType::GT => {
						self.emit(OpCodeType::GT, &vec![]);
					},
					_ => return Err(CompilerError::UNKNOWN_INFIX_OPERATOR(i.operator.clone()))
				}
			},
			ast::Expression::LITERAL(l) => {
				match l {
					ast::Literal::INT(i) => {
						let integer = Object::INTEGER(*i);
						let ops = &vec![self.add_constant(integer)];
						let p = self.emit(OpCodeType::CONSTANT, ops);
					}
					ast::Literal::BOOL(b) => {
						if *b {
							self.emit(OpCodeType::TRUE, &vec![]);
						} else {
							self.emit(OpCodeType::FALSE, &vec![]);
						}
					}
					_ => return Err(CompilerError::UNKNOWN_EXPRESSION_LITERAL(l.clone()))
				};
			}
			_ => return Err(CompilerError::UNKNOWN_EXPRESSION(expr.clone()))
		};
		Ok(())
	}

	fn emit(&mut self, op: OpCodeType, operands: &Vec<usize>) -> usize {
		let ins = make(op, operands).unwrap();
		let pos = self.add_instruction(&ins);
		return pos
	}

	fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
		let pos_new_instruction = self.instructions.len();
		self.instructions.extend_from_slice(ins);
		pos_new_instruction
	}

	fn add_constant(&mut self, obj: Object) -> usize {
		self.constants.push(obj);
		return self.constants.len() - 1;
	}

	pub fn bytecode(&self) -> Bytecode {
		Bytecode {
			instructions: &self.instructions,
			constants: &self.constants,
		}
	}
}