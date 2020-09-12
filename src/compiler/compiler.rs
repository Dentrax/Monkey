use crate::types::object::{Object, OBJ_NULL, CompiledFunction};
use crate::code::code::{Instructions, OpCodeType, make, IInstructions};
use std::error::Error;
use crate::ast::ast;
use std::fmt::{Formatter};
use std::fmt;
use crate::ast::ast::{Node, InfixType, PrefixType, Literal, Expression};
use crate::compiler::symbol_table::{SymbolTable, SymbolScope, Symbol};
use crate::types::builtins::Builtin;

#[derive(Clone)]
pub struct CompilationScope {
	pub instructions: Instructions,
	pub last_instruction: Option<EmittedInstruction>,
	pub prev_instruction: Option<EmittedInstruction>,
}

pub struct Compiler {
	pub constants: Vec<Object>,
	pub symbol_table: SymbolTable,
	pub scopes: Vec<CompilationScope>,
	pub scope_index: usize,
}

#[derive(Clone)]
pub struct EmittedInstruction {
	pub opcode: OpCodeType,
	position: usize,
}

impl EmittedInstruction {
	fn is_pop(&self) -> bool {
		self.opcode == OpCodeType::POP
	}
}

#[derive(Debug, PartialEq)]
pub enum CompilerError {
	WRONG_CONSTANTS_TYPE { want: Object, got: Object },
	WRONG_NUMBER_OF_CONSTANTS { want: usize, got: usize },
	WRONG_CONSTANTS_INTEGER_EQUALITY { want: isize, got: isize },
	WRONG_CONSTANTS_STRING_EQUALITY { want: String, got: String },
	WRONG_COMPILED_FUNCTION_EQUALITY { want: CompiledFunction, got: CompiledFunction },
	WRONG_INSTRUCTION_AT { at: usize, want: String, got: String },
	WRONG_INSTRUCTIONS_LENGTH { want: String, got: String },
	UNKNOWN_INFIX_OPERATOR(InfixType),
	UNKNOWN_EXPRESSION_LITERAL(Literal),
	UNKNOWN_EXPRESSION(Expression),
	UNDEFINED_SYMBOL(String),
}

impl fmt::Display for CompilerError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			CompilerError::WRONG_CONSTANTS_TYPE { want, got } => {
				write!(f, "different types: got: {} want: {}", got, want)
			}
			CompilerError::WRONG_NUMBER_OF_CONSTANTS { want, got } => {
				write!(f, "wrong number of constants: got: {} want: {}", got, want)
			}
			CompilerError::WRONG_CONSTANTS_INTEGER_EQUALITY { want, got } => {
				write!(f, "wrong constants equality (integer): got: {} want: {}", got, want)
			}
			CompilerError::WRONG_CONSTANTS_STRING_EQUALITY { want, got } => {
				write!(f, "wrong constants equality (string): got: {} want: {}", got, want)
			}
			CompilerError::WRONG_COMPILED_FUNCTION_EQUALITY { want, got } => {
				write!(f, "wrong compiled function (fn): got: {} want: {} instructions got: {} want: {}", got, want, got.instructions.string(), want.instructions.string())
			}
			CompilerError::WRONG_INSTRUCTION_AT { at, want, got } => {
				write!(f, "wrong instruction: at: {}\ngot:\n{:#?}\nwant:\n{:#?}", at, got, want)
			}
			CompilerError::WRONG_INSTRUCTIONS_LENGTH { want, got } => {
				write!(f, "wrong instructions length:\ngot:\n{:#?}\nwant:\n{:#?}", got, want)
			}
			CompilerError::UNKNOWN_INFIX_OPERATOR(t) => {
				write!(f, "unknown infix operator: {}", t.to_string())
			}
			CompilerError::UNKNOWN_EXPRESSION_LITERAL(l) => {
				write!(f, "unknown expression literal: {}", l.to_string())
			}
			CompilerError::UNKNOWN_EXPRESSION(e) => {
				write!(f, "unknown expression: {}", e.to_string())
			}
			CompilerError::UNDEFINED_SYMBOL(s) => {
				write!(f, "undefined symbol: {}", s)
			}
		}
	}
}

pub struct Bytecode<'a> {
	pub instructions: &'a Instructions,
	pub constants: &'a Vec<Object>,
}

impl Compiler {
	pub fn new() -> Self {
		let scope_main = CompilationScope {
			instructions: vec![],
			last_instruction: None,
			prev_instruction: None
		};

		let mut symbol_table = SymbolTable::new();

		for (i, v) in Builtin::iterator().into_iter().enumerate() {
			symbol_table.define_builtin(i, v.to_string());
		}

		Compiler {
			constants: vec![],
			symbol_table,
			scopes: vec![scope_main],
			scope_index: 0
		}
	}

	pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
		let scope_main = CompilationScope {
			instructions: vec![],
			last_instruction: None,
			prev_instruction: None
		};

		Compiler {
			constants,
			symbol_table,
			scopes: vec![scope_main],
			scope_index: 0
		}
	}

	pub fn compile(&mut self, node: Node) -> Result<Bytecode, CompilerError> {
		match node {
			Node::PROGRAM(p) => {
				self.compile_program(&p)?;
			}
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
			ast::Statement::BLOCK(b) => {
				self.compile_statement_block(b)?;
			}
			ast::Statement::LET(l) => {
				self.compile_expression(&l.value)?;

				let symbol = self.symbol_table.define(&l.name);

				let op_scope = match symbol.scope {
					SymbolScope::GLOBAL => OpCodeType::GS,
					SymbolScope::LOCAL => OpCodeType::LS,
					_ => panic!("unexpected builtin handle")
				};

				self.emit(op_scope, &vec![symbol.index]);
			}
			ast::Statement::RETURN(r) => {
				self.compile_expression(&r.value)?;
				self.emit(OpCodeType::RETV, &vec![]);
			}
			_ => panic!("[compile::statement]: Unexpected statement: {}", stmt.to_string())
		}

		Ok(())
	}

	fn compile_statement_block(&mut self, stmt: &ast::BlockStatement) -> Result<(), CompilerError> {
		for stmt in &stmt.statements {
			self.compile_statement(stmt)?;
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
					return Ok(());
				}

				self.compile_expression(&i.left)?;
				self.compile_expression(&i.right)?;

				match i.operator {
					InfixType::PLUS => {
						self.emit(OpCodeType::ADD, &vec![]);
					}
					InfixType::MINUS => {
						self.emit(OpCodeType::SUB, &vec![]);
					}
					InfixType::DIVISION => {
						self.emit(OpCodeType::DIV, &vec![]);
					}
					InfixType::MULTIPLICATION => {
						self.emit(OpCodeType::MUL, &vec![]);
					}
					InfixType::EQ => {
						self.emit(OpCodeType::EQ, &vec![]);
					}
					InfixType::NEQ => {
						self.emit(OpCodeType::NEQ, &vec![]);
					}
					InfixType::GT => {
						self.emit(OpCodeType::GT, &vec![]);
					}
					_ => return Err(CompilerError::UNKNOWN_INFIX_OPERATOR(i.operator.clone()))
				}
			}
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
					ast::Literal::STRING(s) => {
						let str = Object::STRING(s.to_string());
						let ops = &vec![self.add_constant(str)];
						self.emit(OpCodeType::CONSTANT, ops);
					}
					_ => return Err(CompilerError::UNKNOWN_EXPRESSION_LITERAL(l.clone()))
				};
			}
			ast::Expression::PREFIX(p) => {
				self.compile_expression(&p.right)?;

				match &p.operator {
					PrefixType::BANG => {
						self.emit(OpCodeType::BANG, &vec![]);
					}
					PrefixType::MINUS => {
						self.emit(OpCodeType::MINUS, &vec![]);
					}
				}
			}
			ast::Expression::IF(i) => {
				self.compile_expression(&i.condition)?;

				//Emit an `OpJumpNotTruthy` with a bogus value
				let jump_not_truthy_pos = self.emit(OpCodeType::JMPNT, &vec![9999]);

				self.compile_statement_block(&i.consequence)?;

				if self.is_instruction_last_type(OpCodeType::POP) {
					self.remove_instruction_last();
				}
				//Emit an `OpJump` with a bogus value
				let jump_pos = self.emit(OpCodeType::JMP, &vec![9999]);
				let after_consequence_pos = self.current_instructions().len();

				self.change_operand(jump_not_truthy_pos, after_consequence_pos);

				if let Some(v) = &i.alternative {
					self.compile_statement_block(&i.alternative.as_ref().unwrap())?;
					if self.is_instruction_last_type(OpCodeType::POP) {
						self.remove_instruction_last();
					}
				} else {
					self.emit(OpCodeType::NULL, &vec![]);
				}

				let after_consequence_pos = self.current_instructions().len();
				self.change_operand(jump_pos, after_consequence_pos);
			}
			ast::Expression::IDENT(name) => {
				match self.symbol_table.resolve(name) {
					Some(symbol) => {
						self.load_symbol(&symbol); //clone
					}
					_ => {
						return Err(CompilerError::UNDEFINED_SYMBOL(name.to_string()));
					}
				}
			}
			ast::Expression::ARRAY(arr) => {
				for a in &arr.elements {
					self.compile_expression(a)?;
				}
				self.emit(OpCodeType::ARR, &vec![arr.elements.len()]);
			}
			ast::Expression::HASH(hash) => {
				let mut keys: Vec<&Expression> = hash.pairs.keys().into_iter().collect();
				keys.sort_by(|l, r| (*l).to_string().cmp(&(*r).to_string()));

				for k in &keys {
					self.compile_expression(*k)?;
					self.compile_expression(hash.pairs.get(*k).unwrap())?;
				}
				self.emit(OpCodeType::HASH, &vec![keys.len() * 2]);
			}
			ast::Expression::INDEX(id) => {
				self.compile_expression(&id.left)?;
				self.compile_expression(&id.index)?;

				self.emit(OpCodeType::ID, &vec![]);
			}
			ast::Expression::FUNCTION(func) => {
				self.enter_scope();

				for parameter in &func.parameters {
					self.symbol_table.define(parameter);
				}

				self.compile_statement_block(&func.body);

				if self.is_instruction_last_type(OpCodeType::POP) {
					self.replace_last_pop_with_return();
				}

				if !self.is_instruction_last_type(OpCodeType::RETV) {
					self.emit(OpCodeType::RET, &vec![]);
				}

				let free_symbols = self.symbol_table.free_symbols.clone();
				let num_locals = self.symbol_table.num_definitions;
				let num_params = func.parameters.len();
				let instructions = self.leave_scope();

				for s in &free_symbols {
					self.load_symbol(s);
				}

				let compiled_fn = Object::COMPILED_FUNCTION(CompiledFunction { instructions, num_locals, num_params });

				let fn_index = self.add_constant(compiled_fn);

				let ops = &vec![fn_index, free_symbols.len()];

				self.emit(OpCodeType::CL, ops);
			}
			ast::Expression::CALL(call) => {
				self.compile_expression(&call.function);

				for argument in &call.arguments {
					self.compile_expression(argument)?;
				}

				self.emit(OpCodeType::CALL, &vec![call.arguments.len()]);
			}
			_ => return Err(CompilerError::UNKNOWN_EXPRESSION(expr.clone()))
		};

		Ok(())
	}

	pub fn emit(&mut self, op: OpCodeType, operands: &Vec<usize>) -> usize {
		let ins = make(op, operands).unwrap();
		let pos = self.add_instruction(&ins);
		self.set_instruction_last(op, pos);
		return pos;
	}

	fn load_symbol(&mut self, s: &Symbol) {
		let op = match &s.scope {
			SymbolScope::GLOBAL => OpCodeType::GG,
			SymbolScope::LOCAL => OpCodeType::LG,
			SymbolScope::BUILTIN => OpCodeType::BG,
			SymbolScope::FREE => OpCodeType::FREE,
		};

		self.emit(op, &vec![s.index]);
	}

	fn set_instruction_last(&mut self, op: OpCodeType, pos: usize) {
		if let Some(instruction) = &self.scopes[self.scope_index].last_instruction {
			self.scopes[self.scope_index].prev_instruction = Some(instruction.clone());
		}
		let last = EmittedInstruction { opcode: op, position: pos };

		self.scopes[self.scope_index].last_instruction = Some(last);
	}

	fn remove_instruction_last(&mut self) {
		let ref mut scope = self.scopes[self.scope_index];

		if let Some(instruction) = &scope.last_instruction {
			scope.instructions.truncate(instruction.position);
		}

		scope.last_instruction = scope.prev_instruction.clone();
	}

	fn is_instruction_last_type(&mut self, op: OpCodeType) -> bool {
		if self.current_instructions().len() == 0 {
			return false;
		}

		if let Some(ins) = &self.scopes[self.scope_index].last_instruction {
			return ins.opcode == op
		}

		return false;
	}

	fn replace_instruction(&mut self, pos: usize, new_instruction: Vec<u8>) {
		for i in 0..new_instruction.len() {
			self.scopes[self.scope_index].instructions[pos + i] = new_instruction[i];
		}
	}

	fn replace_last_pop_with_return(&mut self) {
		if let Some(ref ins) = self.scopes[self.scope_index].last_instruction {
			self.replace_instruction(ins.position, make(OpCodeType::RETV, &vec![]).unwrap());
		}

		if let Some(ref mut ins) = self.scopes[self.scope_index].last_instruction {
			ins.opcode = OpCodeType::RETV;
		}
	}

	fn change_operand(&mut self, op_pos: usize, operand: usize) {
		let op = OpCodeType::from(self.scopes[self.scope_index].instructions[op_pos]);
		let new_instruction = make(op, &vec![operand]);
		self.replace_instruction(op_pos, new_instruction.unwrap());
	}

	pub fn current_instructions(&self) -> &Instructions {
		&self.scopes[self.scope_index].instructions
	}

	fn add_instruction(&mut self, ins: &Vec<u8>) -> usize {
		let pos_new_instruction = self.current_instructions().len();

		self.scopes[self.scope_index].instructions.extend_from_slice(ins);

		pos_new_instruction
	}

	fn add_constant(&mut self, obj: Object) -> usize {
		self.constants.push(obj);
		return self.constants.len() - 1;
	}

	pub fn enter_scope(&mut self) {
		let scope = CompilationScope {
			instructions: vec![],
			last_instruction: None,
			prev_instruction: None
		};

		self.scopes.push(scope);
		self.scope_index += 1;

		self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
	}

	pub fn leave_scope(&mut self) -> Instructions {
		self.scope_index -= 1;

		if let Some(s) = &self.symbol_table.outer {
			self.symbol_table = s.as_ref().clone()
		}

		self.scopes.pop().unwrap().instructions
	}

	pub fn bytecode(&self) -> Bytecode {
		Bytecode {
			instructions: &self.scopes[self.scope_index].instructions,
			constants: &self.constants,
		}
	}
}