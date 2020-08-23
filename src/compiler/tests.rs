use crate::types::object::Object;
use crate::code::code::{Instructions, make, OpCodeType, IInstructions};
use crate::parser::parser::Parser;
use crate::lexer::lexer::Lexer;
use crate::compiler::compiler::{Compiler, CompilerError};
use std::fmt::Error;
use crate::ast::ast::Node::PROGRAM;
use std::collections::HashMap;
use crate::compiler::symbol_table::{Symbol, SymbolTable, SymbolScope};

struct CompilerTestCase<'a> {
	input: &'a str,
	expectedConstants: Vec<Object>,
	expectedInstructions: Vec<Instructions>,
}

// SYMBOL-TABLE TESTS

#[test]
fn test_symbol_table_define() {
	let mut expected: HashMap<String, Symbol> = HashMap::new();
	expected.insert("a".to_string(), Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0, }, );
	expected.insert("b".to_string(), Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1, }, );

	let mut global = SymbolTable::new();

	let a = global.define("a");
	assert_eq!(&a, expected.get("a").unwrap());

	let b = global.define("b");
	assert_eq!(&b, expected.get("b").unwrap());
}

#[test]
fn test_symbol_table_resolve() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let mut expected: HashMap<String, Symbol> = HashMap::new();
	expected.insert("a".to_string(), Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0, }, );
	expected.insert("b".to_string(), Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1, }, );

	for (s, sym) in expected {
		let result = global.resolve(&*sym.name);

		assert_eq!(result.unwrap(), &sym);
	}
}

// COMPILER TESTS

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		CompilerTestCase {
			input: "1; 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 + 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::ADD, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 - 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::SUB, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 * 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::MUL, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "2 / 1",
			expectedConstants: vec![
				Object::INTEGER(2),
				Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::DIV, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "-1",
			expectedConstants: vec![
				Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::MINUS, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_expression_boolean() {
	let tests = vec![
		CompilerTestCase {
			input: "true",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "false",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::FALSE, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 > 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::GT, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 < 2",
			expectedConstants: vec![
				Object::INTEGER(2),
				Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::GT, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 == 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::EQ, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "1 != 2",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::NEQ, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "true == false",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				make(OpCodeType::FALSE, &vec![]).unwrap(),
				make(OpCodeType::EQ, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "true != false",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				make(OpCodeType::FALSE, &vec![]).unwrap(),
				make(OpCodeType::NEQ, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "!true",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				make(OpCodeType::BANG, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_conditionals() {
	let tests = vec![
		CompilerTestCase {
			input: "if (true) { 7 }; 7777;",
			expectedConstants: vec![Object::INTEGER(7), Object::INTEGER(7777)],
			expectedInstructions: vec![
				// 0000
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				// 0001
				make(OpCodeType::JMPNT, &vec![10]).unwrap(),
				// 0004
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				// 0007
				make(OpCodeType::JMP, &vec![11]).unwrap(),
				// 0010
				make(OpCodeType::NULL, &vec![]).unwrap(),
				// 0011
				make(OpCodeType::POP, &vec![]).unwrap(),
				// 0012
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				// 0015
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "if (true) { 7 } else { 15 }; 7777;",
			expectedConstants: vec![Object::INTEGER(7), Object::INTEGER(15), Object::INTEGER(7777)],
			expectedInstructions: vec![
				// 0000
				make(OpCodeType::TRUE, &vec![]).unwrap(),
				// 0001
				make(OpCodeType::JMPNT, &vec![10]).unwrap(),
				// 0004
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				// 0007
				make(OpCodeType::JMP, &vec![13]).unwrap(),
				// 0010
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				// 0013
				make(OpCodeType::POP, &vec![]).unwrap(),
				// 0014
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				// 0017
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		CompilerTestCase {
			input: "let one = 1; let two = 2;",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::GS, &vec![1]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "let one = 1; one;",
			expectedConstants: vec![Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::GG, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "let one = 1; let two = one; two",
			expectedConstants: vec![Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::GG, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![1]).unwrap(),
				make(OpCodeType::GG, &vec![1]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
	for t in tests {
		let (program, errors) = Parser::new(Lexer::new(t.input.to_owned())).parse();

		assert_eq!(errors.len(), 0);

		let mut compiler = Compiler::new();

		match compiler.compile(PROGRAM(program)) {
			Ok(o) => {
				let bytecode = compiler.bytecode();

				match test_instructions(&t.expectedInstructions, &bytecode.instructions) {
					Ok(e) => assert!(true),
					Err(e) => panic!("Error on test_instructions: {}", e)
				}

				match test_constants(&t.expectedConstants, &bytecode.constants) {
					Ok(e) => assert!(true),
					Err(e) => panic!("Error on test_constants: {}", e)
				}
			}
			Err(e) => {
				panic!("Compiler error: {}", e)
			}
		}
	}
}

fn test_instructions(expected: &Vec<Instructions>, actual: &Instructions) -> Result<(), CompilerError> {
	let concatted = concat_instructions(expected);

	if actual.len() != concatted.len() {
		return Err(CompilerError::WRONG_INSTRUCTIONS_LENGTH { want: concatted.string(), got: actual.string() });
	}

	for (i, ins) in concatted.iter().enumerate() {
		if actual[i] != *ins {
			return Err(CompilerError::WRONG_INSTRUCTION_AT { at: i, want: concatted.string(), got: actual.clone().string() });
		}
	}

	Ok(())
}

fn concat_instructions(instructions: &Vec<Instructions>) -> Instructions {
	let mut out = Instructions::new();

	for i in instructions {
		for o in i {
			out.push(*o);
		}
	}

	out
}

fn test_constants(expected: &Vec<Object>, actual: &Vec<Object>) -> Result<(), CompilerError> {
	if expected.len() != actual.len() {
		return Err(CompilerError::WRONG_NUMBER_OF_CONSTANTS { want: expected.len(), got: actual.len() });
	}

	for (lhs, rhs) in expected.iter().zip(actual) {
		match (lhs, rhs) {
			(Object::INTEGER(l), Object::INTEGER(r)) => {
				if l != r {
					return Err(CompilerError::WRONG_CONSTANTS_INTEGER_EQUALITY { want: *l, got: *r });
				}
			}
			_ => {
				return Err(CompilerError::WRONG_CONSTANTS_TYPE { want: lhs.clone(), got: rhs.clone() });
			}
		}
	}

	Ok(())
}