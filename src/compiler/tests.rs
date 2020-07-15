use crate::types::object::Object;
use crate::code::code::{Instructions, make, OpCodeType, IInstructions};
use crate::parser::parser::Parser;
use crate::lexer::lexer::Lexer;
use crate::compiler::compiler::{Compiler, CompilerError};
use std::fmt::Error;
use crate::ast::ast::Node::PROGRAM;

struct CompilerTestCase<'a> {
	input: &'a str,
	expectedConstants: Vec<Object>,
	expectedInstructions: Vec<Instructions>,
}

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
			},
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
			return Err(CompilerError::WRONG_INSTRUCTION_AT { at: i, want: concatted, got: actual.clone() });
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