use crate::types::object::{Object, OBJ_NULL};
use crate::parser::parser::Parser;
use crate::lexer::lexer::Lexer;
use crate::compiler::compiler::Compiler;
use crate::ast::ast::Node::PROGRAM;
use crate::vm::vm::VM;

struct VMTestCase<'a> {
	input: &'a str,
	expected: Object,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		VMTestCase {
			input: "1",
			expected: Object::INTEGER(1)
		},
		VMTestCase {
			input: "2",
			expected: Object::INTEGER(2)
		},
		VMTestCase {
			input: "1 + 2",
			expected: Object::INTEGER(3)
		},
		VMTestCase {
			input: "1 - 2",
			expected: Object::INTEGER(-1)
		},
		VMTestCase {
			input: "4 / 2",
			expected: Object::INTEGER(2)
		},
		VMTestCase {
			input: "50 / 2 * 2 + 10 - 5",
			expected: Object::INTEGER(55)
		},
		VMTestCase {
			input: "5 * (2 + 10)",
			expected: Object::INTEGER(60)
		},
		VMTestCase {
			input: "5 + 5 + 5 + 5 - 10",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "2 * 2 * 2 * 2 * 2",
			expected: Object::INTEGER(32)
		},
		VMTestCase {
			input: "5 * 2 + 10",
			expected: Object::INTEGER(20)
		},
		VMTestCase {
			input: "5 + 2 * 10",
			expected: Object::INTEGER(25)
		},
		VMTestCase {
			input: "5 * (2 + 10)",
			expected: Object::INTEGER(60)
		},
		VMTestCase {
			input: "-5",
			expected: Object::INTEGER(-5)
		},
		VMTestCase {
			input: "-10",
			expected: Object::INTEGER(-10)
		},
		VMTestCase {
			input: "-50 + 100 + -50",
			expected: Object::INTEGER(0)
		},
		VMTestCase {
			input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
			expected: Object::INTEGER(50)
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_expression_boolean() {
	let tests = vec![
		VMTestCase {
			input: "true",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "false",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 < 2",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "1 > 2",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 < 1",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 > 1",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 == 1",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "1 != 1",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 == 2",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "1 != 2",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "true == true",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "false == false",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "true == false",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "true != false",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "false != true",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "(1 < 2) == true",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "(1 < 2) == false",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "(1 > 2) == true",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "(1 > 2) == false",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "!true",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "!false",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "!7",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "!!true",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "!!false",
			expected: Object::BOOLEAN(false)
		},
		VMTestCase {
			input: "!!7",
			expected: Object::BOOLEAN(true)
		},
		VMTestCase {
			input: "!(if (false) { 5; })",
			expected: Object::BOOLEAN(true)
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
	let tests = vec![
		VMTestCase {
			input: "if (true) { 10 }",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "if (true) { 10 } else { 20 }",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "if (false) { 10 } else { 20 }",
			expected: Object::INTEGER(20)
		},
		VMTestCase {
			input: "if (1) { 10 }",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "if (1 < 2) { 10 }",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "if (1 < 2) { 10 } else { 20 }",
			expected: Object::INTEGER(10)
		},
		VMTestCase {
			input: "if (1 > 2) { 10 } else { 20 }",
			expected: Object::INTEGER(20)
		},
		VMTestCase {
			input: "if (1 > 2) { 10 }",
			expected: OBJ_NULL
		},
		VMTestCase {
			input: "if (false) { 10 }",
			expected: OBJ_NULL
		},
		VMTestCase {
			input: "if ((if (false) { 10 })) { 10 } else { 20 }",
			expected: Object::INTEGER(20)
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		VMTestCase {
			input: "let one = 1; one",
			expected: Object::INTEGER(1)
		},
		VMTestCase {
			input: "let one = 1; let two = 2; one + two",
			expected: Object::INTEGER(3)
		},
		VMTestCase {
			input: "let one = 1; let two = one + one; one + two",
			expected: Object::INTEGER(3)
		},
	];

	run_vm_tests(tests);
}

fn run_vm_tests(tests: Vec<VMTestCase>) {
	for t in tests {
		let (program, errors) = Parser::new(Lexer::new(t.input.to_owned())).parse();

		assert_eq!(errors.len(), 0);

		let mut compiler = Compiler::new();

		match compiler.compile(PROGRAM(program)) {
			Ok(o) => {
				let bytecode = compiler.bytecode();

				let mut vm = VM::new(&bytecode);

				vm.run();

				if let Some(e) = vm.last_popped_stack_elem() {
					test_expected_object(&t.input, &t.expected, e);
				}
			},
			Err(e) => {
				panic!("VM error: {}", e)
			}
		}
	}
}

fn test_expected_object(input: &str, expected: &Object, actual: &Object) {
	match (&expected, &actual) {
		(Object::INTEGER(l), Object::INTEGER(r)) => {
			if l != r {
				panic!("integer object has wrong value for `{}`. got: {}, want: {}", input, r, l)
			}
		},
		(Object::BOOLEAN(l), Object::BOOLEAN(r)) => {
			if l != r {
				panic!("boolean object has wrong value for `{}`. got: {}, want: {}", input, r, l)
			}
		},
		(Object::NULL, a) => {
			if !a.is_null() {
				panic!("null object is not null value for `{}`. got: {}, want: NULL", input, a.to_string())
			}
		}
		_ => panic!("Unexpected comparison types for `{}`. expected: {:?}, actual: {:?}", input, expected, actual)
	}
}