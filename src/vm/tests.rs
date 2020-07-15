use crate::types::object::Object;
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
					test_expected_object(&t.expected, e);
				}
			},
			Err(e) => {
				panic!("VM error: {}", e)
			}
		}
	}
}

fn test_expected_object(expected: &Object, actual: &Object) {
	match (&expected, &actual) {
		(Object::INTEGER(l), Object::INTEGER(r)) => {
			if l != r {
				panic!("integer object has wrong value. got: {}, want: {}", r, l)
			}
		},
		(Object::BOOLEAN(l), Object::BOOLEAN(r)) => {
			if l != r {
				panic!("boolean object has wrong value. got: {}, want: {}", r, l)
			}
		},
		_ => panic!("Unexpected comparison types. expected: {:?}, actual: {:?}", expected, actual)
	}
}