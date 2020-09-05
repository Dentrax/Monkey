use crate::types::object::{Object, OBJ_NULL};
use crate::parser::parser::Parser;
use crate::lexer::lexer::Lexer;
use crate::compiler::compiler::Compiler;
use crate::ast::ast::Node::PROGRAM;
use crate::ast::ast::*;
use crate::ast::ast::Expression::ARRAY;
use crate::vm::vm::VM;
use crate::types::array::Array;
use crate::ast::ast::HashLiteral;
use crate::types::hashable::{Hash, Hashable};

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

struct VMTestCase<'a> {
	input: &'a str,
	expected: Object,
}

#[test]
fn test_integer_arithmetic() {
	let tests = vec![
		VMTestCase {
			input: "1",
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: "2",
			expected: Object::INTEGER(2),
		},
		VMTestCase {
			input: "1 + 2",
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: "1 - 2",
			expected: Object::INTEGER(-1),
		},
		VMTestCase {
			input: "4 / 2",
			expected: Object::INTEGER(2),
		},
		VMTestCase {
			input: "50 / 2 * 2 + 10 - 5",
			expected: Object::INTEGER(55),
		},
		VMTestCase {
			input: "5 * (2 + 10)",
			expected: Object::INTEGER(60),
		},
		VMTestCase {
			input: "5 + 5 + 5 + 5 - 10",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "2 * 2 * 2 * 2 * 2",
			expected: Object::INTEGER(32),
		},
		VMTestCase {
			input: "5 * 2 + 10",
			expected: Object::INTEGER(20),
		},
		VMTestCase {
			input: "5 + 2 * 10",
			expected: Object::INTEGER(25),
		},
		VMTestCase {
			input: "5 * (2 + 10)",
			expected: Object::INTEGER(60),
		},
		VMTestCase {
			input: "-5",
			expected: Object::INTEGER(-5),
		},
		VMTestCase {
			input: "-10",
			expected: Object::INTEGER(-10),
		},
		VMTestCase {
			input: "-50 + 100 + -50",
			expected: Object::INTEGER(0),
		},
		VMTestCase {
			input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
			expected: Object::INTEGER(50),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_expression_boolean() {
	let tests = vec![
		VMTestCase {
			input: "true",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "false",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 < 2",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "1 > 2",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 < 1",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 > 1",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 == 1",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "1 != 1",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 == 2",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "1 != 2",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "true == true",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "false == false",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "true == false",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "true != false",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "false != true",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "(1 < 2) == true",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "(1 < 2) == false",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "(1 > 2) == true",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "(1 > 2) == false",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "!true",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "!false",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "!7",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "!!true",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "!!false",
			expected: Object::BOOLEAN(false),
		},
		VMTestCase {
			input: "!!7",
			expected: Object::BOOLEAN(true),
		},
		VMTestCase {
			input: "!(if (false) { 5; })",
			expected: Object::BOOLEAN(true),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_expression_string() {
	let tests = vec![
		VMTestCase {
			input: "\"string\"",
			expected: Object::STRING(String::from("string")),
		},
		VMTestCase {
			input: "\"str\" + \"ing\"",
			expected: Object::STRING(String::from("string")),
		},
		VMTestCase {
			input: "\"str\" + \"ing\" + \"s\"",
			expected: Object::STRING(String::from("strings")),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_expression_index() {
	let tests = vec![
		VMTestCase {
			input: "[1, 2, 3][1]",
			expected: Object::INTEGER(2),
		},
		VMTestCase {
			input: "[1, 2, 3][0 + 2]",
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: "[[1, 1, 1]][0][0]",
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: "[][0]",
			expected: Object::NULL,
		},
		VMTestCase {
			input: "[1, 2, 3][99]",
			expected: Object::NULL,
		},
		VMTestCase {
			input: "[1][-1]",
			expected: Object::NULL,
		},
		VMTestCase {
			input: "{1: 1, 2: 2}[1]",
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: "{1: 1, 2: 2}[2]",
			expected: Object::INTEGER(2),
		},
		VMTestCase {
			input: "{1: 1}[0]",
			expected: Object::NULL,
		},
		VMTestCase {
			input: "{}[0]",
			expected: Object::NULL,
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_literal_array() {
	let tests = vec![
		VMTestCase {
			input: "[]",
			expected: Object::ARRAY(Array { elements: vec![] }),
		},
		VMTestCase {
			input: "[1, 2, 3]",
			expected: Object::ARRAY(Array { elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)] }),
		},
		VMTestCase {
			input: "[1 + 2, 3 * 4, 5 + 6]",
			expected: Object::ARRAY(Array { elements: vec![Object::INTEGER(3), Object::INTEGER(12), Object::INTEGER(11)] }),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_literal_hash() {
	let tests = vec![
		VMTestCase {
			input: "{}",
			expected: Object::HASH(Hash { pairs: hashmap![] }),
		},
		VMTestCase {
			input: "{1: 2, 2: 3}",
			expected: Object::HASH(Hash { pairs: hashmap![
				Hashable::INTEGER(1) => Object::INTEGER(2),
				Hashable::INTEGER(2) => Object::INTEGER(3)
			]})
		},
		VMTestCase {
			input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
			expected: Object::HASH(Hash { pairs: hashmap![
				Hashable::INTEGER(2) => Object::INTEGER(4),
				Hashable::INTEGER(6) => Object::INTEGER(16)
			]}),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
	let tests = vec![
		VMTestCase {
			input: "if (true) { 10 }",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "if (true) { 10 } else { 20 }",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "if (false) { 10 } else { 20 }",
			expected: Object::INTEGER(20),
		},
		VMTestCase {
			input: "if (1) { 10 }",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "if (1 < 2) { 10 }",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "if (1 < 2) { 10 } else { 20 }",
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: "if (1 > 2) { 10 } else { 20 }",
			expected: Object::INTEGER(20),
		},
		VMTestCase {
			input: "if (1 > 2) { 10 }",
			expected: OBJ_NULL,
		},
		VMTestCase {
			input: "if (false) { 10 }",
			expected: OBJ_NULL,
		},
		VMTestCase {
			input: "if ((if (false) { 10 })) { 10 } else { 20 }",
			expected: Object::INTEGER(20),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_call_without_args() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let fivePlusTen = fn() { 5 + 10; };
			fivePlusTen();
			"#,
			expected: Object::INTEGER(15),
		},
		VMTestCase {
			input: r#"
			let one = fn() { 1; };
			let two = fn() { 2; };
			one() + two()
			"#,
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: r#"
			let a = fn() { 1 };
			let b = fn() { a() + 1 };
			let c = fn() { b() + 1 };
			c();
			"#,
			expected: Object::INTEGER(3),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_call_with_returns() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let earlyExit = fn() { return 99; 100; };
			earlyExit();
			"#,
			expected: Object::INTEGER(99),
		},
		VMTestCase {
			input: r#"
			let earlyExit = fn() { return 99; return 100; };
			earlyExit();
			"#,
			expected: Object::INTEGER(99),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_call_without_return_value() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let noReturn = fn() { };
			noReturn();
			"#,
			expected: OBJ_NULL,
		},
		VMTestCase {
			input: r#"
			let noReturn = fn() { };
			let noReturnTwo = fn() { noReturn(); };
			noReturn();
			noReturnTwo();
			"#,
			expected: OBJ_NULL,
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_first_class() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let returnsOne = fn() { 1; };
			let returnsOneReturner = fn() { returnsOne; };
			returnsOneReturner()();
			"#,
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: r#"
			let returnsOneReturner = fn() {
				let returnsOne = fn() { 1; };
				returnsOne;
			};
			returnsOneReturner()();
			"#,
			expected: Object::INTEGER(1),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_calls_without_bindings() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let one = fn() { let one = 1; one };
			one();
			"#,
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: r#"
			let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
			oneAndTwo();
			"#,
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: r#"
			let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
			let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
			oneAndTwo() + threeAndFour();
			"#,
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: r#"
			let firstFoobar = fn() { let foobar = 50; foobar; };
			let secondFoobar = fn() { let foobar = 100; foobar; };
			firstFoobar() + secondFoobar();
			"#,
			expected: Object::INTEGER(150),
		},
		VMTestCase {
			input: r#"
			let globalSeed = 50;
			let minusOne = fn() {
			let num = 1;
				globalSeed - num;
			}
			let minusTwo = fn() {
				let num = 2;
				globalSeed - num;
			}
			minusOne() + minusTwo();
			"#,
			expected: Object::INTEGER(97),
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_functions_calls_with_args_and_bindings() {
	let tests = vec![
		VMTestCase {
			input: r#"
			let identity = fn(a) { a; };
			identity(4);
			"#,
			expected: Object::INTEGER(4),
		},
		VMTestCase {
			input: r#"
			let sum = fn(a, b) { a + b; };
			sum(1, 2);
			"#,
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: r#"
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			sum(1, 2);
			"#,
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: r#"
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			sum(1, 2) + sum(3, 4);
			"#,
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: r#"
			let sum = fn(a, b) {
				let c = a + b;
				c;
			};
			let outer = fn() {
				sum(1, 2) + sum(3, 4);
			};
			outer();
			"#,
			expected: Object::INTEGER(10),
		},
		VMTestCase {
			input: r#"
			let globalNum = 10;
			let sum = fn(a, b) {
				let c = a + b;
				c + globalNum;
			};
			let outer = fn() {
				sum(1, 2) + sum(3, 4) + globalNum;
			};
			outer() + globalNum;
			"#,
			expected: Object::INTEGER(50),
		},
	];

	run_vm_tests(tests);
}

#[test]
#[should_panic(expected = "wrong number of arguments: want=0, got=1")]
fn test_functions_calls_with_wrong_args() {
	let tests = vec![
		VMTestCase {
			input: "fn() { 1; }(1);",
			expected: OBJ_NULL,
		},
		VMTestCase {
			input: "fn(a) { a; }();",
			expected: OBJ_NULL,
		},
		VMTestCase {
			input: "fn(a, b) { a + b; }(1);",
			expected: OBJ_NULL,
		},
	];

	run_vm_tests(tests);
}

#[test]
fn test_global_let_statements() {
	let tests = vec![
		VMTestCase {
			input: "let one = 1; one",
			expected: Object::INTEGER(1),
		},
		VMTestCase {
			input: "let one = 1; let two = 2; one + two",
			expected: Object::INTEGER(3),
		},
		VMTestCase {
			input: "let one = 1; let two = one + one; one + two",
			expected: Object::INTEGER(3),
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
			}
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
		}
		(Object::BOOLEAN(l), Object::BOOLEAN(r)) => {
			if l != r {
				panic!("boolean object has wrong value for `{}`. got: {}, want: {}", input, r, l)
			}
		}
		(Object::STRING(l), Object::STRING(r)) => {
			if l != r {
				panic!("string object has wrong value for `{}`. got: {}, want: {}", input, r, l)
			}
		}
		(Object::ARRAY(l), Object::ARRAY(r)) => {
			if l != r {
				panic!("array object has wrong value for `{:#?}`. got: {:#?}, want: {:#?}", input, r, l)
			}
		}
		(Object::HASH(l), Object::HASH(r)) => {
			if l != r {
				panic!("hash object has wrong value for `{:#?}`. got: {:?}, want: {:?}", input, r, l)
			}
		}
		(Object::NULL, a) => {
			if !a.is_null() {
				panic!("null object is not null value for `{}`. got: {}, want: NULL", input, a.to_string())
			}
		}
		(a, Object::NULL) => {
			if !a.is_null() {
				panic!("null object received for `{}`. got: NULL, want: {}", input, a.to_string())
			}
		}
		_ => panic!("Unexpected comparison types for `{}`. expected: {:?}, actual: {:?}", input, expected, actual)
	}
}