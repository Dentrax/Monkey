use crate::ast::ast::*;
use crate::lexer::lexer::*;
use crate::parser::parser::*;
use crate::eval::eval::*;
use crate::types::object::*;
use crate::types::array::*;
use crate::types::hashable::*;
use crate::types::env::*;

use std::rc::Rc;
use std::cell::RefCell;

fn test_eval(input: &str) -> Result<Object, EvalError> {
	let lexer = Lexer::new(input.to_owned());
	let mut parser = Parser::new(lexer);
	let mut evaluator = Evaluator::new();

	let (actual, errs) = parser.parse();

	if errs.len() == 0 {
		return evaluator.eval(Node::PROGRAM(actual));
	}

	panic!("fail!");
}

#[test]
fn test_eval_expression_integer() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "7",
			expected: 7,
		},
		Test {
			input: "-7",
			expected: -7,
		},
		Test {
			input: "15",
			expected: 15,
		},
		Test {
			input: "-15",
			expected: -15,
		},
		Test {
			input: "5 + 5 + 5 + 5 - 10",
			expected: 10,
		},
		Test {
			input: "2 * 2 * 2 * 2 * 2",
			expected: 32,
		},
		Test {
			input: "-50 + 100 + -50",
			expected: 0,
		},
		Test {
			input: "5 * 2 + 10",
			expected: 20,
		},
		Test {
			input: "5 + 2 * 10",
			expected: 25,
		},
		Test {
			input: "20 + 2 * -10",
			expected: 0,
		},
		Test {
			input: "50 / 2 * 2 + 10",
			expected: 60,
		},
		Test {
			input: "2 * (5 + 10)",
			expected: 30,
		},
		Test {
			input: "3 * 3 * 3 + 10",
			expected: 37,
		},
		Test {
			input: "3 * (3 * 3) + 10",
			expected: 37,
		},
		Test {
			input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
			expected: 50,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_expression_boolean() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: "true",
			expected: true,
		},
		Test {
			input: "false",
			expected: false,
		},
		Test {
			input: "1 < 2",
			expected: true,
		},
		Test {
			input: "1 > 2",
			expected: false,
		},
		Test {
			input: "1 < 1",
			expected: false,
		},
		Test {
			input: "1 > 1",
			expected: false,
		},
		Test {
			input: "1 == 1",
			expected: true,
		},
		Test {
			input: "1 != 1",
			expected: false,
		},
		Test {
			input: "1 == 2",
			expected: false,
		},
		Test {
			input: "1 != 2",
			expected: true,
		},
		Test {
			input: "true == true",
			expected: true,
		},
		Test {
			input: "false == false",
			expected: true,
		},
		Test {
			input: "true == false",
			expected: false,
		},
		Test {
			input: "true != false",
			expected: true,
		},
		Test {
			input: "false != true",
			expected: true,
		},
		Test {
			input: "(1 < 2) == true",
			expected: true,
		},
		Test {
			input: "(1 < 2) == false",
			expected: false,
		},
		Test {
			input: "(1 > 2) == true",
			expected: false,
		},
		Test {
			input: "(1 > 2) == false",
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_expression_string() {
	struct Test<'a> {
		input: &'a str,
		expected: String,
	}

	let tests = vec![
		Test {
			input: r#""Hello World!""#,
			expected: String::from("Hello World!"),
		},
		Test {
			input: r#""Hello" + " " + "World!""#,
			expected: String::from("Hello World!"),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::STRING(test.expected));
	}
}

#[test]
fn test_eval_expression_string_equality() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: r#""foo" == "foo""#,
			expected: true,
		},
		Test {
			input: r#""foo" != "foo""#,
			expected: false,
		},
		Test {
			input: r#""foo" == "bar""#,
			expected: false,
		},
		Test {
			input: r#""foo" != "bar""#,
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_expression_if_else() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "if (true) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (false) { 10 }",
			expected: OBJ_NULL,
		},
		Test {
			input: "if (1) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (1 < 2) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (1 > 2) { 10 }",
			expected: OBJ_NULL,
		},
		Test {
			input: "if (1 > 2) { 10 } else { 20 }",
			expected: Object::INTEGER(20),
		},
		Test {
			input: "if (1 < 2) { 10 } else { 20 }",
			expected: Object::INTEGER(10),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_expression_array_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "[1, 2, 3][0]",
			expected: Object::INTEGER(1),
		},
		Test {
			input: "[1, 2, 3][1]",
			expected: Object::INTEGER(2),
		},
		Test {
			input: "[1, 2, 3][2]",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let i = 0; [1][i];",
			expected: Object::INTEGER(1),
		},
		Test {
			input: "[1, 2, 3][1 + 1];",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let myArray = [1, 2, 3]; myArray[2];",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
			expected: Object::INTEGER(6),
		},
		Test {
			input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
			expected: Object::INTEGER(2),
		},
		Test {
			input: "[1, 2, 3][3]",
			expected: OBJ_NULL,
		},
		Test {
			input: "[1, 2, 3][-1]",
			expected: OBJ_NULL,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_return() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "return 10;",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "return 11; 9;",
			expected: Object::INTEGER(11),
		},
		Test {
			input: "return 2 * 6; 9;",
			expected: Object::INTEGER(12),
		},
		Test {
			input: "9; return 2 * 7; 9;",
			expected: Object::INTEGER(14),
		},
		Test {
			input: "if (2 > 1) { if (3 > 1) { return 7; } return 0; }",
			expected: Object::INTEGER(7),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_operator_bang() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: "!true",
			expected: false,
		},
		Test {
			input: "!false",
			expected: true,
		},
		Test {
			input: "!7",
			expected: false,
		},
		Test {
			input: "!!true",
			expected: true,
		},
		Test {
			input: "!!false",
			expected: false,
		},
		Test {
			input: "!!7",
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_handle_error() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "7 + true;",
			expected: "type mismatch: INTEGER + BOOLEAN",
		},
		Test {
			input: "7 + true; 7;",
			expected: "type mismatch: INTEGER + BOOLEAN",
		},
		Test {
			input: "-true",
			expected: "unknown operator: -BOOLEAN",
		},
		Test {
			input: "true + false;",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "7; true + false; 7",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "if (10 > 1) { true + false; }",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "foobar",
			expected: "identifier not found: foobar",
		},
		Test {
			input: "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); x",
			expected: "identifier not found: x",
		},
		Test {
			input: r#""Hello" - "World""#,
			expected: "unknown operator: STRING - STRING",
		},
		Test {
			input: "len(1)",
			expected: "argument to 'len' not supported, got INTEGER",
		},
		Test {
			input: r#"len("one", "two")"#,
			expected: "wrong number of arguments. got=2, want=1",
		},
		Test {
			input: r#"{"name": "Monkey"}[fn(x) { x }];"#,
			expected: "unusable as hash key: FUNCTION",
		},
	];

	for test in tests {
		match test_eval(test.input) {
			Ok(e) => {
				panic!("expected error: {}, got: {} instead, for: {}", test.expected, e, test.input)
			}
			Err(e) => {
				assert_eq!(e.to_string(), test.expected);
			}
		}
	}
}

#[test]
fn test_eval_statement_let() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "let a = 7; a;",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "let a = 7 * 7; a;",
			expected: Object::INTEGER(49),
		},
		Test {
			input: "let a = 7; let b = a; b;",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "let a = 7; let b = a; let c = a + b + 7; c;",
			expected: Object::INTEGER(21),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "fn(x) { x + 2; };",
			expected: Object::FUNCTION(Function {
				parameters: vec![String::from("x")],
				body: BlockStatement {
					statements: vec![
						Statement::EXPRESSION(
							Expression::INFIX(
								InfixExpression {
									left: Box::new(Expression::IDENT(String::from("x"))),
									operator: InfixType::PLUS,
									right: Box::new(Expression::LITERAL(Literal::INT(2))),
								}
							)
						)
					]
				},
				env: Rc::new(RefCell::new(Environment::new())),
			}),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_call() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "let identity = fn(x) { x; }; identity(7);",
			expected: 7,
		},
		Test {
			input: "let identity = fn(x) { return x; }; identity(15);",
			expected: 15,
		},
		Test {
			input: "let double = fn(x) { x * 34; }; double(2);",
			expected: 68,
		},
		Test {
			input: "let add = fn(x, y) { x + y; }; add(7, 7);",
			expected: 14,
		},
		Test {
			input: "let add = fn(x, y) { x + y; }; add(1 + 3, add(5, 7));",
			expected: 16,
		},
		Test {
			input: "fn(x) { x; } (35)",
			expected: 35,
		},
		Test {
			input: "let add = fn(a, b) { a + b }; let sub = fn(a, b) { a - b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(2, 2, add);",
			expected: 4,
		},
		Test {
			input: "let add = fn(a, b) { a + b }; let sub = fn(a, b) { a - b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(10, 2, sub);",
			expected: 8,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_statement_function_call_closures() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(14); addTwo(35);",
			expected: 49,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_statement_function_builtin() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"len("")"#,
			expected: Object::INTEGER(0),
		},
		Test {
			input: r#"len("four")"#,
			expected: Object::INTEGER(4),
		},
		Test {
			input: r#"len("hello world")"#,
			expected: Object::INTEGER(11),
		},
		Test {
			input: "len([])",
			expected: Object::INTEGER(0),
		},
		Test {
			input: "len([1, 2, 3])",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "first([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "first([7, 11, 13])",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "last([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "last([7, 11, 13])",
			expected: Object::INTEGER(13),
		},
		Test {
			input: "rest([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "rest([7, 11, 13])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(11), Object::INTEGER(13)]}),
		},
		Test {
			input: "reverse([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "reverse([7, 11, 13])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(13), Object::INTEGER(11), Object::INTEGER(7)]}),
		},
		Test {
			input: r#"reverse("Hello World!")"#,
			expected: Object::STRING(String::from("!dlroW olleH")),
		},
		Test {
			input: "push(1, [])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1)]}),
		},
		Test {
			input: "push(3, [1, 2])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)]}),
		},
		Test {
			input: "push([1, 2], 3)",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)]}),
		},
		Test {
			input: "push([3, 4], [1, 2])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::ARRAY(Array{elements: vec![Object::INTEGER(3), Object::INTEGER(4)]})]}),
		},
		Test {
			input: r#"puts("Hello World!")"#,
			expected: OBJ_NULL
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_builtin_integration() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"
			let map = fn(arr, f) {
				let iter = fn(arr, accumulated) {
					if (len(arr) == 0) {
						accumulated
					} else {
						iter(rest(arr), push(accumulated, f(first(arr))));
					}
				};
				iter(arr, []);
			};
			let a = [1, 2, 3, 4];
			let double = fn(x) { x * 2 };
			map(a, double);
			"#,
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(2), Object::INTEGER(4), Object::INTEGER(6), Object::INTEGER(8)]}),
		},
		Test {
			input: r#"
			let reduce = fn(arr, initial, f) {
				let iter = fn(arr, result) {
					if (len(arr) == 0) {
						result
					} else {
						iter(rest(arr), f(result, first(arr)));
					}
				};
				iter(arr, initial);
			};
			let sum = fn(arr) {
				reduce(arr, 0, fn(initial, el) { initial + el });
			};
			sum([1, 2, 3, 4, 5]);
			"#,
			expected: Object::INTEGER(15),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_array() {
	struct Test<'a> {
		input: &'a str,
		expected: Array,
	}

	let tests = vec![
		Test {
			input: "[1, 2 * 2, 3 + 3]",
			expected: Array{elements: vec![Object::INTEGER(1), Object::INTEGER(4), Object::INTEGER(6)]},
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::ARRAY(test.expected));
	}
}

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

#[test]
fn test_eval_statement_function_hash() {
	struct Test<'a> {
		input: &'a str,
		expected: Hash,
	}

	let tests = vec![
		Test {
			input: r#"
			let two = "two";
			{
				"one": 10 - 9,
				two: 1 + 1,
				"thr" + "ee": 6 / 2,
				4: 4,
				true: 5,
				false: 6
			}
			"#,
			expected: Hash{ pairs: hashmap![
				Hashable::STRING(String::from("two")) => Object::INTEGER(2),
				Hashable::STRING(String::from("one")) => Object::INTEGER(1),
				Hashable::STRING(String::from("three")) => Object::INTEGER(3),
				Hashable::BOOLEAN(true) => Object::INTEGER(5),
				Hashable::BOOLEAN(false) => Object::INTEGER(6),
				Hashable::INTEGER(4) => Object::INTEGER(4)
			]},
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::HASH(test.expected));
	}
}

#[test]
fn test_eval_statement_function_hash_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"{"foo": 5}["foo"]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{"foo": 5}["bar"]"#,
			expected: OBJ_NULL,
		},
		Test {
			input: r#"let key = "foo"; {"foo": 5}[key]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{}["foo"]"#,
			expected: OBJ_NULL,
		},
		Test {
			input: r#"{5: 5}[5]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{true: 5}[true]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{false: 5}[false]"#,
			expected: Object::INTEGER(5),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_hash_integration() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: r#"
			let people = [{"name": "Alice", "age": 24}, {"name": "Anna", "age": 28}];
			let getAge = fn(person) { person["name"]; };
			return getAge(people[0]) + getAge(people[1]);
			"#,
			expected: "AliceAnna",
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::STRING(String::from(test.expected)));
	}
}