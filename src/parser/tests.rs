use crate::ast::ast::*;
use crate::lexer::lexer::*;
use crate::parser::parser::*;

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

#[test]
fn test_ast_operator_precedence_string() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "-a * b",
			expected: "((-a) * b)",
		},
		Test {
			input: "!-a",
			expected: "(!(-a))",
		},
		Test {
			input: "a + b + c",
			expected: "((a + b) + c)",
		},
		Test {
			input: "a + b - c",
			expected: "((a + b) - c)",
		},
		Test {
			input: "a * b * c",
			expected: "((a * b) * c)",
		},
		Test {
			input: "a * b / c",
			expected: "((a * b) / c)",
		},
		Test {
			input: "a + b / c",
			expected: "(a + (b / c))",
		},
		Test {
			input: "a + b * c + d / e - f",
			expected: "(((a + (b * c)) + (d / e)) - f)",
		},
		Test {
			input: "3 + 4; -5 * 5",
			expected: "(3 + 4)((-5) * 5)",
		},
		Test {
			input: "5 > 4 == 3 < 4",
			expected: "((5 > 4) == (3 < 4))",
		},
		Test {
			input: "5 < 4 != 3 > 4",
			expected: "((5 < 4) != (3 > 4))",
		},
		Test {
			input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
			expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.to_string(), test.expected);
	}
}

#[test]
fn test_ast_operator_precedence_group_expression_string() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "1 + (2 + 3) + 4",
			expected: "((1 + (2 + 3)) + 4)",
		},
		Test {
			input: "(5 + 5) * 2",
			expected: "((5 + 5) * 2)",
		},
		Test {
			input: "2 / (5 + 5)",
			expected: "(2 / (5 + 5))",
		},
		Test {
			input: "-(5 + 5)",
			expected: "(-(5 + 5))",
		},
		Test {
			input: "!(true == true)",
			expected: "(!(true == true))",
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.to_string(), test.expected);
	}
}

#[test]
fn test_ast_operator_precedence_call() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "a + add(b * c) + d",
			expected: "((a + add((b * c))) + d)",
		},
		Test {
			input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		},
		Test {
			input: "add(a + b + c * d / f + g)",
			expected: "add((((a + b) + ((c * d) / f)) + g))",
		},
		Test {
			input: "a * [1, 2, 3, 4][b * c] * d",
			expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
		},
		Test {
			input: "add(a * b[2], b[1], 2 * [1, 2][1])",
			expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.to_string(), test.expected);
	}
}

#[test]
fn test_parse_statement_let() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "let x = 7;",
			expected: Statement::LET(LetStatement { name: String::from("x"), value: Expression::LITERAL(Literal::INT(7)) }),
		},
		Test {
			input: "let y = true;",
			expected: Statement::LET(LetStatement { name: String::from("y"), value: Expression::LITERAL(Literal::BOOL(true)) }),
		},
		Test {
			input: "let z = y;",
			expected: Statement::LET(LetStatement { name: String::from("z"), value: Expression::IDENT(String::from("y")) }),
		},
		Test {
			input: "let w = \"furkan\";",
			expected: Statement::LET(LetStatement { name: String::from("w"), value: Expression::LITERAL(Literal::STRING(String::from("furkan"))) }),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_return() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "return 15;",
			expected: Statement::RETURN(ReturnStatement { value: Expression::LITERAL(Literal::INT(15)) }),
		},
		Test {
			input: "return x;",
			expected: Statement::RETURN(ReturnStatement { value: Expression::IDENT(String::from("x")) }),
		},
		Test {
			input: "return \"string\";",
			expected: Statement::RETURN(ReturnStatement { value: Expression::LITERAL(Literal::STRING(String::from("string"))) }),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_expression() {
	let input = "f;u;r;k;a;n;";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::IDENT(String::from("f"))),
		Statement::EXPRESSION(Expression::IDENT(String::from("u"))),
		Statement::EXPRESSION(Expression::IDENT(String::from("r"))),
		Statement::EXPRESSION(Expression::IDENT(String::from("k"))),
		Statement::EXPRESSION(Expression::IDENT(String::from("a"))),
		Statement::EXPRESSION(Expression::IDENT(String::from("n"))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_integer() {
	let input = "7;15;34;";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::LITERAL(Literal::INT(7))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::INT(15))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::INT(34))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_string() {
	let input = r#""helloworld!";"hello world!";"hello+world!";"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("helloworld!")))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("hello world!")))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("hello+world!")))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_boolean() {
	let input = "true;false;";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::LITERAL(Literal::BOOL(true))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::BOOL(false))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_prefix() {
	let input = "!7;-15;!true;!false";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::INT(7))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::BOOL(true))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_infix() {
	let input = r#"7 + 15;
7 - 15;
7 * 15;
7 / 15;
7 > 15;
7 < 15;
7 == 15;
7 != 15;
true == true;
true != false;
false == false;"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::PLUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MULTIPLICATION, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::DIVISION, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::GT, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::LT, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::NEQ, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(true))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::BOOL(true))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(true))), operator: InfixType::NEQ, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(false))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_if() {
	let input = "if (x < y) { z }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::IF(
				IfExpression {
					condition: Box::new(
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::IDENT(String::from("x"))),
								operator: InfixType::LT,
								right: Box::new(Expression::IDENT(String::from("y"))),
							}
						)
					),
					consequence: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("z")))
						]
					},
					alternative: None,
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_if_else() {
	let input = "if (x < y) { z } else { w }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::IF(
				IfExpression {
					condition: Box::new(
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::IDENT(String::from("x"))),
								operator: InfixType::LT,
								right: Box::new(Expression::IDENT(String::from("y"))),
							}
						)
					),
					consequence: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("z")))
						]
					},
					alternative: Some(BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("w"))),
						]
					}),
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_function() {
	let input = "fn(x, y) { z + w; }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::FUNCTION(
				FunctionLiteral {
					parameters: vec![
						String::from("x"),
						String::from("y"),
					],
					body: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::INFIX(
								InfixExpression {
									left: Box::new(Expression::IDENT(String::from("z"))),
									operator: InfixType::PLUS,
									right: Box::new(Expression::IDENT(String::from("w"))),
								}
							)),
						],
					},
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_function_parameters() {
	struct Test<'a> {
		input: &'a str,
		expected: Vec<String>,
	}

	let tests = vec![
		Test {
			input: "fn() {};",
			expected: vec![],
		},
		Test {
			input: "fn(x) {};",
			expected: vec![String::from("x")],
		},
		Test {
			input: "fn(x, y, z) {};",
			expected: vec![String::from("x"), String::from("y"), String::from("z")],
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(Statement::EXPRESSION(Expression::FUNCTION(literal))) = actual.statements.first() {
			assert_eq!(literal.parameters, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_expression_call() {
	let input = "add(1, 2 * 3, 4 + 5);";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::CALL(
				CallExpression {
					function: Box::new(
						Expression::IDENT(String::from("add"))
					),
					arguments: vec![
						Expression::LITERAL(
							Literal::INT(1)
						),
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::LITERAL(Literal::INT(2))),
								operator: InfixType::MULTIPLICATION,
								right: Box::new(Expression::LITERAL(Literal::INT(3))),
							}
						),
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::LITERAL(Literal::INT(4))),
								operator: InfixType::PLUS,
								right: Box::new(Expression::LITERAL(Literal::INT(5))),
							}
						)
					],
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_array() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "[1, 2 * 2, 3 + 3]",
			expected: Statement::EXPRESSION(Expression::ARRAY(ArrayLiteral{
				elements: vec![
					Expression::LITERAL(Literal::INT(1)),
					Expression::INFIX(InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(2))),
						operator: InfixType::MULTIPLICATION,
						right: Box::new(Expression::LITERAL(Literal::INT(2))),
					}),
					Expression::INFIX(InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(3))),
						operator: InfixType::PLUS,
						right: Box::new(Expression::LITERAL(Literal::INT(3))),
					}),
				]
			})),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_expression_map() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "{}",
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: hashmap![]})),
		},
		Test {
			input: r#"{"one": 1, "two": 2, "ten": 10}"#,
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: hashmap![
				Expression::LITERAL(Literal::STRING(String::from("one"))) => Expression::LITERAL(Literal::INT(1)),
				Expression::LITERAL(Literal::STRING(String::from("two"))) => Expression::LITERAL(Literal::INT(2)),
				Expression::LITERAL(Literal::STRING(String::from("ten"))) => Expression::LITERAL(Literal::INT(10))
			]})),
		},
		Test {
			input: r#"{"one": 0 + 1, "two": 10 - 8, "ten": 50 / 5}"#,
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: hashmap![
				Expression::LITERAL(Literal::STRING(String::from("one"))) => Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(0))),
						operator: InfixType::PLUS,
						right: Box::new(Expression::LITERAL(Literal::INT(1))),
					}
				),
				Expression::LITERAL(Literal::STRING(String::from("two"))) => Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(10))),
						operator: InfixType::MINUS,
						right: Box::new(Expression::LITERAL(Literal::INT(8))),
					}
				),
				Expression::LITERAL(Literal::STRING(String::from("ten"))) => Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(50))),
						operator: InfixType::DIVISION,
						right: Box::new(Expression::LITERAL(Literal::INT(5))),
					}
				)
			]})),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}


#[test]
fn test_parse_statement_expression_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "myArray[5 + 7]",
			expected: Statement::EXPRESSION(Expression::INDEX(IndexExpression{
				left: Box::new(Expression::IDENT(String::from("myArray"))),
				index: Box::new(Expression::INFIX(InfixExpression{
					left: Box::new(Expression::LITERAL(Literal::INT(5))),
					operator: InfixType::PLUS,
					right: Box::new(Expression::LITERAL(Literal::INT(7)))
				}))
			}))
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}