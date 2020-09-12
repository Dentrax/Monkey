use crate::types::object::{Object, CompiledFunction};
use crate::code::code::{Instructions, make, OpCodeType, IInstructions};
use crate::parser::parser::Parser;
use crate::lexer::lexer::Lexer;
use crate::compiler::compiler::{Compiler, CompilerError};
use std::fmt::Error;
use crate::ast::ast::Node::PROGRAM;
use std::collections::HashMap;
use crate::compiler::symbol_table::{Symbol, SymbolTable, SymbolScope};
use std::borrow::Borrow;
use std::ops::Deref;

struct CompilerTestCase<'a> {
	input: &'a str,
	expectedConstants: Vec<Object>,
	expectedInstructions: Vec<Instructions>,
}

struct SymbolTableTestCase {
	table: SymbolTable,
	expectedSymbols: Vec<Symbol>,
	expectedFreeSymbols: Vec<Symbol>,
}

// SYMBOL-TABLE TESTS

#[test]
fn test_symbol_table_define() {
	let mut expected: HashMap<String, Symbol> = HashMap::new();
	expected.insert("a".to_string(), Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0 });
	expected.insert("b".to_string(), Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1 });
	expected.insert("c".to_string(), Symbol { name: "c".to_string(), scope: SymbolScope::LOCAL, index: 0 });
	expected.insert("d".to_string(), Symbol { name: "d".to_string(), scope: SymbolScope::LOCAL, index: 1 });
	expected.insert("e".to_string(), Symbol { name: "e".to_string(), scope: SymbolScope::LOCAL, index: 0 });
	expected.insert("f".to_string(), Symbol { name: "f".to_string(), scope: SymbolScope::LOCAL, index: 1 });

	let mut global = SymbolTable::new();

	let a = global.define("a");
	assert_eq!(&a, expected.get("a").unwrap());

	let b = global.define("b");
	assert_eq!(&b, expected.get("b").unwrap());

	let mut local_first = SymbolTable::new_enclosed(global);

	let c = local_first.define("c");
	assert_eq!(&c, expected.get("c").unwrap());

	let d = local_first.define("d");
	assert_eq!(&d, expected.get("d").unwrap());

	let mut local_second = SymbolTable::new_enclosed(local_first);

	let e = local_second.define("e");
	assert_eq!(&e, expected.get("e").unwrap());

	let f = local_second.define("f");
	assert_eq!(&f, expected.get("f").unwrap());
}

#[test]
fn test_symbol_table_resolve() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let mut expected: HashMap<String, Symbol> = HashMap::new();
	expected.insert("a".to_string(), Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0 });
	expected.insert("b".to_string(), Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1 });

	for (s, sym) in expected {
		let result = global.resolve(&*sym.name);

		assert_eq!(result.unwrap(), sym);
	}
}

#[test]
fn test_symbol_table_resolve_local() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let mut local = SymbolTable::new_enclosed(global);
	local.define("c");
	local.define("d");

	let mut expected: HashMap<String, Symbol> = HashMap::new();
	expected.insert("a".to_string(), Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0 });
	expected.insert("b".to_string(), Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1 });
	expected.insert("c".to_string(), Symbol { name: "c".to_string(), scope: SymbolScope::LOCAL, index: 0 });
	expected.insert("d".to_string(), Symbol { name: "d".to_string(), scope: SymbolScope::LOCAL, index: 1 });

	for (s, sym) in expected {
		let result = local.resolve(&*sym.name);

		assert_eq!(result.unwrap(), sym);
	}
}

#[test]
fn test_symbol_table_resolve_local_nested() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let mut local_first = SymbolTable::new_enclosed(global);
	local_first.define("c");
	local_first.define("d");

	let mut local_second = SymbolTable::new_enclosed(local_first.clone());
	local_second.define("e");
	local_second.define("f");

	let tests = vec![
		SymbolTableTestCase {
			table: local_first.clone(),
			expectedSymbols: vec![
				Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0 },
				Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1 },
				Symbol { name: "c".to_string(), scope: SymbolScope::LOCAL, index: 0 },
				Symbol { name: "d".to_string(), scope: SymbolScope::LOCAL, index: 1 },
			],
			expectedFreeSymbols: vec![]
		},
		SymbolTableTestCase {
			table: local_second.clone(),
			expectedSymbols: vec![
				Symbol { name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0 },
				Symbol { name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1 },
				Symbol { name: "e".to_string(), scope: SymbolScope::LOCAL, index: 0 },
				Symbol { name: "f".to_string(), scope: SymbolScope::LOCAL, index: 1 },
			],
			expectedFreeSymbols: vec![]
		}
	];

	for mut test in tests {
		for (i, symbol) in test.expectedSymbols.iter().enumerate() {
			let result = match test.table.resolve(&*symbol.name) {
				Some(s) => s,
				None => panic!("name '{}' not resolvable", &*symbol.name)
			};

			assert_eq!(result, *symbol);
		}
	}
}

#[test]
fn test_symbol_table_resolve_builtins() {
	let mut global = SymbolTable::new();

	let expected: Vec<Symbol> = vec![
		Symbol { name: "a".to_string(), scope: SymbolScope::BUILTIN, index: 0 },
		Symbol { name: "c".to_string(), scope: SymbolScope::BUILTIN, index: 1 },
		Symbol { name: "e".to_string(), scope: SymbolScope::BUILTIN, index: 2 },
		Symbol { name: "f".to_string(), scope: SymbolScope::BUILTIN, index: 3 }
	];

	for sym in &expected {
		global.define_builtin(sym.index, sym.name.clone());
	}

	let local_first = SymbolTable::new_enclosed(global.clone());
	let local_second = SymbolTable::new_enclosed(local_first.clone());

	for mut table in vec![global, local_first, local_second] {
		for symbol in &expected {
			let result = match table.resolve(&symbol.name) {
				Some(s) => s,
				None => panic!("name '{}' not resolvable", &*symbol.name)
			};

			assert_eq!(result, *symbol);
		}
	}
}

#[test]
fn test_symbol_table_resolve_free() {
	let mut global = SymbolTable::new();
	global.define("a");
	global.define("b");

	let mut local_first = SymbolTable::new_enclosed(global.clone());
	local_first.define("c");
	local_first.define("d");

	let mut local_second = SymbolTable::new_enclosed(local_first.clone());
	local_second.define("e");
	local_second.define("f");

	let tests = vec![
		SymbolTableTestCase {
			table: local_first.clone(),
			expectedSymbols: vec![
				Symbol::new("a", SymbolScope::GLOBAL, 0),
				Symbol::new("b", SymbolScope::GLOBAL, 1),
				Symbol::new("c", SymbolScope::LOCAL, 0),
				Symbol::new("d", SymbolScope::LOCAL, 1),
			],
			expectedFreeSymbols: vec![]
		},
		SymbolTableTestCase {
			table: local_second.clone(),
			expectedSymbols: vec![
				Symbol::new("a", SymbolScope::GLOBAL, 0),
				Symbol::new("b", SymbolScope::GLOBAL, 1),
				Symbol::new("c", SymbolScope::FREE, 0),
				Symbol::new("d", SymbolScope::FREE, 1),
				Symbol::new("e", SymbolScope::LOCAL, 0),
				Symbol::new("f", SymbolScope::LOCAL, 1),
			],
			expectedFreeSymbols: vec![
				Symbol::new("c", SymbolScope::LOCAL, 0),
				Symbol::new("d", SymbolScope::LOCAL, 1),
			]
		}
	];

	for mut test in tests {
		for (i, symbol) in test.expectedSymbols.iter().enumerate() {
			let result = match test.table.resolve(&*symbol.name) {
				Some(s) => s,
				None => panic!("name '{}' not resolvable", &*symbol.name)
			};

			assert_eq!(result, *symbol);
		}

		assert_eq!(test.table.free_symbols.len(), test.expectedFreeSymbols.len());

		for (i, sym) in test.expectedFreeSymbols.iter().enumerate() {
			assert_eq!(&test.table.free_symbols[i], sym);
		}
	}
}

#[test]
fn test_symbol_table_unresolve_free() {
	let mut global = SymbolTable::new();
	global.define("a");

	let mut local_first = SymbolTable::new_enclosed(global);
	local_first.define("c");

	let mut local_second = SymbolTable::new_enclosed(local_first);
	local_second.define("e");
	local_second.define("f");

	let expected: Vec<Symbol> = vec![
		Symbol::new("a", SymbolScope::GLOBAL, 0),
		Symbol::new("c", SymbolScope::FREE, 0),
		Symbol::new("e", SymbolScope::LOCAL, 0),
		Symbol::new("f", SymbolScope::LOCAL, 1),
	];

	for sym in expected {
		match local_second.resolve(&*sym.name) {
			Some(s) => assert_eq!(sym, s),
			None => panic!("name '{}' not resolvable", &*sym.name)
		};
	}

	let unexpected: Vec<&str> = vec![
		"b",
		"d"
	];

	for name in unexpected {
		if let Some(_) = local_second.resolve(&name) {
			panic!("name {} resolved, but was expected not to", name)
		}
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
fn test_expression_string() {
	let tests = vec![
		CompilerTestCase {
			input: "\"string\"",
			expectedConstants: vec![Object::STRING(String::from("string"))],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "\"str\" + \"ing\"",
			expectedConstants: vec![Object::STRING(String::from("str")), Object::STRING(String::from("ing"))],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::ADD, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_expression_index() {
	let tests = vec![
		CompilerTestCase {
			input: "[1, 2, 3][1 + 1]",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3), Object::INTEGER(1), Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::ARR, &vec![3]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![4]).unwrap(),
				make(OpCodeType::ADD, &vec![]).unwrap(),
				make(OpCodeType::ID, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "{1: 2}[2 - 1]",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(2), Object::INTEGER(1)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::HASH, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::SUB, &vec![]).unwrap(),
				make(OpCodeType::ID, &vec![]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_literal_array() {
	let tests = vec![
		CompilerTestCase {
			input: "[]",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::ARR, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "[1, 2, 3]",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::ARR, &vec![3]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "[1 + 2, 3 - 4, 5 * 6]",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3), Object::INTEGER(4), Object::INTEGER(5), Object::INTEGER(6)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::ADD, &vec![]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::SUB, &vec![]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![4]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![5]).unwrap(),
				make(OpCodeType::MUL, &vec![]).unwrap(),
				make(OpCodeType::ARR, &vec![3]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_literal_hash() {
	let tests = vec![
		CompilerTestCase {
			input: "{}",
			expectedConstants: vec![],
			expectedInstructions: vec![
				make(OpCodeType::HASH, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "{1: 2, 3: 4, 5: 6}",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3), Object::INTEGER(4), Object::INTEGER(5), Object::INTEGER(6)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![4]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![5]).unwrap(),
				make(OpCodeType::HASH, &vec![6]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "{1: 2 + 3, 4: 5 * 6}",
			expectedConstants: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3), Object::INTEGER(4), Object::INTEGER(5), Object::INTEGER(6)],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::ADD, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![4]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![5]).unwrap(),
				make(OpCodeType::MUL, &vec![0]).unwrap(),
				make(OpCodeType::HASH, &vec![4]).unwrap(),
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
fn test_functions() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { return 5 + 10 }",
			expectedConstants: vec![
				Object::INTEGER(5),
				Object::INTEGER(10),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![2, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "fn() { 5 + 10 }",
			expectedConstants: vec![
				Object::INTEGER(5),
				Object::INTEGER(10),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![2, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "fn() { 1; 2 }",
			expectedConstants: vec![
				Object::INTEGER(1),
				Object::INTEGER(2),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::POP, &vec![]).unwrap(),
						make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![2, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "fn() { }",
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::RET, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![0, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_function_calls() {
	let tests = vec![
		CompilerTestCase {
			input: "fn() { 24 }();",
			expectedConstants: vec![
				Object::INTEGER(24),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![1, 0]).unwrap(),
				make(OpCodeType::CALL, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "let noArg = fn() { 24 }; noArg();",
			expectedConstants: vec![
				Object::INTEGER(24),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![1, 0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::GG, &vec![0]).unwrap(),
				make(OpCodeType::CALL, &vec![0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			let oneArg = fn(a) { a };
			oneArg(24);
			"#,
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1,
				}),
				Object::INTEGER(24)
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![0, 0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::GG, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CALL, &vec![1]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			let manyArg = fn(a, b, c) { a; b; c };
			manyArg(24, 25, 26);
			"#,
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::POP, &vec![]).unwrap(),
						make(OpCodeType::LG, &vec![1]).unwrap(),
						make(OpCodeType::POP, &vec![]).unwrap(),
						make(OpCodeType::LG, &vec![2]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 3,
					num_params: 3
				}),
				Object::INTEGER(24),
				Object::INTEGER(25),
				Object::INTEGER(26)
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![0, 0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::GG, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
				make(OpCodeType::CALL, &vec![3]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_compiler_scopes() {
	let mut compiler = Compiler::new();

	assert_eq!(compiler.scope_index, 0);

	let symbol_table_global = compiler.symbol_table.clone();

	compiler.emit(OpCodeType::MUL, &vec![]);

	compiler.enter_scope();

	assert_eq!(compiler.scope_index, 1);

	compiler.emit(OpCodeType::SUB, &vec![]);

	assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1);

	match &compiler.scopes[compiler.scope_index].last_instruction {
		Some(i) => assert_eq!(i.opcode, OpCodeType::SUB),
		None => panic!("last_instruction not found in scope index: {}", compiler.scope_index)
	}

	match &compiler.symbol_table.outer {
		Some(outer) => {
			if outer.as_ref() != &symbol_table_global {
				panic!("compiler did not enclose symbol table");
			}
		},
		None => panic!("compiler did not enclose symbol table"),
	}

	compiler.leave_scope();

	assert_eq!(compiler.scope_index, 0);

	if compiler.symbol_table != symbol_table_global {
		panic!("compiler did not restore symbol table");
	}
	if let Some(_) = &compiler.symbol_table.outer {
		panic!("compiler modified global symbol table incorrectly");
	}

	compiler.emit(OpCodeType::ADD, &vec![]);

	assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2);

	match &compiler.scopes[compiler.scope_index].last_instruction {
		Some(i) => assert_eq!(i.opcode, OpCodeType::ADD),
		None => panic!("last_instruction not found in scope index: {}", compiler.scope_index)
	}

	match &compiler.scopes[compiler.scope_index].prev_instruction {
		Some(i) => assert_eq!(i.opcode, OpCodeType::MUL),
		None => panic!("prev_instruction not found in scope index: {}", compiler.scope_index)
	}
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

#[test]
fn test_builtins() {
	let tests = vec![
		CompilerTestCase {
			input: r#"
			len([]);
			push([], 1);
			"#,
			expectedConstants: vec![Object::INTEGER(1)],
			expectedInstructions: vec![
				//len
				make(OpCodeType::BG, &vec![0]).unwrap(),
				make(OpCodeType::ARR, &vec![0]).unwrap(),
				make(OpCodeType::CALL, &vec![1]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
				//push
				make(OpCodeType::BG, &vec![5]).unwrap(),
				make(OpCodeType::ARR, &vec![0]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::CALL, &vec![2]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: "fn() { len([]) }",
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::BG, &vec![0]).unwrap(),
						make(OpCodeType::ARR, &vec![0]).unwrap(),
						make(OpCodeType::CALL, &vec![1]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![0, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_statement_let_scopes() {
	let tests = vec![
		CompilerTestCase {
			input: r#"
			let num = 55;
			fn() { num }
			"#,
			expectedConstants: vec![
				Object::INTEGER(55),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::GG, &vec![0]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::CL, &vec![1, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			fn() {
				let num = 55;
				num
			}
			"#,
			expectedConstants: vec![
				Object::INTEGER(55),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::LS, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![1, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			fn() {
				let a = 55;
				let b = 77;
				a + b
			}
			"#,
			expectedConstants: vec![
				Object::INTEGER(55),
				Object::INTEGER(77),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
						make(OpCodeType::LS, &vec![0]).unwrap(),
						make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
						make(OpCodeType::LS, &vec![1]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![1]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 2,
					..CompiledFunction::default()
				})
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![2, 0]).unwrap(),
				make(OpCodeType::POP, &vec![]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

#[test]
fn test_closures() {
	let tests = vec![
		CompilerTestCase {
			input: r#"
			fn(a) {
				fn(b) {
					a + b
				}
			}
			"#,
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::FREE, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1
				}),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::CL, &vec![0, 1]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1
				}),
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![1, 0]).unwrap(),
				make(OpCodeType::POP, &vec![0]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			fn(a) {
				fn(b) {
					fn(c) {
						a + b + c
					}
				}
			}
			"#,
			expectedConstants: vec![
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::FREE, &vec![0]).unwrap(),
						make(OpCodeType::FREE, &vec![1]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1
				}),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::FREE, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::CL, &vec![0, 2]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1
				}),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::CL, &vec![1, 1]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 1
				}),
			],
			expectedInstructions: vec![
				make(OpCodeType::CL, &vec![2, 0]).unwrap(),
				make(OpCodeType::POP, &vec![0]).unwrap(),
			],
		},
		CompilerTestCase {
			input: r#"
			let global = 55;
			fn() {
				let a = 66;
				fn() {
					let b = 77;
					fn() {
						let c = 88;
						global + a + b + c;
					}
				}
			}
			"#,
			expectedConstants: vec![
				Object::INTEGER(55),
				Object::INTEGER(66),
				Object::INTEGER(77),
				Object::INTEGER(88),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![3]).unwrap(),
						make(OpCodeType::LS, &vec![0]).unwrap(),
						make(OpCodeType::GG, &vec![0]).unwrap(),
						make(OpCodeType::FREE, &vec![0]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::FREE, &vec![1]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::ADD, &vec![]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 0
				}),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
						make(OpCodeType::LS, &vec![0]).unwrap(),
						make(OpCodeType::FREE, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::CL, &vec![4, 2]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 0
				}),
				Object::COMPILED_FUNCTION(CompiledFunction {
					instructions: merge_instructions(vec![
						make(OpCodeType::CONSTANT, &vec![1]).unwrap(),
						make(OpCodeType::LS, &vec![0]).unwrap(),
						make(OpCodeType::LG, &vec![0]).unwrap(),
						make(OpCodeType::CL, &vec![5, 1]).unwrap(),
						make(OpCodeType::RETV, &vec![]).unwrap(),
					]),
					num_locals: 1,
					num_params: 0
				}),
			],
			expectedInstructions: vec![
				make(OpCodeType::CONSTANT, &vec![0]).unwrap(),
				make(OpCodeType::GS, &vec![0]).unwrap(),
				make(OpCodeType::CL, &vec![6, 0]).unwrap(),
				make(OpCodeType::POP, &vec![0]).unwrap(),
			],
		},
	];

	run_compiler_tests(tests);
}

fn merge_instructions(instructions: Vec<Instructions>) -> Instructions {
	instructions.into_iter().flatten().collect::<Instructions>()
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
					Err(e) => panic!("Error on test_instructions: {}, for input: {}", e, t.input)
				}

				match test_constants(&t.expectedConstants, &bytecode.constants) {
					Ok(e) => assert!(true),
					Err(e) => panic!("Error on test_constants: {}, for input: {}", e, t.input)
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
			(Object::STRING(l), Object::STRING(r)) => {
				if l != r {
					return Err(CompilerError::WRONG_CONSTANTS_STRING_EQUALITY { want: l.clone(), got: r.clone() });
				}
			}
			(Object::COMPILED_FUNCTION(l), Object::COMPILED_FUNCTION(r)) => {
				if l != r {
					return Err(CompilerError::WRONG_COMPILED_FUNCTION_EQUALITY { want: l.clone(), got: r.clone() });
				}
			}
			_ => {
				return Err(CompilerError::WRONG_CONSTANTS_TYPE { want: lhs.clone(), got: rhs.clone() });
			}
		}
	}

	Ok(())
}