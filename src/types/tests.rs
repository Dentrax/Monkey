use crate::ast::ast::BlockStatement;

use crate::types::object::*;
use crate::types::builtins::*;
use crate::types::array::*;
use crate::types::hashable::*;
use crate::types::env::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

#[test]
fn test_objects() {
	struct Test<'a> {
		input: Object,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: Object::INTEGER(7),
			expected: "INTEGER",
		},
		Test {
			input: Object::BUILTIN(Builtin::LEN),
			expected: "BUILTIN",
		},
		Test {
			input: Object::ARRAY(Array{elements: vec![]}),
			expected: "ARRAY",
		},
		Test {
			input: Object::HASH(Hash{ pairs: HashMap::new() }),
			expected: "HASH",
		},
		Test {
			input: Object::BOOLEAN(false),
			expected: "BOOLEAN",
		},
		Test {
			input: Object::RETURN(Box::new(OBJ_NULL)),
			expected: "RETURN",
		},
		Test {
			input: Object::STRING(String::from("")),
			expected: "STRING",
		},
		Test {
			input: Object::FUNCTION(Function { parameters: vec![], body: BlockStatement { statements: vec![] }, env: Rc::new(RefCell::new(Environment::new())) }),
			expected: "FUNCTION",
		},
		Test {
			input: Object::NULL,
			expected: "NULL",
		},
	];

	for test in tests {
		let type_name = test.input.get_type();
		assert_eq!(type_name, test.expected);

		match test.input {
			Object::FUNCTION(_) => assert_eq!(test.input.is_function(), true),
			Object::BUILTIN(_) => assert_eq!(test.input.is_builtin(), true),
			Object::ARRAY(_) => assert_eq!(test.input.is_array(), true),
			Object::HASH(_) => assert_eq!(test.input.is_hash(), true),
			Object::INTEGER(_) => assert_eq!(test.input.is_integer(), true),
			Object::BOOLEAN(_) => assert_eq!(test.input.is_boolean(), true),
			Object::STRING(_) => assert_eq!(test.input.is_string(), true),
			Object::RETURN(_) => assert_eq!(test.input.is_return(), true),
			Object::ERROR(_) => assert_eq!(test.input.is_error(), true),
			Object::NULL => assert_eq!(test.input.is_null(), true),
		}
	}
}