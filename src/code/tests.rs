use crate::code::code::{OpCodeType, make, CodeError, Instructions, IInstructions, lookup, read_operands};


#[test]
fn test_make() {
	struct Test {
		op: OpCodeType,
		operands: Vec<usize>,
		expected: Vec<u8>,
		expectedErr: Option<CodeError>,
	};

	let tests = vec![
		Test {
			op: OpCodeType::CONSTANT,
			operands: vec![65534],
			expected: vec![OpCodeType::CONSTANT as u8, 0xFF, 0xFE],
			expectedErr: None,
		},
		Test {
			op: OpCodeType::ADD,
			operands: vec![],
			expected: vec![OpCodeType::ADD as u8],
			expectedErr: None,
		},
		Test {
			op: OpCodeType::LG,
			operands: vec![255],
			expected: vec![OpCodeType::LG as u8, 0xFF],
			expectedErr: None,
		},
	];

	for test in tests {
		match make(test.op, &test.operands) {
			Ok(v) => assert_eq!(v, test.expected),
			Err(e) => assert_eq!(e, test.expectedErr.unwrap()),
		};
	}
}

#[test]
fn test_read_operands() {
	struct Test {
		op: OpCodeType,
		operands: Vec<usize>,
		bytesRead: usize,
	};

	let tests = vec![
		Test {
			op: OpCodeType::CONSTANT,
			operands: vec![65535],
			bytesRead: 2
		},
		Test {
			op: OpCodeType::LG,
			operands: vec![255],
			bytesRead: 1
		},
	];

	for test in tests {
		let instruction = make(test.op, &test.operands).unwrap();
		let def = lookup(test.op);
		let (operandsRead, n) = read_operands(&def, &instruction[1..]);
		assert_eq!(n, test.bytesRead);
		assert_eq!(operandsRead, test.operands);
	}
}

#[test]
fn test_string() {
	struct Test<'a> {
		instructions: Vec<Instructions>,
		expected: &'a str,
	};

	let tests = vec![
		Test {
			instructions: vec![
				make(OpCodeType::ADD, &vec![]).unwrap(),
				make(OpCodeType::LG, &vec![1]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![2]).unwrap(),
				make(OpCodeType::CONSTANT, &vec![65535]).unwrap(),
			],
			expected: "0000 OpAdd\n0001 OpGetLocal 1\n0003 OpConstant 2\n0006 OpConstant 65535\n",
		},
	];

	for test in tests {
		let concatted = test.instructions.into_iter().flatten().collect::<Instructions>();
		assert_eq!(concatted.string(), test.expected.to_string())
	}
}