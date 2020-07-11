use crate::code::code::{OpCodeType, make, CodeError};


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
	];

	for test in tests {
		match make(test.op, test.operands) {
			Ok(v) => assert_eq!(v, test.expected),
			Err(e) => assert_eq!(e, test.expectedErr.unwrap()),
		};
	}
}