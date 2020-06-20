use crate::token::token::*;
use crate::lexer::lexer::*;

#[test]
fn test_next_token_basic() {
	let input = "=+(){},;?!-/*7;15<x>34;7==7;15!=15;";
	let expected = vec![
		Token::ASSIGN,
		Token::PLUS,
		Token::LPAREN,
		Token::RPAREN,
		Token::LBRACE,
		Token::RBRACE,
		Token::COMMA,
		Token::SEMICOLON,
		Token::ILLEGAL('?'),
		Token::BANG,
		Token::MINUS,
		Token::SLASH,
		Token::ASTERISK,
		Token::INT(7),
		Token::SEMICOLON,
		Token::INT(15),
		Token::LT,
		Token::IDENT(String::from("x")),
		Token::GT,
		Token::INT(34),
		Token::SEMICOLON,
		Token::INT(7),
		Token::EQ,
		Token::INT(7),
		Token::SEMICOLON,
		Token::INT(15),
		Token::NEQ,
		Token::INT(15),
		Token::SEMICOLON,
		Token::EOF,
	];
	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}

#[test]
fn test_next_token_let() {
	let input = r#"let three = 3;
let seven = 7;

let add = fn(x, y) = {
	x + y;
};

let result = add(three, seven);"#;

	let expected = vec![
		Token::LET,
		Token::IDENT(String::from("three")),
		Token::ASSIGN,
		Token::INT(3),
		Token::SEMICOLON,
		Token::LET,
		Token::IDENT(String::from("seven")),
		Token::ASSIGN,
		Token::INT(7),
		Token::SEMICOLON,
		Token::LET,
		Token::IDENT(String::from("add")),
		Token::ASSIGN,
		Token::FUNCTION,
		Token::LPAREN,
		Token::IDENT(String::from("x")),
		Token::COMMA,
		Token::IDENT(String::from("y")),
		Token::RPAREN,
		Token::ASSIGN,
		Token::LBRACE,
		Token::IDENT(String::from("x")),
		Token::PLUS,
		Token::IDENT(String::from("y")),
		Token::SEMICOLON,
		Token::RBRACE,
		Token::SEMICOLON,
		Token::LET,
		Token::IDENT(String::from("result")),
		Token::ASSIGN,
		Token::IDENT(String::from("add")),
		Token::LPAREN,
		Token::IDENT(String::from("three")),
		Token::COMMA,
		Token::IDENT(String::from("seven")),
		Token::RPAREN,
		Token::SEMICOLON,
	];

	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}

#[test]
fn test_next_token_condition() {
	let input = r#"if (7 < 15) {
	return true;
} else {
	return false;
}"#;

	let expected = vec![
		Token::IF,
		Token::LPAREN,
		Token::INT(7),
		Token::LT,
		Token::INT(15),
		Token::RPAREN,
		Token::LBRACE,
		Token::RETURN,
		Token::BOOL(true),
		Token::SEMICOLON,
		Token::RBRACE,
		Token::ELSE,
		Token::LBRACE,
		Token::RETURN,
		Token::BOOL(false),
		Token::SEMICOLON,
		Token::RBRACE,
		Token::EOF,
	];

	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}

#[test]
fn test_next_token_string() {
	let input = r#"
"foobar"
"foo bar"
"+foo+bar+"
"#;

	let expected = vec![
		Token::STRING(String::from("foobar")),
		Token::STRING(String::from("foo bar")),
		Token::STRING(String::from("+foo+bar+")),
		Token::EOF,
	];

	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}

#[test]
fn test_next_token_array() {
	let input = "[1, 2];";

	let expected = vec![
		Token::LBRACKET,
		Token::INT(1),
		Token::COMMA,
		Token::INT(2),
		Token::RBRACKET,
		Token::SEMICOLON,
		Token::EOF,
	];

	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}

#[test]
fn test_next_token_map() {
	let input = r#"{"foo": "bar"};"#;

	let expected = vec![
		Token::LBRACE,
		Token::STRING(String::from("foo")),
		Token::COLON,
		Token::STRING(String::from("bar")),
		Token::RBRACE,
		Token::SEMICOLON,
		Token::EOF,
	];

	let mut l = Lexer::new(input.to_owned());
	for i in expected {
		let t = l.next_token();
		assert_eq!(t, i);
	}
}