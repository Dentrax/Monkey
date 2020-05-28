use std::{fmt, result, error};
use std::io;
use std::fmt::Error;
use std::borrow::BorrowMut;

const PROMPT: &str = ">> ";

fn main() {
	let mut input = String::new();

	loop {
		let s = match io::stdin().read_line(&mut input) {
			Ok(_) => {
				let mut l = Lexer::new(input.to_owned());

				while let tok = l.next_token() {
					if tok != Token::EOF {
						println!("TOKEN: {}", tok);
					}
				}
			},
			Err(e) => {
				println!("Something went wrong: {}", e);
				break
			},
		};
	}

	println!("Hello, world!");
}

//=== AST BEGIN ===

pub enum Node {
	PROGRAM(Program),
	STATEMENT(Statement),
	EXPRESSION(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
	INT(usize),
	STRING(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	IDENT(String),
	LITERAL(Literal)
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
	name: String,
	value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	INCOMPLETE,

	//name, value
	LET(LetStatement),

	//value => x + y
	EXP(Expression)
}

pub struct Program {
	statements: Vec<Statement>
}

impl Program {
	pub fn new() -> Self {
		Program { statements: vec![] }
	}
}

impl fmt::Display for Node {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Node::PROGRAM(p) => p.fmt(f),
			Node::STATEMENT(s) => s.fmt(f),
			Node::EXPRESSION(e) => e.fmt(f),
		}
	}
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::IDENT(i) => write!(f, "{}", i),
			_ => write!(f, "handle me")
		}
	}
}

impl fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for curr in &self.statements {
			write!(f, "{}", curr);
		}
		Ok(())
	}
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Statement::INCOMPLETE => write!(f, "INCOMPLETE"),
			Statement::LET(s) => {
				write!(f, "let {} = {};", s.name, s.value)
			},
			Statement::EXP(exp) => {
				write!(f, "{}", exp)
			}
		}
	}
}

//=== AST END ====

//=== PARSER BEGIN ===

#[test]
fn test_parse_statement_let() {
	let input = r#"
let f = 03;
let u = 07;
let r = 15;
let k = 32;
let a = 34;
let n = 35;"#;

	let actual = Parser::new(Lexer::new(input.to_owned())).parse().unwrap();

	let expecteds = vec![
		Statement::LET(LetStatement{name: String::from("f"), value: Expression::IDENT(String::from("TODO"))}),
		Statement::LET(LetStatement{name: String::from("u"), value: Expression::IDENT(String::from("TODO"))}),
		Statement::LET(LetStatement{name: String::from("r"), value: Expression::IDENT(String::from("TODO"))}),
		Statement::LET(LetStatement{name: String::from("k"), value: Expression::IDENT(String::from("TODO"))}),
		Statement::LET(LetStatement{name: String::from("a"), value: Expression::IDENT(String::from("TODO"))}),
		Statement::LET(LetStatement{name: String::from("n"), value: Expression::IDENT(String::from("TODO"))}),
	];

	assert_eq!(actual.statements.len(), 6);
	assert_eq!(actual.statements, expecteds);



}

fn parse(input: &str) -> Program {
	let l = Lexer::new(input.to_owned());

	let mut p = Parser::new(l);

	return p.parse().unwrap()
}

//pub type Result<T> = result::Result<T, ParserErrorType>;

pub struct Parser {
	lexer: Lexer,
	curr_token: Token,
	peek_token: Token,
}

#[derive(Debug)]
pub enum ParserError {
	TODO,
	INVALID_TOKEN(Token),
	UNEXPECTED_TOKEN{want: String, got: String},
	NO_IDENT(Token),
	UNEXPECTED_STATEMENT_TOKEN(Token),
}

impl fmt::Display for ParserError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ParserError::TODO => write!(f, "todo"),
			ParserError::INVALID_TOKEN(t) => write!(f, "invalid token: {}", t),
			ParserError::UNEXPECTED_TOKEN { want, got } => write!(
				f,
				"parser found unexpected token: {}, expected: {}",
				got, want,
			),
			ParserError::NO_IDENT(t) => write!(f, "no ident: {}", t),
			ParserError::UNEXPECTED_STATEMENT_TOKEN(t) => write!(f, "unexpected statement token: {}", t),

		}
	}
}

impl Default for Parser {
	fn default() -> Parser {
		Parser {lexer: Lexer::default(), curr_token: Token::EOF, peek_token: Token::EOF}
	}
}

impl Parser {
	fn new(lexer: Lexer) -> Self {
		let mut p = Parser::default();
		p.lexer = lexer;

		//call twice in order to set both of curr_token and peek_token
		p.next_token();
		p.next_token();

		return p;
	}

	fn next_token(&mut self) -> Result<(), Error> {
		self.curr_token = self.peek_token.clone();
		self.peek_token = self.lexer.next_token();

		Ok(())
	}

	fn curr_token_is(&self, token: Token) -> bool {
		return self.curr_token == token
	}

	fn peek_token_is(&self, token: Token) -> bool {
		return self.peek_token == token
	}

	fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
		if self.peek_token_is(token) {
			self.next_token();
			return Ok(());
		}
		return Err(ParserError::UNEXPECTED_TOKEN{want: format!("{}", self.peek_token), got: format!("{}", self.curr_token)})
	}

	//TODO: vec err arr
	fn parse(&mut self) -> Result<Program, ParserError> {
		let mut program = Program::new();
		while self.curr_token != Token::EOF {
			match self.parse_statement() {
				Ok(s) => program.statements.push(s),
				Err(e) => return Err(e)
			}
			self.next_token();
		}
		Ok(program)
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		match self.curr_token {
			Token::LET => self.parse_statement_let(),
			_ => Ok(Statement::INCOMPLETE),
			//_ => Err(ParserError::UNEXPECTED_STATEMENT_TOKEN(self.curr_token.clone())),
		}
	}

	fn read_identifier(&mut self) -> Result<String, ParserError> {
		match self.curr_token {
			Token::IDENT(ref mut s) => Ok(s.clone()),
			_ => Err(ParserError::NO_IDENT(self.curr_token.clone()))
		}
	}

	fn parse_statement_let(&mut self) -> Result<Statement, ParserError> {
		self.next_token();

		let ident = self.read_identifier()?;

		self.expect_peek(Token::ASSIGN)?;

		//TODO: We're skipping the expressions until we encounter a semicolon
		while !self.curr_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(Statement::LET(LetStatement{name: ident.clone(), value: Expression::IDENT(String::from("TODO"))}))
	}
}

// === PARSER END ====

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
		Token::TRUE,
		Token::SEMICOLON,
		Token::RBRACE,
		Token::ELSE,
		Token::LBRACE,
		Token::RETURN,
		Token::FALSE,
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


struct Lexer {
	//current input string
	input: String,

	//curr position input (points to current char)
	pos: usize,

	//curr reading position in input (after current char)
	read_pos: usize,

	//current char under examination
	ch: char,
}

impl Default for Lexer {
	fn default() -> Lexer {
		Lexer { input: String::new(), pos: 0, read_pos: 0, ch: '\0' }
	}
}

impl Lexer {
	fn new(input: String) -> Self {
		let mut l = Lexer::default();
		l.input = input;
		l.read_char();
		return l;
	}

	fn peek_char(&self) -> char {
		if self.read_pos >= self.input.len() {
			return '\0';
		} else if let Some(c) = self.input.chars().nth(self.read_pos) {
			return c;
		}
		return '\0';
	}

	fn read_char(&mut self) {
		if self.read_pos >= self.input.len() {
			self.ch = '\0';
		} else {
			if let Some(c) = self.input.chars().nth(self.read_pos) {
				self.ch = c;
			}
		}
		self.pos = self.read_pos;
		self.read_pos += 1;
	}

	fn read_number(&mut self) -> String {
		let pos = self.pos;
		while Lexer::is_digit(self.ch) {
			self.read_char();
		}
		return self.input[pos..self.pos].to_string();
	}

	fn next_token(&mut self) -> Token {
		let t: Token;

		self.skip_whitespace();

		match self.ch {
			'+' => t = Token::PLUS,
			'-' => t = Token::MINUS,
			'/' => t = Token::SLASH,
			'*' => t = Token::ASTERISK,
			'<' => t = Token::LT,
			'>' => t = Token::GT,
			';' => t = Token::SEMICOLON,
			'(' => t = Token::LPAREN,
			')' => t = Token::RPAREN,
			',' => t = Token::COMMA,
			'{' => t = Token::LBRACE,
			'}' => t = Token::RBRACE,
			'\0' => t = Token::EOF,
			'\t' => t = Token::TAB,
			'\r' => t = Token::CR,
			'\n' => t = Token::LF,
			'=' => {
				if self.peek_char() == '=' {
					self.read_char();
					t = Token::EQ;
				} else {
					t = Token::ASSIGN;
				}
			}
			'!' => {
				if self.peek_char() == '=' {
					self.read_char();
					t = Token::NEQ;
				} else {
					t = Token::BANG;
				}
			}
			_ => {
				if Lexer::is_letter(self.ch) {
					let id = self.read_identifier();
					let to = Token::lookup_ident(id);
					return to;
				} else if Lexer::is_digit(self.ch) {
					let id = self.read_number();
					return Token::INT(id.parse().unwrap());
				}
				t = Token::ILLEGAL(self.ch);
			}
		}

		self.read_char();

		return t;
	}

	fn read_identifier(&mut self) -> String {
		let pos = self.pos;
		while Lexer::is_letter(self.ch) {
			self.read_char();
		}
		return self.input[pos..self.pos].to_string();
	}

	fn skip_whitespace(&mut self) {
		while self.ch.is_whitespace() || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
			self.read_char();
		}
	}

	fn is_letter(ch: char) -> bool {
		return ch.is_ascii_alphabetic() || ch == '_';
	}

	fn is_digit(ch: char) -> bool {
		return ch.is_ascii_digit();
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	ILLEGAL(char),
	EOF,
	TAB,
	CR,
	LF,

	IDENT(String),
	INT(usize),
	BOOL(bool),

	EQ,
	NEQ,

	TRUE,
	FALSE,

	ASSIGN,
	PLUS,
	MINUS,

	BANG,
	ASTERISK,
	SLASH,

	LT,
	GT,

	COMMA,
	SEMICOLON,

	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,

	FUNCTION,
	LET,
	IF,
	ELSE,
	RETURN,
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Token::ILLEGAL(i) => write!(f, "{}", i),
			Token::EOF => write!(f, "EOF"),
			Token::TAB => write!(f, "TAB"),
			Token::CR => write!(f, "CR"),
			Token::LF => write!(f, "LF"),

			Token::IDENT(i) => write!(f, "{}", i),
			Token::INT(i) => write!(f, "{}", i),
			Token::BOOL(i) => write!(f, "{}", i),

			Token::EQ => write!(f, "EQ"),
			Token::NEQ => write!(f, "NEQ"),

			Token::TRUE => write!(f, "TRUE"),
			Token::FALSE => write!(f, "FALSE"),

			Token::ASSIGN => write!(f, "="),
			Token::PLUS => write!(f, "+"),
			Token::MINUS => write!(f, "-"),

			Token::BANG => write!(f, "!"),
			Token::ASTERISK => write!(f, "*"),
			Token::SLASH => write!(f, "/"),

			Token::LT => write!(f, "<"),
			Token::GT => write!(f, ">"),

			Token::COMMA => write!(f, ","),
			Token::SEMICOLON => write!(f, ";"),

			Token::LPAREN => write!(f, "("),
			Token::RPAREN => write!(f, ")"),
			Token::LBRACE => write!(f, "{{"),
			Token::RBRACE => write!(f, "}}"),

			Token::FUNCTION => write!(f, "FUNCTION"),
			Token::LET => write!(f, "LET"),
			Token::IF => write!(f, "IF"),
			Token::ELSE => write!(f, "ELSE"),
			Token::RETURN => write!(f, "RETURN"),
		}
	}
}

impl Token {
	fn lookup_ident(ident: String) -> Token {
		match ident.as_str() {
			"fn" => Token::FUNCTION,
			"let" => Token::LET,
			"true" => Token::TRUE,
			"false" => Token::FALSE,
			"if" => Token::IF,
			"else" => Token::ELSE,
			"return" => Token::RETURN,

			_ => Token::IDENT(ident.to_string())
		}
	}
}