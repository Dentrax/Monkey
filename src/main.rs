use std::{fmt};
use std::io;
use std::fmt::{Error, Formatter};

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

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Literal::INT(i) => write!(f, "{}", i),
			Literal::STRING(s) => write!(f, "{}", s)
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	IDENT(String),
	LITERAL(Literal),

	PREFIX(PrefixExpression),
	INFIX(InfixExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixType {
	BANG,
	MINUS,
}

impl fmt::Display for PrefixType {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			PrefixType::BANG => write!(f, "!"),
			PrefixType::MINUS => write!(f, "-")
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpression {
	operator: PrefixType,
	right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "({}{})", self.operator, self.right)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum InfixType {
	PLUS,
	MINUS,
	DIVISION,
	MULTIPLICATION,
	LT,
	GT,
	EQ,
	NEQ,

}

impl fmt::Display for InfixType {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			InfixType::PLUS => write!(f, "+"),
			InfixType::MINUS => write!(f, "-"),
			InfixType::DIVISION => write!(f, "/"),
			InfixType::MULTIPLICATION => write!(f, "*"),
			InfixType::LT => write!(f, "<"),
			InfixType::GT => write!(f, ">"),
			InfixType::EQ => write!(f, "=="),
			InfixType::NEQ => write!(f, "!="),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpression {
	left: Box<Expression>,
	operator: InfixType,
	right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "({} {} {})", self.left, self.operator, self.right)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
	name: String,
	value: Expression,
}

impl fmt::Display for LetStatement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "let {} = {};", self.name, self.value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
	value: Expression,
}

impl fmt::Display for ReturnStatement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "return {};", self.value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	INCOMPLETE,

	//name, value
	LET(LetStatement),

	//value => x + y
	EXPRESSION(Expression),

	RETURN(ReturnStatement)
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
			Expression::LITERAL(i) => write!(f, "{}", i),
			Expression::PREFIX(i) => write!(f, "{}", i),
			Expression::INFIX(i) => write!(f, "{}", i),
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
			Statement::INCOMPLETE => write!(f, "incomplete"),
			Statement::LET(stmt) => stmt.fmt(f),
			Statement::EXPRESSION(stmt) => stmt.fmt(f),
			Statement::RETURN(stmt) => stmt.fmt(f),
		}
	}
}

#[test]
fn test_ast_string() {
	let program = Program {
		statements: vec![
			Statement::LET(LetStatement{name: String::from("country"), value: Expression::IDENT(String::from("istanbul"))}),
			Statement::LET(LetStatement{name: String::from("neighborhood"), value: Expression::IDENT(String::from("maslak"))})
		]
	};

	let expected = "let country = istanbul;let neighborhood = maslak;";

	assert_eq!(program.to_string(), expected);
}

//=== AST END ====

//=== PARSER BEGIN ===

#[test]
fn test_ast_operator_precedence_string() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "-a * b",
			expected: "((-a) * b)"
		},
		Test {
			input: "!-a",
			expected: "(!(-a))"
		},
		Test {
			input: "a + b + c",
			expected: "((a + b) + c)"
		},
		Test {
			input: "a + b - c",
			expected: "((a + b) - c)",
		},
		Test {
			input: "a * b * c",
			expected: "((a * b) * c)"
		},
		Test {
			input: "a * b / c",
			expected: "((a * b) / c)"
		},
		Test {
			input: "a + b / c",
			expected: "(a + (b / c))"
		},
		Test {
			input: "a + b * c + d / e - f",
			expected: "(((a + (b * c)) + (d / e)) - f)",
		},
		Test {
			input: "3 + 4; -5 * 5",
			expected: "(3 + 4)((-5) * 5)"
		},
		Test {
			input: "5 > 4 == 3 < 4",
			expected: "((5 > 4) == (3 < 4))"
		},
		Test {
			input: "5 < 4 != 3 > 4",
			expected: "((5 < 4) != (3 > 4))"
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
fn test_parse_statement_let() {
	let input = r#"
let f = 03;
let u = 07;
let r = 15;
let k = 32;
let a = 34;
let n = 35;"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::LET(LetStatement{name: String::from("f"), value: Expression::IDENT(String::from("TODO::LET"))}),
		Statement::LET(LetStatement{name: String::from("u"), value: Expression::IDENT(String::from("TODO::LET"))}),
		Statement::LET(LetStatement{name: String::from("r"), value: Expression::IDENT(String::from("TODO::LET"))}),
		Statement::LET(LetStatement{name: String::from("k"), value: Expression::IDENT(String::from("TODO::LET"))}),
		Statement::LET(LetStatement{name: String::from("a"), value: Expression::IDENT(String::from("TODO::LET"))}),
		Statement::LET(LetStatement{name: String::from("n"), value: Expression::IDENT(String::from("TODO::LET"))}),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_return() {
	let input = r#"
return 15;
return 34;
return 707707;"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::RETURN(ReturnStatement{ value: Expression::IDENT(String::from("TODO::RETURN"))}),
		Statement::RETURN(ReturnStatement{ value: Expression::IDENT(String::from("TODO::RETURN"))}),
		Statement::RETURN(ReturnStatement{ value: Expression::IDENT(String::from("TODO::RETURN"))}),
	];

	assert_eq!(actual.statements, expecteds);
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
fn test_parse_statement_expression_prefix() {
	let input = "!7;-15;";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression{operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::INT(7)))})),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression{operator: PrefixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
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
7 != 15;"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::PLUS, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MULTIPLICATION, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::DIVISION, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::GT, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::LT, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression{left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::NEQ, right: Box::new(Expression::LITERAL(Literal::INT(15)))})),
	];

	assert_eq!(actual.statements, expecteds);
}

type ParserPrefixFunc = fn(&mut Parser) -> Result<Expression, ParserError>;
type ParserInfixFunc = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

pub struct Parser {
	lexer: Lexer,
	curr_token: Token,
	peek_token: Token,
}

#[derive(Debug)]
pub enum ParserError {
	TODO,
	INVALID_TOKEN(Token),
	INVALID_LITERAL(String),
	UNEXPECTED_TOKEN{want: String, got: String},
	NO_IDENT(Token),
	UNEXPECTED_STATEMENT_TOKEN(Token),
	UNEXPECTED_PREFIX_FUNC(Token),
	UNEXPECTED_PREFIX_TYPE(Token),
	UNEXPECTED_INFIX_TYPE(Token),
}

impl fmt::Display for ParserError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ParserError::TODO => write!(f, "todo"),
			ParserError::INVALID_TOKEN(t) => write!(f, "invalid token: {}", t),
			ParserError::INVALID_LITERAL(t) => write!(f, "invalid literal: {}", t),
			ParserError::UNEXPECTED_TOKEN { want, got } => write!(
				f,
				"parser found unexpected token: {}, expected: {}",
				got, want,
			),
			ParserError::NO_IDENT(t) => write!(f, "no ident: {}", t),
			ParserError::UNEXPECTED_STATEMENT_TOKEN(t) => write!(f, "unexpected statement token: {}", t),
			ParserError::UNEXPECTED_PREFIX_FUNC(t) => write!(f, "unexpected prefix func for: {}", t),
			ParserError::UNEXPECTED_PREFIX_TYPE(t) => write!(f, "unexpected prefix type for token: {}", t),
			ParserError::UNEXPECTED_INFIX_TYPE(t) => write!(f, "unexpected infix type for token: {}", t),

		}
	}
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
	//
	LOWEST,
	// ==
	EQUALS,
	// <>
	LESSGREATER,
	// +
	SUM,
	// *
	PRODUCT,
	//-X or !X
	PREFIX,
	// func(X)
	CALL,
}

fn get_precedence_for_token_type(token: &Token) -> Precedence {
	match token {
		Token::PLUS => Precedence::SUM,
		Token::MINUS => Precedence::SUM,
		Token::EQ => Precedence::EQUALS,
		Token::NEQ => Precedence::EQUALS,
		Token::LT => Precedence::LESSGREATER,
		Token::GT => Precedence::LESSGREATER,
		Token::SLASH => Precedence::PRODUCT,
		Token::ASTERISK => Precedence::PRODUCT,
		_ => Precedence::LOWEST,
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

	fn curr_precedence(&self) -> Precedence {
		return get_precedence_for_token_type(&self.curr_token);
	}

	fn peek_precedence(&self) -> Precedence {
		return get_precedence_for_token_type(&self.peek_token);
	}

	fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
		if self.peek_token_is(token) {
			self.next_token();
			return Ok(());
		}
		return Err(ParserError::UNEXPECTED_TOKEN{want: format!("{}", self.peek_token), got: format!("{}", self.curr_token)})
	}

	//TODO: vec err arr
	fn parse(&mut self) -> (Program, Vec<ParserError>) {
		let mut program = Program::new();
		let mut errs = Vec::new();
		while self.curr_token != Token::EOF {
			match self.parse_statement() {
				Ok(s) => program.statements.push(s),
				Err(e) => errs.push(e),
			}
			self.next_token();
		}
		(program, errs)
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		match self.curr_token {
			Token::LET => self.parse_statement_let(),
			Token::RETURN => self.parse_statement_return(),
			_ => self.parse_statement_expression(),
		}
	}

	fn read_identifier(&mut self) -> Result<String, ParserError> {
		match self.curr_token {
			Token::IDENT(ref mut s) => Ok(s.clone()),
			_ => Err(ParserError::NO_IDENT(self.curr_token.clone()))
		}
	}


	//TODO: Token::INT(String)?
	fn parse_literal_integer(&mut self) -> Result<Expression, ParserError> {
		match self.curr_token {
			Token::INT(i) => {
				Ok(Expression::LITERAL(Literal::INT(i)))
			}
			_ => Err(ParserError::INVALID_TOKEN(self.curr_token.clone()))
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

		Ok(Statement::LET(LetStatement{name: ident.clone(), value: Expression::IDENT(String::from("TODO::LET"))}))
	}

	fn parse_statement_return(&mut self) -> Result<Statement, ParserError> {
		self.next_token();

		//TODO: We're skipping the expressions until we encounter a semicolon
		while !self.curr_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(Statement::RETURN(ReturnStatement{value: Expression::IDENT(String::from("TODO::RETURN"))}))
	}

	fn parse_statement_expression(&mut self) -> Result<Statement, ParserError> {
		let expr = self.parse_expression(Precedence::LOWEST)?;

		if self.peek_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(Statement::EXPRESSION(expr))
	}


	fn parse_statement_identifier(&mut self) -> Result<Expression, ParserError> {
		match self.read_identifier() {
			Ok(ident) => Ok(Expression::IDENT(ident)),
			Err(err) => Err(err),
		}
	}

	fn parse_prefix(&self) -> Option<ParserPrefixFunc> {
		Some(match &self.curr_token {
			Token::IDENT(_) => Parser::parse_statement_identifier,
			Token::INT(_) => Parser::parse_literal_integer,
			Token::BANG | Token::MINUS => Parser::parse_expression_prefix,
			_ => return None
		})
	}

	fn parse_infix(&self) -> Option<ParserInfixFunc> {
		Some(match &self.peek_token {
			| Token::PLUS
			| Token::MINUS
			| Token::SLASH
			| Token::ASTERISK
			| Token::EQ
			| Token::NEQ
			| Token::LT
			| Token::GT => Parser::parse_expression_infix,
			_ => return None
		})
	}

	// Hearth of our Vaughan Pratt parser - “Top Down Operator Precedence”
	fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
		let prefix = self.parse_prefix().ok_or_else(|| ParserError::UNEXPECTED_PREFIX_FUNC(self.curr_token.clone()))?;
		let mut left_exp = prefix(self)?;

		while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek_precedence() {
			let infix = self.parse_infix().ok_or_else(|| ParserError::UNEXPECTED_INFIX_TYPE(self.curr_token.clone()))?;
			self.next_token();
			left_exp = infix(self, left_exp)?;
		}
		
		Ok(left_exp)
	}

	fn parse_expression_prefix(&mut self) -> Result<Expression, ParserError> {
		let prefix_type = match self.curr_token {
			Token::BANG => PrefixType::BANG,
			Token::MINUS => PrefixType::MINUS,
			_ => return Err(ParserError::UNEXPECTED_PREFIX_TYPE(self.curr_token.clone()))
		};

		self.next_token();

		match self.parse_expression(Precedence::PREFIX) {
			Ok(expr) => Ok(Expression::PREFIX(PrefixExpression{operator: prefix_type, right: Box::new(expr)})),
			Err(e) => Err(e),
		}
	}

	fn parse_expression_infix(&mut self, left: Expression) -> Result<Expression, ParserError> {
		let infix_type = match self.curr_token {
			Token::PLUS => InfixType::PLUS,
			Token::MINUS => InfixType::MINUS,
			Token::SLASH => InfixType::DIVISION,
			Token::ASTERISK => InfixType::MULTIPLICATION,
			Token::EQ => InfixType::EQ,
			Token::NEQ => InfixType::NEQ,
			Token::LT => InfixType::LT,
			Token::GT => InfixType::GT,
			_ => return Err(ParserError::UNEXPECTED_INFIX_TYPE(self.curr_token.clone()))
		};

		let curr_precedence = self.curr_precedence();

		self.next_token();

		match self.parse_expression(curr_precedence) {
			Ok(expr) => Ok(Expression::INFIX(InfixExpression{left: Box::new(left), operator: infix_type, right: Box::new(expr)})),
			Err(e) => Err(e),
		}
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
					return Token::INT(id.parse::<usize>().unwrap()); //TODO: match ret err?
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