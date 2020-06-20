use crate::ast::ast::*;

use crate::token::token::*;
use crate::lexer::lexer::*;

use std::{fmt};
use std::fmt::{Error};

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
	UNEXPECTED_TOKEN { want: String, got: String },
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
	// index[]
	INDEX
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
		Token::LPAREN => Precedence::CALL,
		Token::LBRACKET => Precedence::INDEX,
		_ => Precedence::LOWEST,
	}
}

impl Default for Parser {
	fn default() -> Parser {
		Parser { lexer: Lexer::default(), curr_token: Token::EOF, peek_token: Token::EOF }
	}
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
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
		return self.curr_token == token;
	}

	fn peek_token_is(&self, token: Token) -> bool {
		return self.peek_token == token;
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
		return Err(ParserError::UNEXPECTED_TOKEN { want: format!("{}", self.peek_token), got: format!("{}", self.curr_token) });
	}

	pub fn parse(&mut self) -> (Program, Vec<ParserError>) {
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


	fn parse_literal_integer(&mut self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::INT(i) => {
				Ok(Expression::LITERAL(Literal::INT(i.clone())))
			}
			_ => Err(ParserError::INVALID_TOKEN(self.curr_token.clone()))
		}
	}

	fn parse_literal_string(&mut self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::STRING(i) => {
				Ok(Expression::LITERAL(Literal::STRING(i.clone())))
			}
			_ => Err(ParserError::INVALID_TOKEN(self.curr_token.clone()))
		}
	}

	fn parse_literal_boolean(&mut self) -> Result<Expression, ParserError> {
		match &self.curr_token {
			Token::BOOL(i) => {
				Ok(Expression::LITERAL(Literal::BOOL(i.clone())))
			}
			_ => Err(ParserError::INVALID_TOKEN(self.curr_token.clone()))
		}
	}

	fn parse_literal_function(&mut self) -> Result<Expression, ParserError> {
		self.expect_peek(Token::LPAREN)?;

		let parameters = self.parse_literal_function_parameters()?;

		self.expect_peek(Token::LBRACE)?;

		let body = if let Statement::BLOCK(block) = self.parse_statement_block()? {
			block
		} else {
			return Err(ParserError::UNEXPECTED_STATEMENT_TOKEN(self.curr_token.clone()));
		};

		Ok(Expression::FUNCTION(
			FunctionLiteral {
				parameters,
				body,
			}
		))
	}

	fn parse_literal_function_parameters(&mut self) -> Result<Vec<String>, ParserError> {
		let mut identifiers = Vec::new();

		if self.peek_token_is(Token::RPAREN) {
			self.next_token();
			return Ok(identifiers);
		}

		self.next_token();
		identifiers.push(self.read_identifier()?);

		while self.peek_token_is(Token::COMMA) {
			self.next_token();
			self.next_token();

			identifiers.push(self.read_identifier()?);
		}

		self.expect_peek(Token::RPAREN)?;

		Ok(identifiers)
	}

	fn parse_statement_let(&mut self) -> Result<Statement, ParserError> {
		self.next_token();

		let ident = self.read_identifier()?;

		self.expect_peek(Token::ASSIGN)?;

		self.next_token();

		let expr = self.parse_expression(Precedence::LOWEST)?;

		if self.peek_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(
			Statement::LET(
				LetStatement {
					name: ident.clone(),
					value: expr,
				}
			)
		)
	}

	fn parse_statement_return(&mut self) -> Result<Statement, ParserError> {
		self.next_token();

		let expr = self.parse_expression(Precedence::LOWEST)?;

		if self.peek_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(
			Statement::RETURN(
				ReturnStatement {
					value: expr
				}
			)
		)
	}

	fn parse_statement_expression(&mut self) -> Result<Statement, ParserError> {
		let expr = self.parse_expression(Precedence::LOWEST)?;

		if self.peek_token_is(Token::SEMICOLON) {
			self.next_token();
		}

		Ok(Statement::EXPRESSION(expr))
	}

	fn parse_statement_block(&mut self) -> Result<Statement, ParserError> {
		self.next_token();

		let mut statements = vec![];

		while !self.curr_token_is(Token::RBRACE) && !self.curr_token_is(Token::EOF) {
			statements.push(self.parse_statement()?);
			self.next_token();
		}

		Ok(Statement::BLOCK(BlockStatement { statements }))
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
			Token::STRING(_) => Parser::parse_literal_string,
			Token::BOOL(_) => Parser::parse_literal_boolean,
			Token::BANG | Token::MINUS => Parser::parse_expression_prefix,
			Token::LPAREN => Parser::parse_expression_grouped,
			Token::IF => Parser::parse_expression_if,
			Token::FUNCTION => Parser::parse_literal_function,
			Token::LBRACKET => Parser::parse_expression_array,
			Token::LBRACE => Parser::parse_literal_hash,
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
			Token::LPAREN => Parser::parse_expression_call,
			Token::LBRACKET => Parser::parse_expression_index,
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


	fn parse_expression_array(&mut self) -> Result<Expression, ParserError> {
		let elems = self.parse_expression_list(Token::RBRACKET)?;
		Ok(Expression::ARRAY(ArrayLiteral{elements: elems}))
	}

	fn parse_literal_hash(&mut self) -> Result<Expression, ParserError> {
		let mut pairs = vec![];

		while !self.peek_token_is(Token::RBRACE) {
			self.next_token();
			let key = self.parse_expression(Precedence::LOWEST)?;

			self.expect_peek(Token::COLON)?;

			self.next_token();
			let value = self.parse_expression(Precedence::LOWEST)?;

			pairs.push((key, value));

			if !self.peek_token_is(Token::RBRACE) {
				self.expect_peek(Token::COMMA)?;
			}
		}

		self.expect_peek(Token::RBRACE)?;

		Ok(Expression::HASH(HashLiteral{ pairs }))
	}

	fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, ParserError> {
		let mut list: Vec<Expression> = vec![];

		if self.peek_token == end {
			self.next_token();
			return Ok(list)
		}

		self.next_token();

		list.push(self.parse_expression(Precedence::LOWEST)?);

		while self.peek_token_is(Token::COMMA) {
			self.next_token();
			self.next_token();

			list.push(self.parse_expression(Precedence::LOWEST)?);
		}

		self.expect_peek(end)?;

		Ok(list)
	}

	fn parse_expression_prefix(&mut self) -> Result<Expression, ParserError> {
		let prefix_type = match self.curr_token {
			Token::BANG => PrefixType::BANG,
			Token::MINUS => PrefixType::MINUS,
			_ => return Err(ParserError::UNEXPECTED_PREFIX_TYPE(self.curr_token.clone()))
		};

		self.next_token();

		match self.parse_expression(Precedence::PREFIX) {
			Ok(expr) => Ok(Expression::PREFIX(PrefixExpression { operator: prefix_type, right: Box::new(expr) })),
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
			Ok(expr) => Ok(Expression::INFIX(InfixExpression { left: Box::new(left), operator: infix_type, right: Box::new(expr) })),
			Err(e) => Err(e),
		}
	}

	fn parse_expression_call(&mut self, function: Expression) -> Result<Expression, ParserError> {
		let args = self.parse_expression_list(Token::RPAREN)?;
		Ok(
			Expression::CALL(
				CallExpression {
					function: Box::new(function),
					arguments: args,
				}
			)
		)
	}

	fn parse_expression_index(&mut self, left: Expression) -> Result<Expression, ParserError> {
		self.next_token();

		let index = self.parse_expression(Precedence::LOWEST)?;

		self.expect_peek(Token::RBRACKET)?;

		Ok(Expression::INDEX(IndexExpression{left: Box::new(left), index: Box::new(index)}))
	}

	fn parse_expression_grouped(&mut self) -> Result<Expression, ParserError> {
		self.next_token();

		let expr = self.parse_expression(Precedence::LOWEST);
		self.expect_peek(Token::RPAREN)?;

		return expr;
	}

	fn parse_expression_if(&mut self) -> Result<Expression, ParserError> {
		self.expect_peek(Token::LPAREN)?;

		self.next_token();

		let condition = Box::new(self.parse_expression(Precedence::LOWEST)?);

		self.expect_peek(Token::RPAREN)?;

		self.expect_peek(Token::LBRACE)?;

		let consequence = if let Statement::BLOCK(block) = self.parse_statement_block()? {
			block
		} else {
			return Err(ParserError::UNEXPECTED_STATEMENT_TOKEN(self.curr_token.clone()));
		};

		let mut alternative = None;

		if self.peek_token_is(Token::ELSE) {
			self.next_token();
			self.expect_peek(Token::LBRACE)?;

			alternative = if let Statement::BLOCK(block) = self.parse_statement_block()? {
				Some(block)
			} else {
				return Err(ParserError::UNEXPECTED_STATEMENT_TOKEN(self.curr_token.clone()));
			};
		}

		Ok(Expression::IF(
			IfExpression {
				condition,
				consequence,
				alternative,
			}
		))
	}
}
