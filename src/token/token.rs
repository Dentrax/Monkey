use std::{fmt};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	ILLEGAL(char),
	EOF,
	TAB,
	CR,
	LF,

	IDENT(String),
	INT(isize),
	BOOL(bool),
	STRING(String),

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
	COLON,
	SEMICOLON,

	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,
	LBRACKET,
	RBRACKET,

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
			Token::STRING(i) => write!(f, "{}", i),

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
			Token::COLON => write!(f, ":"),
			Token::SEMICOLON => write!(f, ";"),

			Token::LPAREN => write!(f, "("),
			Token::RPAREN => write!(f, ")"),
			Token::LBRACE => write!(f, "{{"),
			Token::RBRACE => write!(f, "}}"),
			Token::LBRACKET => write!(f, "["),
			Token::RBRACKET => write!(f, "]"),

			Token::FUNCTION => write!(f, "FUNCTION"),
			Token::LET => write!(f, "LET"),
			Token::IF => write!(f, "IF"),
			Token::ELSE => write!(f, "ELSE"),
			Token::RETURN => write!(f, "RETURN"),
		}
	}
}

impl Token {
	pub fn lookup_ident(ident: String) -> Token {
		match ident.as_str() {
			"fn" => Token::FUNCTION,
			"let" => Token::LET,
			"true" => Token::BOOL(true),
			"false" => Token::BOOL(false),
			"if" => Token::IF,
			"else" => Token::ELSE,
			"return" => Token::RETURN,

			_ => Token::IDENT(ident.to_string())
		}
	}
}