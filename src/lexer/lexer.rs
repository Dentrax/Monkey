use crate::token::token::*;

pub struct Lexer {
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
	pub fn new(input: String) -> Self {
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

	fn read_string(&mut self) -> String {
		let pos = self.pos + 1;
		loop {
			self.read_char();
			if self.ch == '"' || self.ch == '\0' {
				break;
			}
		}
		return self.input[pos..self.pos].to_string();
	}

	pub fn next_token(&mut self) -> Token {
		let t: Token;

		self.skip_whitespace();

		match self.ch {
			'+' => t = Token::PLUS,
			'-' => t = Token::MINUS,
			'/' => t = Token::SLASH,
			'*' => t = Token::ASTERISK,
			'<' => t = Token::LT,
			'>' => t = Token::GT,
			',' => t = Token::COMMA,
			':' => t = Token::COLON,
			';' => t = Token::SEMICOLON,
			'(' => t = Token::LPAREN,
			')' => t = Token::RPAREN,
			'{' => t = Token::LBRACE,
			'}' => t = Token::RBRACE,
			'[' => t = Token::LBRACKET,
			']' => t = Token::RBRACKET,
			'"' => t = Token::STRING(self.read_string()),
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
					return Token::INT(id.parse::<isize>().unwrap());
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