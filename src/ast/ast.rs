//=== AST BEGIN ===

use std::{fmt};
use std::fmt::{Formatter};

pub enum Node {
	PROGRAM(Program),
	STATEMENT(Statement),
	EXPRESSION(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
	INT(isize),
	STRING(String),
	BOOL(bool),
}

impl fmt::Display for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Literal::INT(i) => write!(f, "{}", i),
			Literal::STRING(s) => write!(f, "{}", s),
			Literal::BOOL(b) => write!(f, "{}", b),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayLiteral {
	pub elements: Vec<Expression>
}

impl fmt::Display for ArrayLiteral {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let elems: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect::<Vec<String>>();
		write!(f, "[{}]", elems.join(", "))
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct HashLiteral {
	pub pairs: Vec<(Expression, Expression)>
}

impl fmt::Display for HashLiteral {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let pairs = self.pairs.iter().map(|(k, v)| format!(r#"{}: {}"#, k, v)).collect::<Vec<String>>();
		write!(f, "{{{}}}", pairs.join(", "))
	}
}


#[derive(Clone, Debug, PartialEq)]
pub struct IndexExpression {
	pub left: Box<Expression>,
	pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "({}[{}])", self.left, self.index)
	}
}


#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	IDENT(String),
	LITERAL(Literal),

	ARRAY(ArrayLiteral),
	HASH(HashLiteral),

	PREFIX(PrefixExpression),
	INFIX(InfixExpression),
	INDEX(IndexExpression),

	IF(IfExpression),
	FUNCTION(FunctionLiteral),
	CALL(CallExpression),
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expression::IDENT(i) => i.fmt(f),
			Expression::LITERAL(i) => i.fmt(f),
			Expression::ARRAY(i) => i.fmt(f),
			Expression::HASH(i) => i.fmt(f),
			Expression::PREFIX(i) => i.fmt(f),
			Expression::INFIX(i) => i.fmt(f),
			Expression::INDEX(i) => i.fmt(f),
			Expression::IF(i) => i.fmt(f),
			Expression::FUNCTION(i) => i.fmt(f),
			Expression::CALL(i) => i.fmt(f),
		}
	}
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
	pub operator: PrefixType,
	pub right: Box<Expression>,
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
	pub left: Box<Expression>,
	pub operator: InfixType,
	pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "({} {} {})", self.left, self.operator, self.right)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpression {
	pub condition: Box<Expression>,
	pub consequence: BlockStatement,
	pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;
		if let Some(s) = &self.alternative {
			write!(f, " else {{ {} }}", s)?;
		}
		Ok(())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLiteral {
	pub parameters: Vec<String>,
	pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "fn({}) {{ {} }}", self.parameters.join(", "), self.body)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpression {
	pub function: Box<Expression>,
	pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut arguments: Vec<String> = vec![];
		for arg in &self.arguments {
			arguments.push(format!("{}", arg));
		}
		write!(f, "{}({})", self.function, arguments.join(", "))
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
	pub name: String,
	pub value: Expression,
}

impl fmt::Display for LetStatement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "let {} = {};", self.name, self.value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
	pub value: Expression,
}

impl fmt::Display for ReturnStatement {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "return {};", self.value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement {
	pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			stmt.fmt(f)?;
		}
		Ok(())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	INCOMPLETE,

	//name, value
	LET(LetStatement),

	//value => x + y
	EXPRESSION(Expression),

	RETURN(ReturnStatement),

	BLOCK(BlockStatement),
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Statement::INCOMPLETE => write!(f, "incomplete"),
			Statement::LET(stmt) => stmt.fmt(f),
			Statement::EXPRESSION(stmt) => stmt.fmt(f),
			Statement::RETURN(stmt) => stmt.fmt(f),
			Statement::BLOCK(stmt) => stmt.fmt(f),
		}
	}
}

pub struct Program {
	pub statements: Vec<Statement>
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

impl fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for curr in &self.statements {
			write!(f, "{}", curr);
		}
		Ok(())
	}
}

#[test]
fn test_ast_string() {
	let program = Program {
		statements: vec![
			Statement::LET(LetStatement { name: String::from("country"), value: Expression::IDENT(String::from("istanbul")) }),
			Statement::LET(LetStatement { name: String::from("neighborhood"), value: Expression::IDENT(String::from("maslak")) })
		]
	};

	let expected = "let country = istanbul;let neighborhood = maslak;";

	assert_eq!(program.to_string(), expected);
}

//=== AST END ====