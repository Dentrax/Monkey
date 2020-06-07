use std::{fmt};
use std::io;
use std::fmt::{Error, Formatter};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::ptr::hash;

const PROMPT: &str = ">> ";

fn main() {
	let mut input = String::new();
	let mut evaluator = Evaluator::new();

	loop {
		input.clear();

		println!("\nType: ");

		let s = match io::stdin().read_line(&mut input) {
			Ok(_) => {
				let (program, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

				if errs.len() > 0 {
					for err in errs {
						println!("ParserErr: {}", err);
					}
					continue;
				}

				match evaluator.eval(Node::PROGRAM(program)) {
					Ok(o) => {
						match o {
							Object::NULL => {}
							_ => println!("Evaluated:\n{}", o)
						}
					}
					Err(e) => {
						println!("EvalError: {}", e);
						continue;
					}
				}
			}
			Err(e) => {
				println!("StdinErr: {}", e);
				break;
			}
		};
	}

	println!("Reached end of the application!");
}

//=== OBJ BEGIN ===

pub const STR_FUNCTION: &'static str = "FUNCTION";
pub const STR_BUILTIN: &'static str = "BUILTIN";
pub const STR_ARRAY: &'static str = "ARRAY";
pub const STR_HASH: &'static str = "HASH";
pub const STR_INTEGER: &'static str = "INTEGER";
pub const STR_BOOLEAN: &'static str = "BOOLEAN";
pub const STR_STRING: &'static str = "STRING";
pub const STR_RETURN: &'static str = "RETURN";
pub const STR_ERROR: &'static str = "ERROR";
pub const STR_NULL: &'static str = "NULL";

pub const OBJ_NULL: Object = Object::NULL;
pub const OBJ_TRUE: Object = Object::BOOLEAN(true);
pub const OBJ_FALSE: Object = Object::BOOLEAN(false);

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	parameters: Vec<String>,
	body: BlockStatement,
	env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let parameters = self.parameters.join(", ");

		write!(f, "<fn({})> {{\n{}\n}}", parameters, self.body)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
	FUNCTION(Function),
	BUILTIN(Builtin),
	ARRAY(Array),
	HASH(Hash),
	INTEGER(isize),
	BOOLEAN(bool),
	STRING(String),
	RETURN(Box<Object>),
	ERROR(String),
	NULL,
}

impl Object {
	pub fn is_function(&self) -> bool {
		self.get_type() == STR_FUNCTION
	}

	pub fn is_builtin(&self) -> bool {
		self.get_type() == STR_BUILTIN
	}

	pub fn is_array(&self) -> bool {
		self.get_type() == STR_ARRAY
	}

	pub fn is_hash(&self) -> bool {
		self.get_type() == STR_HASH
	}

	pub fn is_integer(&self) -> bool {
		self.get_type() == STR_INTEGER
	}

	pub fn is_boolean(&self) -> bool {
		self.get_type() == STR_BOOLEAN
	}

	pub fn is_string(&self) -> bool {
		self.get_type() == STR_STRING
	}

	pub fn is_return(&self) -> bool {
		self.get_type() == STR_RETURN
	}

	pub fn is_error(&self) -> bool {
		self.get_type() == STR_ERROR
	}

	pub fn is_null(&self) -> bool {
		self.get_type() == STR_NULL
	}

	pub fn get_type(&self) -> &str {
		match self {
			Object::FUNCTION(_) => STR_FUNCTION,
			Object::BUILTIN(_) => STR_BUILTIN,
			Object::ARRAY(_) => STR_ARRAY,
			Object::HASH(_) => STR_HASH,
			Object::INTEGER(_) => STR_INTEGER,
			Object::BOOLEAN(_) => STR_BOOLEAN,
			Object::STRING(_) => STR_STRING,
			Object::RETURN(_) => STR_RETURN,
			Object::ERROR(_) => STR_ERROR,
			Object::NULL => STR_NULL,
		}
	}
}

impl fmt::Display for Object {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Object::FUNCTION(func) => func.fmt(f),
			Object::BUILTIN(b) => b.fmt(f),
			Object::ARRAY(a) => a.fmt(f),
			Object::HASH(h) => h.fmt(f),
			Object::INTEGER(i) => write!(f, "{}", i),
			Object::BOOLEAN(i) => write!(f, "{}", i),
			Object::STRING(s) => write!(f, "{}", s),
			Object::RETURN(r) => write!(f, "{}", r),
			Object::ERROR(e) => write!(f, "{}", e),
			Object::NULL => write!(f, ""),
		}
	}
}

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


//=== OBJ END ====


//=== BUILTIN BEGIN ====


#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
	LEN,
	FIRST,
	LAST,
	REST,
	REVERSE,
	PUSH,
}

impl fmt::Display for Builtin {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Builtin::LEN => write!(f, "len"),
			Builtin::FIRST => write!(f, "first"),
			Builtin::LAST => write!(f, "last"),
			Builtin::REST => write!(f, "rest"),
			Builtin::REVERSE => write!(f, "reverse"),
			Builtin::PUSH => write!(f, "push"),
		}
	}
}

impl Builtin {
	fn lookup(builtin: String) -> Option<Self> {
		match builtin.as_str() {
			"len" => Some(Builtin::LEN),
			"first" => Some(Builtin::FIRST),
			"last" => Some(Builtin::LAST),
			"rest" => Some(Builtin::REST),
			"reverse" => Some(Builtin::REVERSE),
			"push" => Some(Builtin::PUSH),
			_ => None
		}
	}

	pub fn apply(&self, args: Vec<Object>) -> Result<Object, EvalError> {
		match self {
			Builtin::LEN => builtin_len(&args),
			Builtin::FIRST => builtin_first(&args),
			Builtin::LAST => builtin_last(&args),
			Builtin::REST => builtin_rest(&args),
			Builtin::REVERSE => builtin_reverse(&args),
			Builtin::PUSH => builtin_push(&args),
		}
	}
}

fn builtin_len(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::STRING(str) => Ok(Object::INTEGER(str.len() as isize)),
		Object::ARRAY(a) => Ok(Object::INTEGER(a.elements.len() as isize)),
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::LEN, args[0].clone()))
	}
}

fn builtin_first(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(a.elements.first().unwrap().clone())
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::FIRST, args[0].clone()))
	}
}

fn builtin_last(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(a.elements.last().unwrap().clone())
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::LAST, args[0].clone()))
	}
}

fn builtin_rest(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(Object::ARRAY(Array{elements: a.elements.iter().skip(1).cloned().collect()}))
			} else {
				Ok(OBJ_NULL)
			}
		},
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::REST, args[0].clone()))
	}
}

fn builtin_reverse(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 1 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match &args[0] {
		Object::ARRAY(a) => {
			if !a.elements.is_empty() {
				Ok(Object::ARRAY(Array{elements: a.elements.clone().into_iter().rev().collect()}))
			} else {
				Ok(OBJ_NULL)
			}
		},
		Object::STRING(str) => {
			Ok(Object::STRING(str.chars().rev().collect::<String>()))
		}
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::REVERSE, args[0].clone()))
	}
}

fn builtin_push(args: &[Object]) -> Result<Object, EvalError> {
	if args.len() != 2 {
		return Err(EvalError::BUILTIN_ERROR_LEN(args.len()))
	}

	match (&args[0], &args[1]) {
		(Object::ARRAY(a), Object::INTEGER(i)) | (Object::INTEGER(i), Object::ARRAY(a)) => {
			let mut result = a.elements.clone();
			result.push(Object::INTEGER(*i));

			Ok(Object::ARRAY(Array{elements: result.clone()}))
		}
		(Object::ARRAY(l), Object::ARRAY(r)) => {
			let mut result = r.elements.clone();
			result.push(args[0].clone());

			Ok(Object::ARRAY(Array{elements: result.clone()}))
		}
		_ => Err(EvalError::UNSUPPORTED_BUILTIN_USAGE(Builtin::PUSH, args[0].clone()))
	}
}

//=== BUILTIN END ====


//=== ARRAY BEGIN ====

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
	elements: Vec<Object>,
}

impl fmt::Display for Array {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let elems = self.elements.iter().map(|o| format!("{}", o)).collect::<Vec<String>>();
		write!(f, "[{}]", elems.join(", "))
	}
}


//=== ARRAY END ====


//=== HASH BEGIN ====


#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Hashable {
	BOOLEAN(bool),
	INTEGER(isize),
	STRING(String),
}

impl fmt::Display for Hashable {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Hashable::STRING(s) => s.fmt(f),
			Hashable::INTEGER(i) => i.fmt(f),
			Hashable::BOOLEAN(b) => b.fmt(f),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hash {
	pairs: HashMap<Hashable, Object>,
}

impl fmt::Display for Hash {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut pairs = vec![];
		for pair in &self.pairs {
			pairs.push(format!(r#"{}: {}"#, pair.0, pair.1));
		}
		pairs.sort();
		write!(f, "{{{}}}", pairs.join(", "))
	}
}

//=== HASH END ====



//=== ENVIRONMENT BEGIN ===

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
	store: HashMap<String, Object>,
	outer: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
	fn default() -> Environment {
		Environment { store: HashMap::new(), outer: None }
	}
}

impl Environment {
	fn new() -> Self {
		return Environment::default();
	}

	fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
		let mut env = Environment::default();
		env.outer = Some(outer);
		env
	}

	fn get(&self, key: &str) -> Option<Object> {
		match self.store.get(key) {
			Some(obj) => {
				Some(obj.clone())
			}
			None => match self.outer {
				Some(ref outer) => outer.borrow_mut().get(key),
				None => None
			},
		}
	}

	fn set(&mut self, key: String, obj: &Object) {
		self.store.insert(key, obj.clone());
	}
}

//=== ENVIRONMENT END ===


//=== EVAL START ====

#[derive(Debug)]
pub enum EvalError {
	UNKNOWN_OPERATOR_PREFIX(Object, PrefixType),
	UNKNOWN_OPERATOR_INFIX(Object, InfixType, Object),
	UNKNOWN_EXPRESSION(Expression),
	UNKNOWN_IDENTIFIER(String),
	UNKNOWN_HASHABLE(Object),
	TYPE_MISMATCH(Object, InfixType, Object),
	UNSUPPORTED_OBJECT(Object),
	UNKNOWN_BUILTIN(String),
	BUILTIN_ERROR_LEN(usize),
	UNSUPPORTED_BUILTIN_USAGE(Builtin, Object),
}

impl fmt::Display for EvalError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			EvalError::UNKNOWN_OPERATOR_PREFIX(o, p) => {
				write!(f, "unknown operator: {}{}", p, o.get_type())
			}
			EvalError::UNKNOWN_OPERATOR_INFIX(l, i, r) => {
				write!(f, "unknown operator: {} {} {}", l.get_type(), i, r.get_type())
			}
			EvalError::UNKNOWN_EXPRESSION(e) => {
				write!(f, "expression not found: {}", e)
			}
			EvalError::UNKNOWN_IDENTIFIER(e) => {
				write!(f, "identifier not found: {}", e)
			}
			EvalError::UNKNOWN_HASHABLE(e) => {
				write!(f, "unusable as hash key: {}", e.get_type())
			}
			EvalError::TYPE_MISMATCH(l, t, r) => {
				write!(f, "type mismatch: {} {} {}", l.get_type(), t, r.get_type())
			}
			EvalError::UNSUPPORTED_OBJECT(o) => {
				write!(f, "unsupported object: {}", o.get_type())
			}
			EvalError::UNKNOWN_BUILTIN(s) => {
				write!(f, "unknown builtin {}", s)
			}
			EvalError::BUILTIN_ERROR_LEN(l) => {
				write!(f, "wrong number of arguments. got={}, want=1", l)
			}
			EvalError::UNSUPPORTED_BUILTIN_USAGE(b, o) => {
				write!(f, "argument to '{}' not supported, got {}", b, o.get_type())
			}
		}
	}
}

pub struct Evaluator {
	environment: Rc<RefCell<Environment>>
}

impl Evaluator {
	pub fn new() -> Self {
		Evaluator {
			environment: Rc::new(RefCell::new(Environment::default()))
		}
	}

	fn eval(&mut self, node: Node) -> Result<Object, EvalError> {
		match node {
			Node::PROGRAM(p) => self.eval_program(p),
			Node::STATEMENT(s) => match s {
				Statement::EXPRESSION(e) => self.eval(Node::EXPRESSION(e)),
				Statement::BLOCK(b) => self.eval_statement_block(b),
				Statement::RETURN(r) => self.eval_statement_return(r),
				Statement::LET(l) => self.eval_statement_let(l),
				_ => unimplemented!(),
			},
			Node::EXPRESSION(e) => match e {
				Expression::LITERAL(l) => match l {
					Literal::INT(i) => Ok(Object::INTEGER(i)),
					Literal::BOOL(i) => match i {
						true => Ok(OBJ_TRUE),
						false => Ok(OBJ_FALSE)
					},
					Literal::STRING(s) => Ok(Object::STRING(s)),
					_ => unimplemented!(),
				}
				Expression::PREFIX(p) => self.eval_expression_prefix(p),
				Expression::INFIX(p) => self.eval_expression_infix(p),
				Expression::IF(p) => self.eval_expression_if(p),
				Expression::IDENT(i) => self.eval_expression_ident(i),
				Expression::FUNCTION(f) => self.eval_expression_function(f),
				Expression::CALL(c) => self.eval_expression_call(c),
				Expression::ARRAY(a) => self.eval_expression_array(a),
				Expression::INDEX(i) => self.eval_expression_index(i),
				Expression::HASH(h) => self.eval_expression_hash(h),
				_ => Err(EvalError::UNKNOWN_EXPRESSION(e))
			}
			_ => unimplemented!(),
		}
	}

	fn eval_program(&mut self, program: Program) -> Result<Object, EvalError> {
		let mut result = Object::NULL;
		for stmt in program.statements {
			result = self.eval(Node::STATEMENT(stmt))?;

			if let Object::RETURN(value) = result {
				return Ok(*value);
			}
		}
		Ok(result)
	}

	fn eval_statement_block(&mut self, block: BlockStatement) -> Result<Object, EvalError> {
		let mut result = OBJ_NULL;
		for stmt in block.statements {
			result = self.eval(Node::STATEMENT(stmt))?;

			if let Object::RETURN(value) = result {
				return Ok(Object::RETURN(value));
			}
		}
		Ok(result)
	}

	fn eval_statement_let(&mut self, stmt: LetStatement) -> Result<Object, EvalError> {
		let value = self.eval(Node::EXPRESSION(stmt.value))?;
		self.environment.borrow_mut().set(stmt.name, &value);

		Ok(value)
	}

	fn eval_statement_return(&mut self, ret: ReturnStatement) -> Result<Object, EvalError> {
		let value = self.eval(Node::EXPRESSION(ret.value))?;
		Ok(Object::RETURN(Box::new(value)))
	}

	fn eval_expression_prefix(&mut self, expr: PrefixExpression) -> Result<Object, EvalError> {
		let right = self.eval(Node::EXPRESSION(*expr.right))?;

		match expr.operator {
			PrefixType::BANG => match right {
				Object::BOOLEAN(b) => match b {
					true => Ok(OBJ_FALSE),
					false => Ok(OBJ_TRUE)
				},
				Object::NULL => Ok(OBJ_NULL),
				_ => Ok(OBJ_FALSE),
			},
			PrefixType::MINUS => match right {
				Object::INTEGER(i) => Ok(Object::INTEGER(-i)),
				_ => Err(EvalError::UNKNOWN_OPERATOR_PREFIX(right, expr.operator))
			},
			_ => Err(EvalError::UNKNOWN_OPERATOR_PREFIX(right, expr.operator))
		}
	}

	fn eval_expression_infix(&mut self, expr: InfixExpression) -> Result<Object, EvalError> {
		let left = self.eval(Node::EXPRESSION(*expr.left))?;
		let right = self.eval(Node::EXPRESSION(*expr.right))?;

		match (left, right) {
			(Object::INTEGER(l), Object::INTEGER(r)) => match expr.operator {
				InfixType::PLUS => Ok(Object::INTEGER(l + r)),
				InfixType::MINUS => Ok(Object::INTEGER(l - r)),
				InfixType::MULTIPLICATION => Ok(Object::INTEGER(l * r)),
				InfixType::DIVISION => Ok(Object::INTEGER(l / r)),

				InfixType::LT => {
					Ok(self.eval_expression_infix_condition(l < r))
				}
				InfixType::GT => {
					Ok(self.eval_expression_infix_condition(l > r))
				}
				InfixType::EQ => {
					Ok(self.eval_expression_infix_condition(l == r))
				}
				InfixType::NEQ => {
					Ok(self.eval_expression_infix_condition(l != r))
				}

				_ => Err(EvalError::UNKNOWN_OPERATOR_INFIX(Object::INTEGER(l), expr.operator, Object::INTEGER(r)))
			},
			(Object::BOOLEAN(l), Object::BOOLEAN(r)) => match expr.operator {
				InfixType::EQ => {
					Ok(self.eval_expression_infix_condition(l == r))
				}
				InfixType::NEQ => {
					Ok(self.eval_expression_infix_condition(l != r))
				}
				_ => Err(EvalError::UNKNOWN_OPERATOR_INFIX(Object::BOOLEAN(l), expr.operator, Object::BOOLEAN(r)))
			},
			(Object::STRING(l), Object::STRING(r)) => {
				Ok(self.eval_expression_infix_string(l, expr.operator, r)?)
			}
			(l, r) => Err(EvalError::TYPE_MISMATCH(l, expr.operator, r)),
			_ => unimplemented!(),
		}
	}

	fn eval_expression_if(&mut self, expr: IfExpression) -> Result<Object, EvalError> {
		let condition = self.eval(Node::EXPRESSION(*expr.condition))?;

		if self.eval_expression_if_truthy(&condition) {
			return self.eval(Node::STATEMENT(Statement::BLOCK(expr.consequence)));
		} else if let Some(alt) = expr.alternative {
			return self.eval(Node::STATEMENT(Statement::BLOCK(alt)));
		} else {
			Ok(OBJ_NULL)
		}
	}

	fn eval_expression_if_truthy(&self, obj: &Object) -> bool {
		match obj {
			Object::NULL => false,
			Object::BOOLEAN(value) => *value,
			_ => true,
		}
	}

	fn eval_expression_infix_condition(&self, condition: bool) -> Object {
		if condition {
			OBJ_TRUE
		} else {
			OBJ_FALSE
		}
	}

	fn eval_expression_infix_string(&self, l: String, operator: InfixType, r: String) -> Result<Object, EvalError> {
		let result = match operator {
			InfixType::PLUS => {
				let mut str = l.to_owned();
				str.push_str(&r.to_owned());
				Object::STRING(str)
			}
			InfixType::EQ => {
				self.eval_expression_infix_condition(l == r)
			}
			InfixType::NEQ => {
				self.eval_expression_infix_condition(l != r)
			}
			_ => return Err(EvalError::UNKNOWN_OPERATOR_INFIX(Object::STRING(l), operator, Object::STRING(r)))
		};

		Ok(result)
	}

	fn eval_expression_ident(&self, ident: String) -> Result<Object, EvalError> {
		match Builtin::lookup(ident.clone()) {
			Some(f) => return Ok(Object::BUILTIN(f)),
			None => {
				match self.environment.borrow_mut().get(&ident) {
					Some(value) => Ok(value),
					None => {
						Err(EvalError::UNKNOWN_IDENTIFIER(ident.clone()))
					}
				}
			}
		}
	}

	fn eval_expression_function(&self, func: FunctionLiteral) -> Result<Object, EvalError> {
		Ok(Object::FUNCTION(Function { parameters: func.parameters.clone(), body: func.body.clone(), env: Rc::clone(&self.environment) }))
	}

	fn eval_expression_call(&mut self, call: CallExpression) -> Result<Object, EvalError> {
		let func = self.eval(Node::EXPRESSION(*call.function))?;
		let args = self.eval_expressions(call.arguments)?;
		return self.apply_function(func, args);
	}

	fn eval_expression_array(&mut self, array: ArrayLiteral) -> Result<Object, EvalError> {
		Ok(Object::ARRAY(Array{elements: self.eval_expressions(array.elements)?}))
	}

	fn eval_expression_index(&mut self, index: IndexExpression) -> Result<Object, EvalError> {
		let left = self.eval(Node::EXPRESSION(*index.left))?;
		let index = self.eval(Node::EXPRESSION(*index.index))?;

		match (left, index) {
			(Object::ARRAY(a), Object::INTEGER(i)) => {
				match a.elements.get(i as usize) {
					Some(e) => Ok(e.clone()),
					None => Ok(OBJ_NULL),
				}
			}
			(Object::HASH(hash), obj) => {
				let key = match obj {
					Object::STRING(s) => Hashable::STRING(s),
					Object::INTEGER(i) => Hashable::INTEGER(i),
					Object::BOOLEAN(b) => Hashable::BOOLEAN(b),
					_ => return Err(EvalError::UNKNOWN_HASHABLE(obj))
				};
				match hash.pairs.get(&key) {
					Some(v) => Ok(v.clone()),
					None => Ok(OBJ_NULL)
				}
			}
			_ => unimplemented!(),
		}
	}

	fn eval_expression_hash(&mut self, hash: HashLiteral) -> Result<Object, EvalError> {

		let mut pairs = HashMap::new();

		for (k, v) in hash.pairs {

			let key = match self.eval(Node::EXPRESSION(k))? {
				Object::STRING(s) => Hashable::STRING(s),
				Object::INTEGER(b) => Hashable::INTEGER(b),
				Object::BOOLEAN(b) => Hashable::BOOLEAN(b),

				_ => unimplemented!(),
			};

			pairs.insert(key, self.eval(Node::EXPRESSION(v))?);
		}

		Ok(Object::HASH(Hash { pairs }))
	}

	fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Result<Object, EvalError> {
		if let Object::FUNCTION(f) = func {
			let old_env = Rc::clone(&self.environment);
			let mut new_env = Environment::new_enclosed(Rc::clone(&f.env));

			for (i, param) in f.parameters.iter().enumerate() {
				new_env.set(param.to_string(), args.get(i).unwrap())
			}

			self.environment = Rc::new(RefCell::new(new_env));

			let evaluated = self.eval(Node::STATEMENT(Statement::BLOCK(f.body)));

			self.environment = old_env;

			return Ok(evaluated)?;
		}

		if let Some(b) = Builtin::lookup(func.to_string()) {
			return Ok(b.apply(args)?)
		}

		Err(EvalError::UNSUPPORTED_OBJECT(func))
	}

	fn eval_expressions(&mut self, exprs: Vec<Expression>) -> Result<Vec<Object>, EvalError> {
		let mut results = vec![];
		for expr in exprs {
			results.push(self.eval(Node::EXPRESSION(expr))?);
		}
		Ok(results)
	}
}

fn test_eval(input: &str) -> Result<Object, EvalError> {
	let lexer = Lexer::new(input.to_owned());
	let mut parser = Parser::new(lexer);
	let mut evaluator = Evaluator::new();

	let (actual, errs) = parser.parse();

	if errs.len() == 0 {
		return evaluator.eval(Node::PROGRAM(actual));
	}

	panic!("fail!");
}

#[test]
fn test_eval_expression_integer() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "7",
			expected: 7,
		},
		Test {
			input: "-7",
			expected: -7,
		},
		Test {
			input: "15",
			expected: 15,
		},
		Test {
			input: "-15",
			expected: -15,
		},
		Test {
			input: "5 + 5 + 5 + 5 - 10",
			expected: 10,
		},
		Test {
			input: "2 * 2 * 2 * 2 * 2",
			expected: 32,
		},
		Test {
			input: "-50 + 100 + -50",
			expected: 0,
		},
		Test {
			input: "5 * 2 + 10",
			expected: 20,
		},
		Test {
			input: "5 + 2 * 10",
			expected: 25,
		},
		Test {
			input: "20 + 2 * -10",
			expected: 0,
		},
		Test {
			input: "50 / 2 * 2 + 10",
			expected: 60,
		},
		Test {
			input: "2 * (5 + 10)",
			expected: 30,
		},
		Test {
			input: "3 * 3 * 3 + 10",
			expected: 37,
		},
		Test {
			input: "3 * (3 * 3) + 10",
			expected: 37,
		},
		Test {
			input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
			expected: 50,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_expression_boolean() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: "true",
			expected: true,
		},
		Test {
			input: "false",
			expected: false,
		},
		Test {
			input: "1 < 2",
			expected: true,
		},
		Test {
			input: "1 > 2",
			expected: false,
		},
		Test {
			input: "1 < 1",
			expected: false,
		},
		Test {
			input: "1 > 1",
			expected: false,
		},
		Test {
			input: "1 == 1",
			expected: true,
		},
		Test {
			input: "1 != 1",
			expected: false,
		},
		Test {
			input: "1 == 2",
			expected: false,
		},
		Test {
			input: "1 != 2",
			expected: true,
		},
		Test {
			input: "true == true",
			expected: true,
		},
		Test {
			input: "false == false",
			expected: true,
		},
		Test {
			input: "true == false",
			expected: false,
		},
		Test {
			input: "true != false",
			expected: true,
		},
		Test {
			input: "false != true",
			expected: true,
		},
		Test {
			input: "(1 < 2) == true",
			expected: true,
		},
		Test {
			input: "(1 < 2) == false",
			expected: false,
		},
		Test {
			input: "(1 > 2) == true",
			expected: false,
		},
		Test {
			input: "(1 > 2) == false",
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_expression_string() {
	struct Test<'a> {
		input: &'a str,
		expected: String,
	}

	let tests = vec![
		Test {
			input: r#""Hello World!""#,
			expected: String::from("Hello World!"),
		},
		Test {
			input: r#""Hello" + " " + "World!""#,
			expected: String::from("Hello World!"),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::STRING(test.expected));
	}
}

#[test]
fn test_eval_expression_string_equality() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: r#""foo" == "foo""#,
			expected: true,
		},
		Test {
			input: r#""foo" != "foo""#,
			expected: false,
		},
		Test {
			input: r#""foo" == "bar""#,
			expected: false,
		},
		Test {
			input: r#""foo" != "bar""#,
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_expression_if_else() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "if (true) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (false) { 10 }",
			expected: OBJ_NULL,
		},
		Test {
			input: "if (1) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (1 < 2) { 10 }",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "if (1 > 2) { 10 }",
			expected: OBJ_NULL,
		},
		Test {
			input: "if (1 > 2) { 10 } else { 20 }",
			expected: Object::INTEGER(20),
		},
		Test {
			input: "if (1 < 2) { 10 } else { 20 }",
			expected: Object::INTEGER(10),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_expression_array_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "[1, 2, 3][0]",
			expected: Object::INTEGER(1),
		},
		Test {
			input: "[1, 2, 3][1]",
			expected: Object::INTEGER(2),
		},
		Test {
			input: "[1, 2, 3][2]",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let i = 0; [1][i];",
			expected: Object::INTEGER(1),
		},
		Test {
			input: "[1, 2, 3][1 + 1];",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let myArray = [1, 2, 3]; myArray[2];",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
			expected: Object::INTEGER(6),
		},
		Test {
			input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
			expected: Object::INTEGER(2),
		},
		Test {
			input: "[1, 2, 3][3]",
			expected: OBJ_NULL,
		},
		Test {
			input: "[1, 2, 3][-1]",
			expected: OBJ_NULL,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_return() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "return 10;",
			expected: Object::INTEGER(10),
		},
		Test {
			input: "return 11; 9;",
			expected: Object::INTEGER(11),
		},
		Test {
			input: "return 2 * 6; 9;",
			expected: Object::INTEGER(12),
		},
		Test {
			input: "9; return 2 * 7; 9;",
			expected: Object::INTEGER(14),
		},
		Test {
			input: "if (2 > 1) { if (3 > 1) { return 7; } return 0; }",
			expected: Object::INTEGER(7),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_operator_bang() {
	struct Test<'a> {
		input: &'a str,
		expected: bool,
	}

	let tests = vec![
		Test {
			input: "!true",
			expected: false,
		},
		Test {
			input: "!false",
			expected: true,
		},
		Test {
			input: "!7",
			expected: false,
		},
		Test {
			input: "!!true",
			expected: true,
		},
		Test {
			input: "!!false",
			expected: false,
		},
		Test {
			input: "!!7",
			expected: true,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::BOOLEAN(test.expected));
	}
}

#[test]
fn test_eval_handle_error() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "7 + true;",
			expected: "type mismatch: INTEGER + BOOLEAN",
		},
		Test {
			input: "7 + true; 7;",
			expected: "type mismatch: INTEGER + BOOLEAN",
		},
		Test {
			input: "-true",
			expected: "unknown operator: -BOOLEAN",
		},
		Test {
			input: "true + false;",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "7; true + false; 7",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "if (10 > 1) { true + false; }",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
			expected: "unknown operator: BOOLEAN + BOOLEAN",
		},
		Test {
			input: "foobar",
			expected: "identifier not found: foobar",
		},
		Test {
			input: "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); x",
			expected: "identifier not found: x",
		},
		Test {
			input: r#""Hello" - "World""#,
			expected: "unknown operator: STRING - STRING",
		},
		Test {
			input: "len(1)",
			expected: "argument to 'len' not supported, got INTEGER",
		},
		Test {
			input: r#"len("one", "two")"#,
			expected: "wrong number of arguments. got=2, want=1",
		},
		Test {
			input: r#"{"name": "Monkey"}[fn(x) { x }];"#,
			expected: "unusable as hash key: FUNCTION",
		},
	];

	for test in tests {
		match test_eval(test.input) {
			Ok(e) => {
				panic!("expected error: {}, got: {} instead, for: {}", test.expected, e, test.input)
			}
			Err(e) => {
				assert_eq!(e.to_string(), test.expected);
			}
		}
	}
}

#[test]
fn test_eval_statement_let() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "let a = 7; a;",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "let a = 7 * 7; a;",
			expected: Object::INTEGER(49),
		},
		Test {
			input: "let a = 7; let b = a; b;",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "let a = 7; let b = a; let c = a + b + 7; c;",
			expected: Object::INTEGER(21),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: "fn(x) { x + 2; };",
			expected: Object::FUNCTION(Function {
				parameters: vec![String::from("x")],
				body: BlockStatement {
					statements: vec![
						Statement::EXPRESSION(
							Expression::INFIX(
								InfixExpression {
									left: Box::new(Expression::IDENT(String::from("x"))),
									operator: InfixType::PLUS,
									right: Box::new(Expression::LITERAL(Literal::INT(2))),
								}
							)
						)
					]
				},
				env: Rc::new(RefCell::new(Environment::new())),
			}),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_call() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "let identity = fn(x) { x; }; identity(7);",
			expected: 7,
		},
		Test {
			input: "let identity = fn(x) { return x; }; identity(15);",
			expected: 15,
		},
		Test {
			input: "let double = fn(x) { x * 34; }; double(2);",
			expected: 68,
		},
		Test {
			input: "let add = fn(x, y) { x + y; }; add(7, 7);",
			expected: 14,
		},
		Test {
			input: "let add = fn(x, y) { x + y; }; add(1 + 3, add(5, 7));",
			expected: 16,
		},
		Test {
			input: "fn(x) { x; } (35)",
			expected: 35,
		},
		Test {
			input: "let add = fn(a, b) { a + b }; let sub = fn(a, b) { a - b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(2, 2, add);",
			expected: 4,
		},
		Test {
			input: "let add = fn(a, b) { a + b }; let sub = fn(a, b) { a - b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(10, 2, sub);",
			expected: 8,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_statement_function_call_closures() {
	struct Test<'a> {
		input: &'a str,
		expected: isize,
	}

	let tests = vec![
		Test {
			input: "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(14); addTwo(35);",
			expected: 49,
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::INTEGER(test.expected));
	}
}

#[test]
fn test_eval_statement_function_builtin() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"len("")"#,
			expected: Object::INTEGER(0),
		},
		Test {
			input: r#"len("four")"#,
			expected: Object::INTEGER(4),
		},
		Test {
			input: r#"len("hello world")"#,
			expected: Object::INTEGER(11),
		},
		Test {
			input: "len([])",
			expected: Object::INTEGER(0),
		},
		Test {
			input: "len([1, 2, 3])",
			expected: Object::INTEGER(3),
		},
		Test {
			input: "first([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "first([7, 11, 13])",
			expected: Object::INTEGER(7),
		},
		Test {
			input: "last([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "last([7, 11, 13])",
			expected: Object::INTEGER(13),
		},
		Test {
			input: "rest([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "rest([7, 11, 13])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(11), Object::INTEGER(13)]}),
		},
		Test {
			input: "reverse([])",
			expected: OBJ_NULL,
		},
		Test {
			input: "reverse([7, 11, 13])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(13), Object::INTEGER(11), Object::INTEGER(7)]}),
		},
		Test {
			input: r#"reverse("Hello World!")"#,
			expected: Object::STRING(String::from("!dlroW olleH")),
		},
		Test {
			input: "push(1, [])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1)]}),
		},
		Test {
			input: "push(3, [1, 2])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)]}),
		},
		Test {
			input: "push([1, 2], 3)",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::INTEGER(3)]}),
		},
		Test {
			input: "push([3, 4], [1, 2])",
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(1), Object::INTEGER(2), Object::ARRAY(Array{elements: vec![Object::INTEGER(3), Object::INTEGER(4)]})]}),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_builtin_integration() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"
			let map = fn(arr, f) {
				let iter = fn(arr, accumulated) {
					if (len(arr) == 0) {
						accumulated
					} else {
						iter(rest(arr), push(accumulated, f(first(arr))));
					}
				};
				iter(arr, []);
			};
			let a = [1, 2, 3, 4];
			let double = fn(x) { x * 2 };
			map(a, double);
			"#,
			expected: Object::ARRAY(Array{elements: vec![Object::INTEGER(2), Object::INTEGER(4), Object::INTEGER(6), Object::INTEGER(8)]}),
		},
		Test {
			input: r#"
			let reduce = fn(arr, initial, f) {
				let iter = fn(arr, result) {
					if (len(arr) == 0) {
						result
					} else {
						iter(rest(arr), f(result, first(arr)));
					}
				};
				iter(arr, initial);
			};
			let sum = fn(arr) {
				reduce(arr, 0, fn(initial, el) { initial + el });
			};
			sum([1, 2, 3, 4, 5]);
			"#,
			expected: Object::INTEGER(15),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_array() {
	struct Test<'a> {
		input: &'a str,
		expected: Array,
	}

	let tests = vec![
		Test {
			input: "[1, 2 * 2, 3 + 3]",
			expected: Array{elements: vec![Object::INTEGER(1), Object::INTEGER(4), Object::INTEGER(6)]},
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::ARRAY(test.expected));
	}
}

macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

#[test]
fn test_eval_statement_function_hash() {
	struct Test<'a> {
		input: &'a str,
		expected: Hash,
	}

	let tests = vec![
		Test {
			input: r#"
			let two = "two";
			{
				"one": 10 - 9,
				two: 1 + 1,
				"thr" + "ee": 6 / 2,
				4: 4,
				true: 5,
				false: 6
			}
			"#,
			expected: Hash{ pairs: hashmap![
				Hashable::STRING(String::from("two")) => Object::INTEGER(2),
				Hashable::STRING(String::from("one")) => Object::INTEGER(1),
				Hashable::STRING(String::from("three")) => Object::INTEGER(3),
				Hashable::BOOLEAN(true) => Object::INTEGER(5),
				Hashable::BOOLEAN(false) => Object::INTEGER(6),
				Hashable::INTEGER(4) => Object::INTEGER(4)
			]},
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::HASH(test.expected));
	}
}

#[test]
fn test_eval_statement_function_hash_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Object,
	}

	let tests = vec![
		Test {
			input: r#"{"foo": 5}["foo"]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{"foo": 5}["bar"]"#,
			expected: OBJ_NULL,
		},
		Test {
			input: r#"let key = "foo"; {"foo": 5}[key]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{}["foo"]"#,
			expected: OBJ_NULL,
		},
		Test {
			input: r#"{5: 5}[5]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{true: 5}[true]"#,
			expected: Object::INTEGER(5),
		},
		Test {
			input: r#"{false: 5}[false]"#,
			expected: Object::INTEGER(5),
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, test.expected);
	}
}

#[test]
fn test_eval_statement_function_hash_integration() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: r#"
			let people = [{"name": "Alice", "age": 24}, {"name": "Anna", "age": 28}];
			let getAge = fn(person) { person["name"]; };
			return getAge(people[0]) + getAge(people[1]);
			"#,
			expected: "AliceAnna",
		},
	];

	for test in tests {
		let evaluated = test_eval(test.input).unwrap();
		assert_eq!(evaluated, Object::STRING(String::from(test.expected)));
	}
}

//=== EVAL END ====


//=== AST BEGIN ===

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
	elements: Vec<Expression>
}

impl fmt::Display for ArrayLiteral {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let elems: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect::<Vec<String>>();
		write!(f, "[{}]", elems.join(", "))
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct HashLiteral {
	pairs: Vec<(Expression, Expression)>
}

impl fmt::Display for HashLiteral {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let pairs = self.pairs.iter().map(|(k, v)| format!(r#"{}: {}"#, k, v)).collect::<Vec<String>>();
		write!(f, "{{{}}}", pairs.join(", "))
	}
}


#[derive(Clone, Debug, PartialEq)]
pub struct IndexExpression {
	left: Box<Expression>,
	index: Box<Expression>,
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
pub struct IfExpression {
	condition: Box<Expression>,
	consequence: BlockStatement,
	alternative: Option<BlockStatement>,
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
	parameters: Vec<String>,
	body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "fn({}) {{ {} }}", self.parameters.join(", "), self.body)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpression {
	function: Box<Expression>,
	arguments: Vec<Expression>,
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
pub struct BlockStatement {
	statements: Vec<Statement>,
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
			expected: "((-a) * b)",
		},
		Test {
			input: "!-a",
			expected: "(!(-a))",
		},
		Test {
			input: "a + b + c",
			expected: "((a + b) + c)",
		},
		Test {
			input: "a + b - c",
			expected: "((a + b) - c)",
		},
		Test {
			input: "a * b * c",
			expected: "((a * b) * c)",
		},
		Test {
			input: "a * b / c",
			expected: "((a * b) / c)",
		},
		Test {
			input: "a + b / c",
			expected: "(a + (b / c))",
		},
		Test {
			input: "a + b * c + d / e - f",
			expected: "(((a + (b * c)) + (d / e)) - f)",
		},
		Test {
			input: "3 + 4; -5 * 5",
			expected: "(3 + 4)((-5) * 5)",
		},
		Test {
			input: "5 > 4 == 3 < 4",
			expected: "((5 > 4) == (3 < 4))",
		},
		Test {
			input: "5 < 4 != 3 > 4",
			expected: "((5 < 4) != (3 > 4))",
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
fn test_ast_operator_precedence_group_expression_string() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "1 + (2 + 3) + 4",
			expected: "((1 + (2 + 3)) + 4)",
		},
		Test {
			input: "(5 + 5) * 2",
			expected: "((5 + 5) * 2)",
		},
		Test {
			input: "2 / (5 + 5)",
			expected: "(2 / (5 + 5))",
		},
		Test {
			input: "-(5 + 5)",
			expected: "(-(5 + 5))",
		},
		Test {
			input: "!(true == true)",
			expected: "(!(true == true))",
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.to_string(), test.expected);
	}
}

#[test]
fn test_ast_operator_precedence_call() {
	struct Test<'a> {
		input: &'a str,
		expected: &'a str,
	}

	let tests = vec![
		Test {
			input: "a + add(b * c) + d",
			expected: "((a + add((b * c))) + d)",
		},
		Test {
			input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		},
		Test {
			input: "add(a + b + c * d / f + g)",
			expected: "add((((a + b) + ((c * d) / f)) + g))",
		},
		Test {
			input: "a * [1, 2, 3, 4][b * c] * d",
			expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
		},
		Test {
			input: "add(a * b[2], b[1], 2 * [1, 2][1])",
			expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.to_string(), test.expected);
	}
}

#[test]
fn test_parse_statement_let() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "let x = 7;",
			expected: Statement::LET(LetStatement { name: String::from("x"), value: Expression::LITERAL(Literal::INT(7)) }),
		},
		Test {
			input: "let y = true;",
			expected: Statement::LET(LetStatement { name: String::from("y"), value: Expression::LITERAL(Literal::BOOL(true)) }),
		},
		Test {
			input: "let z = y;",
			expected: Statement::LET(LetStatement { name: String::from("z"), value: Expression::IDENT(String::from("y")) }),
		},
		Test {
			input: "let w = \"furkan\";",
			expected: Statement::LET(LetStatement { name: String::from("w"), value: Expression::LITERAL(Literal::STRING(String::from("furkan"))) }),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_return() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "return 15;",
			expected: Statement::RETURN(ReturnStatement { value: Expression::LITERAL(Literal::INT(15)) }),
		},
		Test {
			input: "return x;",
			expected: Statement::RETURN(ReturnStatement { value: Expression::IDENT(String::from("x")) }),
		},
		Test {
			input: "return \"string\";",
			expected: Statement::RETURN(ReturnStatement { value: Expression::LITERAL(Literal::STRING(String::from("string"))) }),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
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
fn test_parse_statement_expression_string() {
	let input = r#""helloworld!";"hello world!";"hello+world!";"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("helloworld!")))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("hello world!")))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::STRING(String::from("hello+world!")))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_boolean() {
	let input = "true;false;";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::LITERAL(Literal::BOOL(true))),
		Statement::EXPRESSION(Expression::LITERAL(Literal::BOOL(false))),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_prefix() {
	let input = "!7;-15;!true;!false";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::INT(7))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::BOOL(true))) })),
		Statement::EXPRESSION(Expression::PREFIX(PrefixExpression { operator: PrefixType::BANG, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
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
7 != 15;
true == true;
true != false;
false == false;"#;

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::PLUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MINUS, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::MULTIPLICATION, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::DIVISION, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::GT, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::LT, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::INT(7))), operator: InfixType::NEQ, right: Box::new(Expression::LITERAL(Literal::INT(15))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(true))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::BOOL(true))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(true))), operator: InfixType::NEQ, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
		Statement::EXPRESSION(Expression::INFIX(InfixExpression { left: Box::new(Expression::LITERAL(Literal::BOOL(false))), operator: InfixType::EQ, right: Box::new(Expression::LITERAL(Literal::BOOL(false))) })),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_if() {
	let input = "if (x < y) { z }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::IF(
				IfExpression {
					condition: Box::new(
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::IDENT(String::from("x"))),
								operator: InfixType::LT,
								right: Box::new(Expression::IDENT(String::from("y"))),
							}
						)
					),
					consequence: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("z")))
						]
					},
					alternative: None,
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_if_else() {
	let input = "if (x < y) { z } else { w }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::IF(
				IfExpression {
					condition: Box::new(
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::IDENT(String::from("x"))),
								operator: InfixType::LT,
								right: Box::new(Expression::IDENT(String::from("y"))),
							}
						)
					),
					consequence: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("z")))
						]
					},
					alternative: Some(BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::IDENT(String::from("w"))),
						]
					}),
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_function() {
	let input = "fn(x, y) { z + w; }";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::FUNCTION(
				FunctionLiteral {
					parameters: vec![
						String::from("x"),
						String::from("y"),
					],
					body: BlockStatement {
						statements: vec![
							Statement::EXPRESSION(Expression::INFIX(
								InfixExpression {
									left: Box::new(Expression::IDENT(String::from("z"))),
									operator: InfixType::PLUS,
									right: Box::new(Expression::IDENT(String::from("w"))),
								}
							)),
						],
					},
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_function_parameters() {
	struct Test<'a> {
		input: &'a str,
		expected: Vec<String>,
	}

	let tests = vec![
		Test {
			input: "fn() {};",
			expected: vec![],
		},
		Test {
			input: "fn(x) {};",
			expected: vec![String::from("x")],
		},
		Test {
			input: "fn(x, y, z) {};",
			expected: vec![String::from("x"), String::from("y"), String::from("z")],
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		assert_eq!(actual.statements.len(), 1);
		if let Some(Statement::EXPRESSION(Expression::FUNCTION(literal))) = actual.statements.first() {
			assert_eq!(literal.parameters, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_expression_call() {
	let input = "add(1, 2 * 3, 4 + 5);";

	let (actual, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

	let expecteds = vec![
		Statement::EXPRESSION(
			Expression::CALL(
				CallExpression {
					function: Box::new(
						Expression::IDENT(String::from("add"))
					),
					arguments: vec![
						Expression::LITERAL(
							Literal::INT(1)
						),
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::LITERAL(Literal::INT(2))),
								operator: InfixType::MULTIPLICATION,
								right: Box::new(Expression::LITERAL(Literal::INT(3))),
							}
						),
						Expression::INFIX(
							InfixExpression {
								left: Box::new(Expression::LITERAL(Literal::INT(4))),
								operator: InfixType::PLUS,
								right: Box::new(Expression::LITERAL(Literal::INT(5))),
							}
						)
					],
				}
			)
		),
	];

	assert_eq!(actual.statements, expecteds);
}

#[test]
fn test_parse_statement_expression_array() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "[1, 2 * 2, 3 + 3]",
			expected: Statement::EXPRESSION(Expression::ARRAY(ArrayLiteral{
				elements: vec![
					Expression::LITERAL(Literal::INT(1)),
					Expression::INFIX(InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(2))),
						operator: InfixType::MULTIPLICATION,
						right: Box::new(Expression::LITERAL(Literal::INT(2))),
					}),
					Expression::INFIX(InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(3))),
						operator: InfixType::PLUS,
						right: Box::new(Expression::LITERAL(Literal::INT(3))),
					}),
				]
			})),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}

#[test]
fn test_parse_statement_expression_map() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "{}",
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: vec![]})),
		},
		Test {
			input: r#"{"one": 1, "two": 2, "ten": 10}"#,
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: vec![
				(Expression::LITERAL(Literal::STRING(String::from("one"))), Expression::LITERAL(Literal::INT(1))),
				(Expression::LITERAL(Literal::STRING(String::from("two"))), Expression::LITERAL(Literal::INT(2))),
				(Expression::LITERAL(Literal::STRING(String::from("ten"))), Expression::LITERAL(Literal::INT(10)))
			]})),
		},
		Test {
			input: r#"{"one": 0 + 1, "two": 10 - 8, "ten": 50 / 5}"#,
			expected: Statement::EXPRESSION(Expression::HASH(HashLiteral{pairs: vec![
				(Expression::LITERAL(Literal::STRING(String::from("one"))), Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(0))),
						operator: InfixType::PLUS,
						right: Box::new(Expression::LITERAL(Literal::INT(1))),
					}
				)),
				(Expression::LITERAL(Literal::STRING(String::from("two"))), Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(10))),
						operator: InfixType::MINUS,
						right: Box::new(Expression::LITERAL(Literal::INT(8))),
					}
				)),
				(Expression::LITERAL(Literal::STRING(String::from("ten"))), Expression::INFIX(
					InfixExpression{
						left: Box::new(Expression::LITERAL(Literal::INT(50))),
						operator: InfixType::DIVISION,
						right: Box::new(Expression::LITERAL(Literal::INT(5))),
					}
				)),
			]})),
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
}


#[test]
fn test_parse_statement_expression_index() {
	struct Test<'a> {
		input: &'a str,
		expected: Statement,
	}

	let tests = vec![
		Test {
			input: "myArray[5 + 7]",
			expected: Statement::EXPRESSION(Expression::INDEX(IndexExpression{
				left: Box::new(Expression::IDENT(String::from("myArray"))),
				index: Box::new(Expression::INFIX(InfixExpression{
					left: Box::new(Expression::LITERAL(Literal::INT(5))),
					operator: InfixType::PLUS,
					right: Box::new(Expression::LITERAL(Literal::INT(7)))
				}))
			}))
		},
	];

	for test in tests {
		let (actual, errs) = Parser::new(Lexer::new(test.input.to_owned())).parse();
		if let Some(stmt) = actual.statements.first() {
			assert_eq!(*stmt, test.expected);
		} else {
			assert!(false);
		};
	}
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
	fn lookup_ident(ident: String) -> Token {
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