use crate::ast::ast::*;

use crate::types::object::*;
use crate::types::builtins::*;
use crate::types::hashable::*;
use crate::types::array::*;
use crate::types::env::*;

use std::{fmt};
use std::fmt::{Formatter};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Evaluator {
	pub environment: Rc<RefCell<Environment>>
}

impl Evaluator {
	pub fn new() -> Self {
		Evaluator {
			environment: Rc::new(RefCell::new(Environment::default()))
		}
	}

	pub fn eval(&mut self, node: Node) -> Result<Object, EvalError> {
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
