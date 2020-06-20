use crate::ast::ast::*;
use crate::eval::eval::*;
use crate::parser::parser::*;
use crate::lexer::lexer::*;
use crate::types::object::*;

use std::io;
use std::process;
use std::io::prelude::*;

const PROMPT: &str = ">>> ";

pub fn start() {
    let mut stdout = io::stdout();

    let mut input = String::new();
	let mut evaluator = Evaluator::new();

	loop {
        if write!(&mut stdout, "{}", PROMPT).is_err() { 
			process::exit(1) 
		};

		if stdout.flush().is_err() { 
			process::exit(1) 
		};

		input.clear();

		let _ = match io::stdin().read_line(&mut input) {
			Ok(_) => {
				let (program, errs) = Parser::new(Lexer::new(input.to_owned())).parse();

				if errs.len() > 0 {
					for err in errs {
						println!("SyntaxErr: {}", err);
					}
					continue;
				}

				match evaluator.eval(Node::PROGRAM(program)) {
					Ok(o) => {
						match o {
							Object::NULL => {}
							_ => println!("{}", o)
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
}