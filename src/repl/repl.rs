use crate::ast::ast::*;
use crate::eval::eval::*;
use crate::parser::parser::*;
use crate::lexer::lexer::*;
use crate::types::object::*;

use std::io;
use std::process;
use std::io::prelude::*;
use crate::compiler::compiler::Compiler;
use crate::vm::vm::VM;

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

				//VM
				let mut comp = Compiler::new();
				match comp.compile(Node::PROGRAM(program)) {
					Ok(b) => {
						let mut machine = VM::new(&b);
						machine.run();
						println!("{}", machine.last_popped_stack_elem().unwrap());
					}
					Err(e) => {
						println!("Compilation failed: {}", e);
						continue;
					}
				}

				//EVALUATOR
				// match evaluator.eval(Node::PROGRAM(program)) {
				// 	Ok(o) => {
				// 		match o {
				// 			Object::NULL => {}
				// 			_ => println!("{}", o)
				// 		}
				// 	}
				// 	Err(e) => {
				// 		println!("EvalError: {}", e);
				// 		continue;
				// 	}
				// }
			}
			Err(e) => {
				println!("StdinErr: {}", e);
				break;
			}
		};
	}
}