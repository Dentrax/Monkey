use std::env;
use std::time::Instant;
use monkey::lexer::lexer::Lexer;
use monkey::parser::parser::Parser;
use monkey::compiler::compiler::Compiler;
use monkey::ast::ast::Node;
use monkey::vm::vm::VM;
use monkey::eval::eval::Evaluator;
use monkey::types::object::Object;
use std::process::exit;


const INPUT: &str = r#"
let fibonacci = fn(x) {
	if (x == 0) {
		0
	} else {
		if (x == 1) {
			1;
		} else {
			fibonacci(x - 1) + fibonacci(x - 2);
		}
	}
};
fibonacci(35);
"#;

enum Engine {
	VM,
	EVAL
}

fn main() {
	let engine = match env::args().skip(1).next() {
		Some(x) => {
			match &x[..] {
				"--vm" => Some(Engine::VM),
				"--eval" => Some(Engine::EVAL),
				_ => panic!("use 'vm' or 'eval'")
			}
		},
		None => exit(1)
	};

	let l = Lexer::new(INPUT.parse().unwrap());
	let mut p = Parser::new(l);
	let (program, _) = p.parse();

	match engine {
		Some(e) => {
			match e {
				Engine::VM => {
					let mut compiler = Compiler::new();
					let bytecode = compiler.compile(Node::PROGRAM(program)).unwrap();
					let mut globals = VM::new_globals();
					let mut machine = VM::new_with_global_store(&bytecode, globals);

					let now = Instant::now();

					{
						machine.run();
					}

					let elapsed = now.elapsed();

					let result = machine.last_popped_stack_elem().unwrap();

					println!("engine: {},\nresult: {}\ndurations: secs: {}, nanos: {}\n", "VM", result, elapsed.as_secs(), elapsed.subsec_nanos());
				},
				Engine::EVAL => {
					let start = Instant::now();

					let mut evaluator = Evaluator::new();

					let now = Instant::now();
					let result;

					{
						result = evaluator.eval(Node::PROGRAM(program));
					}

					let elapsed = now.elapsed();

					println!("engine: {},\nresult: {:?}\ndurations: secs: {}, nanos: {}\n", "EVAL", result, elapsed.as_secs(), elapsed.subsec_nanos());
				}
			}
		}
		_ => {}
	}
}