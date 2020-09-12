use std::env;
use monkey::repl;

fn main() {
	//Python 3.8.3 (tags/v3.8.3:6f8c832, May 13 2020, 22:37:02) [MSC v.1924 64 bit (AMD64)] on win32
	println!("{name} Compiler v{version}",
        name = env!("CARGO_PKG_NAME"),
		version = env!("CARGO_PKG_VERSION"),
	);
	
	repl::repl::start();

	println!("Reached end of the application!");
}