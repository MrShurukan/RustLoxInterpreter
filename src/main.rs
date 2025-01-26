mod lox_interpreter;

use crate::lox_interpreter::LoxInterpreter;
use std::io::Write;
use std::process::exit;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
        println!("Alternatively, you can run this without arguments to enter prompt mode");
        exit(64);
    }

    let mut interpreter = LoxInterpreter::new();

    if args.len() == 2 {
        let file_path = args.get(1).unwrap();
        interpreter.run_file(file_path).unwrap_or_else(|error| {
            println!("Unexpected error while running the file '{file_path}'");
            println!("{error}");
        });
    }
    else {
        interpreter.run_prompt().unwrap_or_else(|error| {
            println!("{error}");
        });
    }
}