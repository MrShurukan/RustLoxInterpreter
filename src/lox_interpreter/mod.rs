﻿mod token_type;
mod token;
mod scanner;
mod parser;
mod expression;
mod value;
mod interpreter;
mod statement;
mod environment;
mod environment_stack;

use crate::lox_interpreter::interpreter::Interpreter;
use crate::lox_interpreter::parser::Parser;
use crate::lox_interpreter::scanner::Scanner;
use anyhow::{bail, Context};
use std::io::Write;
use std::process::exit;
use std::{fs, io};
use crate::lox_interpreter::environment_stack::EnvironmentStack;
use crate::lox_interpreter::value::Value;

pub struct LoxInterpreter {
}

impl LoxInterpreter {
    pub fn new() -> Self {
        LoxInterpreter { }
    }

    pub fn run_file(&mut self, file_name: &str) -> anyhow::Result<()> {
        let contents = fs::read_to_string(file_name)
            .context("Can't read the specified file")?;

        let mut environment_stack = EnvironmentStack::new();
        let result = Self::run(&contents, &mut environment_stack);
        match result {
            Err(err) => {
                println!("{}", err);

                // Indicate an error in the exit code.
                exit(65);
            },
            _ => {}
        }

        Ok(())
    }
    pub fn run_prompt(&mut self) -> anyhow::Result<()> {
        println!("LOX REPL");
        let mut environment_stack = EnvironmentStack::new();

        loop {
            print!("> ");
            io::stdout().flush().context("io::stdout().flush failed")?;

            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .context("Failed to read a line from CLI")?;

            let trim = line.trim();
            if trim.len() == 0 {
                continue;
            }

            // Продолжаем работу интерпретатора независимо от результата этой строки
            match Self::run(&trim, &mut environment_stack) {
                Ok(value) if !value.is_nil() => {
                    println!("-> {value}");
                },
                Ok(_) | Err(_) => { },
            };
        }
    }

    fn run(source: &str, environment_stack: &mut EnvironmentStack) -> anyhow::Result<Value> {
        let scanner = Scanner::new(source.to_owned());

        let tokens = scanner.scan_tokens();
        if let Err(errors) = tokens {
            let errors_error = if errors.len() == 1 { "error" } else { "errors" };
            println!("Scanning failed. {} {errors_error} found\n", errors.len());
            for (i, err) in errors.iter().enumerate() {
                println!("Error #{}\n{err}\n", i + 1);
            }

            bail!("Couldn't advance to parsing");
        }

        let tokens_vec = tokens.unwrap();
        let mut parser = Parser::new(tokens_vec.into(), source);
        let statements = parser.parse();

        if let Err(errors) = statements {
            let errors_error = if errors.len() == 1 { "error" } else { "errors" };
            println!("Parsing failed. {} {errors_error} found\n", errors.len());
            for (i, err) in errors.iter().enumerate() {
                println!("Error #{}\n{err}\n", i + 1);
            }

            bail!("Couldn't advance to interpreting");
        }
        
        let mut interpreter: Interpreter;
        interpreter = Interpreter::new_with_environment(source, environment_stack);
        
        let statements = &statements.ok().unwrap();
        let value = interpreter.interpret(&statements);
        if let Err(error) = value {
            println!("{error}\n");

            bail!("Couldn't evaluate");
        }

        Ok(value?)
    }
}