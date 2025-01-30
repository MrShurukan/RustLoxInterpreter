mod token_type;
mod token;
mod scanner;
mod parser;
mod expression;

use crate::lox_interpreter::scanner::Scanner;
use anyhow::{bail, Context};
use std::io::Write;
use std::process::exit;
use std::{fs, io};

pub struct LoxInterpreter {
}

impl LoxInterpreter {
    pub fn new() -> Self {
        LoxInterpreter { }
    }

    pub fn run_file(&mut self, file_name: &str) -> anyhow::Result<()> {
        let contents = fs::read_to_string(file_name)
            .context("Can't read the specified file")?;

        let result = self.run(&contents);
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
            match self.run(&trim) {
                Ok(_) | Err(_) => { },
            };
        }
    }

    fn run(&mut self, source: &str) -> anyhow::Result<()> {
        let mut scanner = Scanner::new(source);
        
        let tokens = scanner.scan_tokens();
        if let Err(errors) = tokens {
            println!("Scanning failed.\n");
            for err in errors {
                println!("{err}\n");
            }
            
            bail!("Couldn't advance to interpreting");
        }
        
        for token in tokens.unwrap() {
            println!("{token:?}")
        }
        
        Ok(())
    }
}