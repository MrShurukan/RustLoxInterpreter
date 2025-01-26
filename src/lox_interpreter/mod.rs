mod token_type;
mod token;
mod scanner;

use std::{fs, io};
use std::io::Write;
use std::process::exit;
use anyhow::Context;
use crate::lox_interpreter::scanner::{Scanner, ScannerError};

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

            self.run(&trim).context("Couldn't run the prompt")?;
        }
    }

    fn run(&mut self, source: &str) -> anyhow::Result<()> {
        let mut scanner = Scanner::new(source);
        
        let tokens = scanner.scan_tokens()?;
        
        for token in tokens {
            println!("{token:?}")
        }
        
        Ok(())
    }
}