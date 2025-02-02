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
use crate::lox_interpreter::parser::Parser;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{PunctuationType, TokenType};

pub struct LoxInterpreter {
}

impl LoxInterpreter {
    pub fn new() -> Self {
        LoxInterpreter { }
    }

    pub fn run_file(&mut self, file_name: &str) -> anyhow::Result<()> {
        let contents = fs::read_to_string(file_name)
            .context("Can't read the specified file")?;

        let result = Self::run(&contents);
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
            match Self::run(&trim) {
                Ok(_) | Err(_) => { },
            };
        }
    }

    fn run(source: &str) -> anyhow::Result<()> {
        let scanner = Scanner::new(source.to_owned());
        
        let tokens = scanner.scan_tokens();
        if let Err(errors) = tokens {
            println!("Scanning failed.\n");
            for err in errors {
                println!("{err}\n");
            }
            
            bail!("Couldn't advance to parsing");
        }

        // let tokens_vec = tokens.unwrap();
        let tokens_vec = vec![Token { token_type: TokenType::Punctuation(PunctuationType::EqualEqual), line: 1 }];
        let parser = Parser::new(tokens_vec);
        let expression = parser.parse();

        if let Err(err) = expression {
            println!("Parsing failed.\n");
            println!("{err}\n");
        
            bail!("Couldn't advance to interpreting");
        }
        
        println!("{:?}", expression?);

        Ok(())
    }
}