use crate::lox_interpreter::environment::Environment;
use crate::lox_interpreter::expression::{EvaluationError, Expression};
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, TokenType};
use crate::lox_interpreter::value::Value;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    VariableDeclaration { identifier: Token, initializer: Option<Expression> },
    Block(Rc<[Statement]>)
}

impl Statement {
    pub fn execute(&self, environments: &mut Vec<Environment>) -> Result<(), EvaluationError> {
        match self {
            Statement::Expression(expr) => { _ = expr.evaluate(environments)? },
            Statement::Print(expr) => { 
                let value = expr.evaluate(environments)?;
                println!("{}", value);
            },
            Statement::VariableDeclaration { identifier, initializer } => {
                let mut value = Value::Nil;
                if let Some(expr) = initializer {
                    value = expr.evaluate(environments)?;
                }

                if let TokenType::Literal(LiteralType::Identifier(name)) = &identifier.token_type {
                    // Grab the most recent environment out of the stack to define the variable in
                    // If it doesn't exist in the stack - something went very wrong somewhere else
                    environments
                        .iter_mut()
                        .nth_back(0)
                        .expect("There was no environments to execute the statement in")
                        .define(name.to_owned(), value);
                }
                else {
                    unreachable!("Variable declaration had token of type != Identifier");
                }
            },
            Statement::Block(statements) => {
                // 1) Create a new environment for the block
                environments.push(Environment::new());

                // 2) Execute statements in that new environment stack
                for statement in statements.iter() {
                    statement.execute(environments)?;
                }
                
                // 3) Pop the created environment from the stack
                environments.pop();
            }
        };
        
        Ok(())
    }
}