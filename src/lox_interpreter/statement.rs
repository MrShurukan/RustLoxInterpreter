use crate::lox_interpreter::environment_stack::EnvironmentStack;
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
    pub fn execute(&self, environments: &mut EnvironmentStack) -> Result<Value, EvaluationError> {
        let mut result = Value::Nil;
        match self {
            Statement::Expression(expr) => { result = expr.evaluate(environments)? },
            Statement::Print(expr) => { 
                let value = expr.evaluate(environments)?;
                println!("{}", value);
            },
            Statement::VariableDeclaration { identifier, initializer } => {
                let mut value = Value::NotInitialized;
                if let Some(expr) = initializer {
                    value = expr.evaluate(environments)?;
                }

                if let TokenType::Literal(LiteralType::Identifier(name)) = &identifier.token_type {
                    // Grab the most recent environment out of the stack to define the variable in
                    environments.define(name.to_owned(), value);
                }
                else {
                    unreachable!("Variable declaration had token of type != Identifier");
                }
            },
            Statement::Block(statements) => {
                // 1) Create a new environment for the block
                environments.push_new();

                // 2) Execute statements in that new environment stack
                for statement in statements.iter() {
                    statement.execute(environments)?;
                }
                
                // 3) Pop the created environment from the stack
                if let Err(e) = environments.remove_last() {
                    // If we messed up during removing (i.e. removed all environments entirely)
                    // then we messed up big time somewhere
                    panic!("{e:?}")
                }
            }
        };
        
        Ok(result)
    }
}