use crate::lox_interpreter::environment::Environment;
use crate::lox_interpreter::expression::{EvaluationError, Expression};
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, TokenType};
use crate::lox_interpreter::value::Value;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    VariableDeclaration { identifier: Token, initializer: Option<Expression> }
}

impl Statement {
    pub fn evaluate(&self, environment: &mut Environment) -> Result<(), EvaluationError> {
        match self {
            Statement::Expression(expr) => { _ = expr.evaluate(environment)? },
            Statement::Print(expr) => { 
                let value = expr.evaluate(environment)?;
                println!("{}", value);
            },
            Statement::VariableDeclaration { identifier, initializer } => {
                let mut value = Value::Nil;
                if let Some(expr) = initializer {
                    value = expr.evaluate(environment)?;
                }

                if let TokenType::Literal(LiteralType::Identifier(name)) = &identifier.token_type {
                    environment.define(name.to_owned(), value);
                }
                else {
                    unreachable!("Variable declaration had token of type != Identifier");
                }
            }
        };
        
        Ok(())
    }
}