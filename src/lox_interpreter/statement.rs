use std::cell::RefCell;
use std::rc::Rc;
use crate::lox_interpreter::environment::Environment;
use crate::lox_interpreter::expression::{EvaluationError, Expression};
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, TokenType};
use crate::lox_interpreter::value::Value;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    VariableDeclaration { identifier: Token, initializer: Option<Expression> },
    Block(Rc<[Statement]>)
}

impl Statement {
    pub fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), EvaluationError> {
        match self {
            Statement::Expression(expr) => { _ = expr.evaluate(Rc::clone(&environment))? },
            Statement::Print(expr) => { 
                let value = expr.evaluate(Rc::clone(&environment))?;
                println!("{}", value);
            },
            Statement::VariableDeclaration { identifier, initializer } => {
                let mut value = Value::Nil;
                if let Some(expr) = initializer {
                    value = expr.evaluate(Rc::clone(&environment))?;
                }

                if let TokenType::Literal(LiteralType::Identifier(name)) = &identifier.token_type {
                    environment.borrow_mut().define(name.to_owned(), value);
                }
                else {
                    unreachable!("Variable declaration had token of type != Identifier");
                }
            },
            Statement::Block(statements) => {
                let new_environment = Rc::new(RefCell::new(Environment::new_with_enclosing(environment)));
                for statement in statements.iter() {
                    statement.execute(Rc::clone(&new_environment))?;
                }
            }
        };
        
        Ok(())
    }
}