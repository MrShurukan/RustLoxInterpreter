use crate::lox_interpreter::expression::{EvaluationError, Expression};

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression)
}

impl Statement {
    pub fn evaluate(&self) -> Result<(), EvaluationError> {
        match self {
            Statement::Expression(expr) => { _ = expr.evaluate()? }
            Statement::Print(expr) => { 
                let value = expr.evaluate()?;
                println!("{}", value);
            }
        };
        
        Ok(())
    }
}