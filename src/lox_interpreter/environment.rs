use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::lox_interpreter::expression::EvaluationError;
use crate::lox_interpreter::value::Value;

pub struct Environment {
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        Self { values: HashMap::new() }
    }
    
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &String) -> Option<Value> {
        match self.values.get(name) {
            Some(value) => {
                Some(value.to_owned())
            }
            None => { None }
        }
    }
    
    pub fn assign(&mut self, name: &String, value: &Value) -> Result<(), EnvironmentError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_owned(), value.to_owned());
            Ok(())
        }
        else {
            Err(EnvironmentError::new(EnvironmentErrorType::UndefinedVariable(name.to_owned())))
        }
    }
}

#[derive(Debug)]
pub struct EnvironmentError {
    pub environment_error_type: EnvironmentErrorType
}

#[derive(Debug)]
pub enum EnvironmentErrorType {
    UndefinedVariable(String)
}

impl Error for EnvironmentError {}
impl Display for EnvironmentError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.environment_error_type {
            EnvironmentErrorType::UndefinedVariable(name) => { write!(f, "Undefined variable '{}'", name) }
        }
    }
}

impl EnvironmentError {
    pub fn new(environment_error_type: EnvironmentErrorType) -> Self {
        Self { environment_error_type }
    }
}