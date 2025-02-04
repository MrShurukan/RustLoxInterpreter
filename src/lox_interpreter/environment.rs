use crate::lox_interpreter::value::Value;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
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

    pub fn get(environments: &Vec<Environment>, name: &String) -> Option<Value> {
        for environment in environments.iter().rev() {
            match environment.values.get(name) {
                // Try to get the value from the current environment
                Some(value) => return Some(value.to_owned()),
                // Otherwise continue and try our luck in the next environment in the stack
                None => continue
            }
        }
        
        None
    }

    pub fn assign(environments: &mut Vec<Environment>, name: &String, value: &Value) -> Result<(), EnvironmentError> {        
        for environment in environments.iter_mut().rev() {
            // Try to set the value in the current environment
            if environment.values.contains_key(name) {
                environment.values.insert(name.to_owned(), value.to_owned());
                return Ok(());
            }
            else {
                // If not found, try moving up by one environment
                continue;
            }
        }

        Err(EnvironmentError::new(EnvironmentErrorType::UndefinedVariable(name.to_owned())))
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