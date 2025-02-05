use crate::lox_interpreter::value::Value;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct Environment {
    pub(crate) values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        Self { values: HashMap::new() }
    }
    
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}

#[derive(Debug)]
pub struct EnvironmentError {
    pub environment_error_type: EnvironmentErrorType
}

#[derive(Debug)]
pub enum EnvironmentErrorType {
    UndefinedVariable(String),
    LastEnvironmentRemoved
}

impl Error for EnvironmentError {}
impl Display for EnvironmentError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.environment_error_type {
            EnvironmentErrorType::UndefinedVariable(name) => { write!(f, "Undefined variable '{}'", name) },
            EnvironmentErrorType::LastEnvironmentRemoved => { write!(f, "Last environment was popped from the stack") }
        }
    }
}

impl EnvironmentError {
    pub fn new(environment_error_type: EnvironmentErrorType) -> Self {
        Self { environment_error_type }
    }
}