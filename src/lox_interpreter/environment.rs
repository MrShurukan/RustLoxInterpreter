use std::cell::RefCell;
use crate::lox_interpreter::value::Value;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

// TODO: Potentially think of environments as a list instead of a reference?
// This seems like it could be rewritten as a plain list as there is only a single path
// of enclosings at a time. Once you exit a block you pop and once you enter a block you push
// Def something to think about.
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

    pub fn get(environments: Rc<Vec<Environment>>, name: &String) -> Option<Value> {
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

    pub fn assign(mut environments: Rc<Vec<Environment>>, name: &String, value: &Value) -> Result<(), EnvironmentError> {
        let environments = Rc::get_mut(&mut environments)
            .expect("Can't get a mutable reference to a vec");
        
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