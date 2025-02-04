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
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        Self { values: HashMap::new(), enclosing: None }
    }

    pub fn new_with_enclosing(environment: Rc<RefCell<Environment>>) -> Self {
        Self { values: HashMap::new(), enclosing: Some(environment) }
    }
    
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(mut environment: Rc<RefCell<Environment>>, name: &String) -> Option<Value> {
        let mut new_enclosing: Rc<RefCell<Environment>>;
        loop {
            match environment.borrow().values.get(name) {
                // Try to get the value from the current environment
                Some(value) => {
                    return Some(value.to_owned())
                }
                None => {
                    match &environment.borrow().enclosing {
                        // If not found, try moving up by one environment
                        Some(enclosing) => {
                            // See below match statement
                            new_enclosing = Rc::clone(enclosing);
                        },
                        // If there is no enclosing environment, we didn't find the variable
                        _ => {
                            return None;
                        }
                    }
                }
            }

            environment = new_enclosing;
        }
    }

    pub fn assign(mut environment: Rc<RefCell<Environment>>, name: &String, value: &Value) -> Result<(), EnvironmentError> {
        let mut new_enclosing: Rc<RefCell<Environment>>;
        loop {
            // Try to set the value in the current environment
            if environment.borrow().values.contains_key(name) {
                environment.borrow_mut().values.insert(name.to_owned(), value.to_owned());
                return Ok(())
            } else {
                match &environment.borrow().enclosing {
                    // If not found, try moving up by one environment
                    Some(enclosing) => {
                        // See below if statement
                        new_enclosing = Rc::clone(&enclosing);
                    },
                    // If there is no enclosing environment, we didn't find the variable
                    _ => {
                        return Err(EnvironmentError::new(EnvironmentErrorType::UndefinedVariable(name.to_owned())));
                    }
                }
            }

            environment = new_enclosing;
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