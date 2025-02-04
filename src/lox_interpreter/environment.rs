use std::collections::HashMap;
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
}