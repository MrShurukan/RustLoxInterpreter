use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
    NotInitialized
}

impl Value {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Number(_) => "number",
            Value::Boolean(_) => "bool",
            Value::Nil => "nil",
            Value::NotInitialized => "[not init]",
        }
    }
    
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(bool) => *bool,
            Value::Nil => false,
            _ => true,
        }
    }
    
    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false
        }
    }

    pub fn is_not_init(&self) -> bool {
        match self {
            Value::NotInitialized => true,
            _ => false
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(str) => { write!(f, "{str}") }
            Value::Number(num) => { write!(f, "{num}") }
            Value::Boolean(bool) => { write!(f, "{bool}") }
            Value::Nil => { write!(f, "nil") },
            Value::NotInitialized => { write!(f, "[not init]") }
        }
    }
}