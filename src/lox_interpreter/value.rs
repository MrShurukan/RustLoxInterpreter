use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil
}

impl Value {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Number(_) => "number",
            Value::Boolean(_) => "bool",
            Value::Nil => "nil"
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
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(str) => { write!(f, "{str}")? }
            Value::Number(num) => { write!(f, "{num}")? }
            Value::Boolean(bool) => { write!(f, "{bool}")? }
            Value::Nil => { write!(f, "nil")? }
        };
        
        Ok(())
    }
}