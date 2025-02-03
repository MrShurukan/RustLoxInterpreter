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
}