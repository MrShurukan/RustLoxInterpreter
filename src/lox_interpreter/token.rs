use std::fmt;
use crate::lox_interpreter::token_type::TokenType;

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token {{ type: {:?}, lexeme: {}, line: {} }}",
               self.token_type, self.lexeme, self.line)
    }
}