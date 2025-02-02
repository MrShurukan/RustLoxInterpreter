use std::fmt;
use crate::lox_interpreter::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    // /// Offset in grapheme count from the start of the line
    // pub line_offset: u32,
    // /// Size of the token lexeme
    // pub lexeme_size: u32
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token {{ type: {:?}, line: {} }}",
               self.token_type, self.line)
    }
}