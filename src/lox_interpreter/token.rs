use std::fmt;
use crate::lox_interpreter::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    /// Offset in grapheme count from the start of the line
    pub line_offset: usize,
    /// Size of the token lexeme
    pub lexeme_size: usize
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token {{ type: {:?}, line: {} }}",
               self.token_type, self.line)
    }
}