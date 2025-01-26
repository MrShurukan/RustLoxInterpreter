use std::error::Error;
use std::fmt::{format, Display, Formatter};
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{PunctuationType, TokenType};

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source, tokens: vec![], start: 0, current: 0, line: 1 }
    }

    pub fn scan_tokens(&mut self) -> Result<&[Token], ScannerError> {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(
            Token { token_type: TokenType::EOF, lexeme: String::new(), line: self.line }
        );

        Ok(self.tokens.as_slice())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c: char = self.advance()
            .expect("For some reason, I expect it to always return valid chars?");

        use TokenType::Punctuation as P;
        use PunctuationType as PT;

        match c {
            // Single char punctuation
            '(' => self.add_token(P(PT::LeftParen)),
            ')' => self.add_token(P(PT::RightParen)),
            '{' => self.add_token(P(PT::LeftBrace)),
            '}' => self.add_token(P(PT::RightBrace)),
            ',' => self.add_token(P(PT::Comma)),
            '.' => self.add_token(P(PT::Dot)),
            '-' => self.add_token(P(PT::Minus)),
            '+' => self.add_token(P(PT::Plus)),
            ';' => self.add_token(P(PT::Semicolon)),
            '*' => self.add_token(P(PT::Star)),

            // Check for multichar punctuation (by checking the next char)
            '!' => {
                let token_type = self.next_char_check('=', P(PT::BangEqual), P(PT::Bang));
                self.add_token(token_type);
            },
            '=' => {
                let token_type = self.next_char_check('=', P(PT::EqualEqual), P(PT::Equal));
                self.add_token(token_type);
            },
            '<' => {
                let token_type = self.next_char_check('=', P(PT::LessEqual), P(PT::Less));
                self.add_token(token_type);
            },
            '>' => {
                let token_type = self.next_char_check('=', P(PT::GreaterEqual), P(PT::Greater));
                self.add_token(token_type);
            },

            // A special case: '/', as it's division and comments
            '/' => {
                // Comment
                if self.next_matches('/') {
                    // Read until the end of the line
                    loop {
                        match self.peek_current() {
                            Some(current_char) => {
                                if current_char == '\n' { break; }
                            },
                            None => break
                        }

                        self.advance();
                    }
                }
                else {
                    self.add_token(P(PT::Slash));
                }
            },

            // Stuff to skip
            ' ' => { },
            '\r' => { },
            '\t' => { },

            '\n' => { self.line += 1; },


            _ => return Err(
                ScannerError::new(
                    ScannerErrorType::UnknownToken(String::from(c)),
                    self.line,
                )
            )
        };

        Ok(())
    }

    fn next_char_check(&mut self, next_char: char,
                           if_true: TokenType,
                           if_false: TokenType) -> TokenType {

        if self.next_matches(next_char) { if_true }
        else { if_false }
    }

    fn next_matches(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }

        let next_char = self.source.chars().nth(self.current);
        match next_char {
            None => { return false; },
            Some(next_char) => {
                if next_char != expected { return false; }
            }
        }

        self.current += 1;
        true
    }

    fn add_token(&mut self, token_type: TokenType) {
        let take_count = self.current - self.start;
        let lexeme = self.source.chars().skip(self.start).take(take_count).collect();
        self.tokens.push(Token { token_type, lexeme, line: self.line });
    }

    fn advance(&mut self) -> Option<char> {
        let char_at_current = self.peek_current();
        self.current += 1;

        char_at_current
    }

    fn peek_current(&self) -> Option<char> {
        if self.is_at_end() {
            None
        }
        else {
            self.source.chars().nth(self.current)
        }
    }
}

#[derive(Debug)]
pub struct ScannerError {
    error_type: ScannerErrorType,
    line: u32,
    location: String
}

impl ScannerError {
    pub fn new(error_type: ScannerErrorType, line: u32) -> Self {
        ScannerError { error_type, line, location: String::from("[not specified]") }
    }

    pub fn new_with_location(error_type: ScannerErrorType, line: u32, location: String) -> Self {
        ScannerError { error_type, line, location }
    }
}

#[derive(Debug)]
pub enum ScannerErrorType {
    UnknownToken(String)
}

impl Error for ScannerError {}
impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Error @{}", self.line, self.location);

        let output = match &self.error_type {
            ScannerErrorType::UnknownToken(lexeme) => format!("Unknown token ('{}')", lexeme),
            _ => format!("{:?}", self)
        };

        write!(f, "{error_header}: {}", output)
    }
}