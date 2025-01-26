use std::error::Error;
use std::fmt::{format, Display, Formatter};
use anyhow::bail;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};

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
            '(' => self.add_token_type(P(PT::LeftParen)),
            ')' => self.add_token_type(P(PT::RightParen)),
            '{' => self.add_token_type(P(PT::LeftBrace)),
            '}' => self.add_token_type(P(PT::RightBrace)),
            ',' => self.add_token_type(P(PT::Comma)),
            '.' => self.add_token_type(P(PT::Dot)),
            '-' => self.add_token_type(P(PT::Minus)),
            '+' => self.add_token_type(P(PT::Plus)),
            ';' => self.add_token_type(P(PT::Semicolon)),
            '*' => self.add_token_type(P(PT::Star)),

            // Check for multichar punctuation (by checking the next char)
            '!' => {
                let token_type = self.next_char_check('=', P(PT::BangEqual), P(PT::Bang));
                self.add_token_type(token_type);
            },
            '=' => {
                let token_type = self.next_char_check('=', P(PT::EqualEqual), P(PT::Equal));
                self.add_token_type(token_type);
            },
            '<' => {
                let token_type = self.next_char_check('=', P(PT::LessEqual), P(PT::Less));
                self.add_token_type(token_type);
            },
            '>' => {
                let token_type = self.next_char_check('=', P(PT::GreaterEqual), P(PT::Greater));
                self.add_token_type(token_type);
            },

            // A special case: '/', as it's division and comments
            '/' => {
                // Comment
                if self.next_matches('/') {
                    // Read until the end of the line
                    while let Some(current_char) = self.peek_current() {
                        if current_char == '\n' { break; }
                        self.advance();
                    }
                }
                else {
                    self.add_token_type(P(PT::Slash));
                }
            },

            // Stuff to skip
            ' ' => { },
            '\r' => { },
            '\t' => { },

            '\n' => self.line += 1,

            // Literals
            '"' => self.parse_string_literal()?,

            _ => return Err(
                ScannerError::new_with_location(
                    ScannerErrorType::UnknownToken(String::from(c)),
                    self.line,
                    self.current,
                    self.source
                )
            )
        };

        Ok(())
    }

    fn parse_string_literal(&mut self) -> Result<(), ScannerError> {
        let start_string_index = self.current;
        while let Some(current_char) = self.peek_current() {
            match current_char {
                '\n' => self.line += 1,
                '"' => break,
                _ => {}
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(
                ScannerError::new_with_location(
                    ScannerErrorType::UnterminatedString,
                    self.line,
                    start_string_index,
                    self.source
                )
            );
        }

        // Skip the closing quote
        self.advance();

        // Get the string value
        let str = self.source.chars()
            .skip(self.start + 1)
            .take(self.current - self.start - 2)
            .collect();
        
        let token = Token {
            lexeme: format!("\"{str}\""),
            token_type: TokenType::Literal(LiteralType::String(str)),
            line: self.line
        };
        
        self.add_token(token);
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

    fn add_token_type(&mut self, token_type: TokenType) {
        let take_count = self.current - self.start;
        let lexeme = self.source.chars().skip(self.start).take(take_count).collect();
        self.tokens.push(Token { token_type, lexeme, line: self.line });
    }
    
    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn advance(&mut self) -> Option<char> {
        let char_at_current = self.peek_current();
        self.current += 1;

        char_at_current
    }

    /// Gets a helper string for error displaying with a marker on the second line.
    /// ### Example
    /// This function is used to create a string that looks something like this:
    /// ```
    /// 14. let test = (hello + 1
    ///                ^
    /// ```
    /// Resulting string consists of two lines and has a marker denoted by
    /// `^` on the second line. The line is automatically inferred by the `marker_position` from
    /// the `self.source` string. See "Arguments" below.
    ///
    /// ### Arguments
    /// * `marker_position` - Absolute index of a char into the `self.source` string.
    /// It is computed by iterating over `self.source.chars()`
    fn get_line_with_marker(source: &str, marker_position: usize) -> anyhow::Result<String> {
        let mut current_position: usize = 0;
        let mut line_start_position: usize = 0;
        let mut line_number: u32 = 1;
        let mut chars = source.chars();

        // Find the line start
        while current_position != marker_position {
            let current_char = chars.next();
            match current_char {
                None => bail!("Reached the end of the string before finding marker_position"),
                Some(current_char) => {
                    if current_char == '\n' {
                        line_start_position = current_position + 1;
                        line_number += 1;
                    }
                }
            }

            current_position += 1;
        }

        let line: String = source.chars()
            .skip(line_start_position)
            .take_while(|c| *c != '\n')
            .collect();

        let marker_offset = marker_position - line_start_position;
        let line_number_string = format!("{line_number}. ");
        let spaces = " ".repeat(line_number_string.len() + marker_offset - 1);

        Ok(format!("{line_number_string}{line}\n{spaces}^"))
    }

    /// Peeks current character.
    ///
    /// Returns `None` if we are already at the end of the string/asked it
    /// to return outside bounds
    fn peek_current(&self) -> Option<char> {
        self.source.chars().nth(self.current)
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

    pub fn new_with_location(error_type: ScannerErrorType, line: u32, marker_position: usize, source: &str) -> Self {
        let location = Scanner::get_line_with_marker(source, marker_position)
            .expect("Couldn't produce line with a marker");
        ScannerError { error_type, line, location }
    }
}

#[derive(Debug)]
pub enum ScannerErrorType {
    UnknownToken(String),
    UnterminatedString
}

impl Error for ScannerError {}
impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Error", self.line);

        let output = match &self.error_type {
            ScannerErrorType::UnknownToken(lexeme) => format!("Unknown token ('{}')", lexeme),
            ScannerErrorType::UnterminatedString => "Unterminated string".parse().unwrap(),

            _ => format!("{:?}", self)
        };

        write!(f, "{error_header}: {}\n\nHappened here:\n{}", output, self.location)
    }
}