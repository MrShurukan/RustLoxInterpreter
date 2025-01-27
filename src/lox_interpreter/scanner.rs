use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};
use anyhow::bail;
use std::error::Error;
use std::fmt::{Display, Formatter};
use unicode_segmentation::UnicodeSegmentation;

pub struct Scanner<'a> {
    /// This is not meant to be modified. This field should stay read-only, as there is a cached
    /// version of how many graphemes are inside there (see `grapheme_count`).
    source: &'a str,
    /// This is a cached field with the value of `self.source.graphemes(true).count()`
    /// If this is changed, code will no longer know the correct grapheme iterator length.
    grapheme_count: usize,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source, tokens: vec![], start: 0, current: 0, line: 1, 
            grapheme_count: source.graphemes(true).count() }
    }

    pub fn scan_tokens(&mut self) -> Result<&[Token], ScannerError> {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.current;

            if let Err(err) = self.scan_token() {
                if let ScannerErrorType::NoMoreTokens = err.error_type {
                    break;
                } else {
                    return Err(err);
                }
            }
        }

        self.tokens.push(
            Token { token_type: TokenType::EOF, lexeme: String::new(), line: self.line }
        );

        Ok(self.tokens.as_slice())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.grapheme_count
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c = match self.advance() {
            Some(token) => token,
            None => return Err(ScannerError::new(ScannerErrorType::NoMoreTokens, self.line)),
        };

        use TokenType::Punctuation as P;
        use PunctuationType as PT;

        match c {
            // Single char punctuation
            "(" => self.add_token_type(P(PT::LeftParen)),
            ")" => self.add_token_type(P(PT::RightParen)),
            "{" => self.add_token_type(P(PT::LeftBrace)),
            "}" => self.add_token_type(P(PT::RightBrace)),
            "," => self.add_token_type(P(PT::Comma)),
            "." => self.add_token_type(P(PT::Dot)),
            "-" => self.add_token_type(P(PT::Minus)),
            "+" => self.add_token_type(P(PT::Plus)),
            ";" => self.add_token_type(P(PT::Semicolon)),
            "*" => self.add_token_type(P(PT::Star)),

            // Check for multichar punctuation (by checking the next char)
            "!" => {
                let token_type = self.next_char_check("=", P(PT::BangEqual), P(PT::Bang));
                self.add_token_type(token_type);
            },
            "=" => {
                let token_type = self.next_char_check("=", P(PT::EqualEqual), P(PT::Equal));
                self.add_token_type(token_type);
            },
            "<" => {
                let token_type = self.next_char_check("=", P(PT::LessEqual), P(PT::Less));
                self.add_token_type(token_type);
            },
            ">" => {
                let token_type = self.next_char_check("=", P(PT::GreaterEqual), P(PT::Greater));
                self.add_token_type(token_type);
            },

            // A special case: '/', as it's division and comments
            "/" => {
                // Comment
                if self.next_matches("/") {
                    // Read until the end of the line
                    while let Some(current_char) = self.peek_current() {
                        if Self::is_newline(current_char) { break; }
                        self.advance();
                    }
                }
                else {
                    self.add_token_type(P(PT::Slash));
                }
            },

            // Stuff to skip
            " " => { },
            "\r" => { },
            "\t" => { },
            
            "\n" | "\r\n" => self.line += 1,

            // Literals
            "\"" => self.parse_string_literal()?,

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
                "\n" | "\r\n" => self.line += 1,
                "\"" => break,
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
        let str = self.source.graphemes(true)
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

    fn next_char_check(&mut self, next_char: &str,
                           if_true: TokenType,
                           if_false: TokenType) -> TokenType {

        if self.next_matches(next_char) { if_true }
        else { if_false }
    }

    fn next_matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() { return false; }

        let next_char = self.peek_current();
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
        let lexeme = self.source.graphemes(true).skip(self.start).take(take_count).collect();
        
        self.tokens.push(Token { token_type, lexeme, line: self.line });
    }
    
    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn advance(&mut self) -> Option<&str> {        
        let char_at_current = Self::peek_current_no_borrow(self.source, self.current);
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
    /// It is computed by iterating over `self.source.graphemes(true)`
    fn get_line_with_marker(source: &str, marker_position: usize) -> anyhow::Result<String> {
        let mut current_position: usize = 0;
        let mut line_start_position: usize = 0;
        let mut line_number: u32 = 1;
        let mut chars = source.graphemes(true);

        // Find the line start
        while current_position != marker_position {
            let current_char = chars.next();
            match current_char {
                None => bail!("Reached the end of the string before finding marker_position"),
                Some(current_char) => {
                    if Self::is_newline(current_char) {
                        line_start_position = current_position + 1;
                        line_number += 1;
                    }
                }
            }

            current_position += 1;
        }

        let line: String = source.graphemes(true)
            .skip(line_start_position)
            .take_while(|c| *c != "\n" && *c != "\r\n")
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
    fn peek_current(&self) -> Option<&str> {
        Self::peek_current_no_borrow(self.source, self.current)
    }
    
    fn peek_current_no_borrow(source: &str, current_index: usize) -> Option<&str> {
        source.graphemes(true).nth(current_index)
    }
    
    fn is_newline(string: &str) -> bool {
        match string {
            "\r\n" | "\n" => true, 
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct ScannerError {
    error_type: ScannerErrorType,
    line: u32,
    location: Option<String>
}

impl ScannerError {
    pub fn new(error_type: ScannerErrorType, line: u32) -> Self {
        ScannerError { error_type, line, location: None }
    }

    pub fn new_with_location(error_type: ScannerErrorType, line: u32, marker_position: usize, source: &str) -> Self {
        let location = Scanner::get_line_with_marker(source, marker_position)
            .expect("Couldn't produce line with a marker");
        ScannerError { error_type, line, location: Some(location) }
    }
}

#[derive(Debug)]
pub enum ScannerErrorType {
    UnknownToken(String),
    UnterminatedString,
    NoMoreTokens
}

impl Error for ScannerError {}
impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Error", self.line);

        let output = match &self.error_type {
            ScannerErrorType::UnknownToken(lexeme) => format!("Unknown token ('{}')", lexeme),
            ScannerErrorType::UnterminatedString => "Unterminated string".parse().unwrap(),
            ScannerErrorType::NoMoreTokens => "There were no more tokens to parse".parse().unwrap(),

            _ => format!("{:?}", self)
        };

        write!(f, "{error_header}: {}", output)?;
        if let Some(location) = &self.location {
            write!(f, "\n\nHappened here:\n{}", location)?;
        }
        
        Ok(())
    }
}