use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType, KEYWORDS};
use anyhow::bail;
use std::error::Error;
use std::fmt::{Display, Formatter};
use unicode_segmentation::UnicodeSegmentation;

pub struct Scanner {
    /// This is not meant to be modified. This field should stay read-only, as there is a cached
    /// version of how many graphemes are inside there (see `grapheme_count`).
    source: String,
    /// This is a cached field with the value of `self.source.graphemes(true).count()`
    /// If this is changed, code will no longer know the correct grapheme iterator length.
    grapheme_count: usize,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: usize,
    current_from_line_start: usize
}

macro_rules! advance_current {
    ($self:ident) => {
        $self.current += 1;
        $self.current_from_line_start += 1;
    };
}

macro_rules! advance_line {
    ($self:ident) => {
        $self.line += 1;
        $self.current_from_line_start = 0;
    };
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner { source: source.to_owned(), tokens: vec![], start: 0, 
            current: 0, line: 1, current_from_line_start: 0, 
            grapheme_count: source.graphemes(true).count() }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Vec<ScannerError>> {
        let mut errors: Vec<ScannerError> = Vec::new();
        
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme.
            self.start = self.current;
            
            if let Err(err) = self.scan_token() {
                if let ScannerErrorType::NoMoreTokens = err.error_type {
                    break;
                } else {
                    errors.push(err);
                }
            }
        }

        self.tokens.push(
            Token { token_type: TokenType::EOF, line: self.line, 
                lexeme_size: 0, line_offset: 0 }
        );
        
        if errors.len() > 0 {
            Err(errors)
        }
        else {
            Ok(self.tokens)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.grapheme_count
    }

    fn scan_token(&mut self) -> Result<(), ScannerError> {
        let c = match self.advance() {
            Some(token) => token,
            None => return Err(self.produce_error(ScannerErrorType::NoMoreTokens)),
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
                // Multiline comment
                else if self.next_matches("*") {
                    self.multiline_comment()?;
                }
                else {
                    self.add_token_type(P(PT::Slash));
                }
            },

            // Stuff to skip
            " " => { },
            "\r" => { },
            "\t" => { },
            
            "\n" | "\r\n" => { advance_line!(self); },

            // Literals
            "\"" => self.parse_string_literal()?,

            // It's okay to use chars here, as numbers will always be expected to be regular chars,
            // not graphemes
            number if number.chars().all(|x| x.is_ascii_digit()) => {
                self.parse_number_literal()?;
            },

            // Identifiers and reserve words are a different story. Could be pretty complex.
            alpha if alpha.chars().all(|x| x.is_alphabetic() || x == '_') => {
                self.parse_alpha()?;
            },

            _ => {
                let error_type = ScannerErrorType::UnknownToken(String::from(c));
                return Err(self.produce_error_with_location(error_type, self.current))
            }
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
                self.produce_error_with_location(ScannerErrorType::UnterminatedString, start_string_index)
            );
        }

        // Skip the closing quote
        self.advance();

        // Get the string value
        let str = self.source.graphemes(true)
            .skip(self.start + 1)
            .take(self.current - self.start - 2)
            .collect();
        
        self.add_token_type(TokenType::Literal(LiteralType::String(str)));
        Ok(())
    }

    fn parse_number_literal(&mut self) -> Result<(), ScannerError> {
        // Advance until there are no more digits
        self.advance_while_digit();

        // Look for a fractional part.
        let current_char = self.peek_current();
        if let Some(current_char) = current_char {
            // Using peek_no_borrow here because it's an arbitrary peek ahead function
            let char_after_that = Self::peek_no_borrow(&self.source, self.current + 1)
                .ok_or_else(|| self.produce_error_with_location(
                    ScannerErrorType::IncorrectNumberToken("Trailing literals are not supported".parse().unwrap()),
                    self.current + 1)
                )?;

            if current_char == "." && char_after_that.chars().all(|c| c.is_ascii_digit()) {
                // Consume the "."
                self.advance();
                // Advance until there are no more digits
                self.advance_while_digit();
            }
        }
        
        // Get the number itself
        let str: String = self.get_current_lexeme();

        let value: f64 = str.parse().map_err(|_| self.produce_error_with_location(
            ScannerErrorType::IncorrectNumberToken("Couldn't convert to a double".parse().unwrap()),
            self.current))?;

        self.add_token_type(TokenType::Literal(LiteralType::Number(value)));
        Ok(())
    }

    fn advance_while_digit(&mut self) {
        while let Some(char) = Self::peek_no_borrow(&self.source, self.current) {
            if char.chars().any(|c| !c.is_ascii_digit()) {
                break;
            }

            self.advance();
        }
    }

    // Identifiers and keywords
    fn parse_alpha(&mut self) -> Result<(), ScannerError> {
        while let Some(char) = self.peek_current() {
            if !char.chars().all(|x| x.is_alphanumeric() || x == '_') {
                break;
            }

            self.advance();
        }

        let lexeme = self.get_current_lexeme();

        // Check if it's reserved
        let keyword_type = KEYWORDS.get(lexeme.as_str());
        if let Some(keyword_type) = keyword_type {
            self.add_token_type(TokenType::Keyword(keyword_type))
        }
        // Otherwise it's just a user configured lexeme
        else {
            self.add_token_type(TokenType::Literal(LiteralType::Identifier(lexeme)));
        }

        Ok(())
    }

    fn multiline_comment(&mut self) -> Result<(), ScannerError> {
        // Remember where the multiline comment started
        let multiline_comment_start = self.current;
        
        // Consume the "/" and the "*"
        self.advance(); self.advance();
        
        let mut nesting_level = 1;

        while let Some(current_char) = self.peek_current() {
            if Self::is_newline(current_char) {
                advance_line!(self);
            }
            // I support nested multiline comments. We need to account for that
            else if current_char == "/" {
                let next_char = Self::peek_no_borrow(&self.source, self.current + 1);
                if next_char.is_some() && next_char.unwrap() == "*" {
                    // Nesting located, consume the "/*" and increase the nesting_level
                    self.advance(); self.advance();
                    nesting_level += 1;
                }
            }
            // If we encounter a star, that could mean we are at the end of the comment
            else if current_char == "*" {
                let next_char = Self::peek_no_borrow(&self.source, self.current + 1);
                if next_char.is_some() && next_char.unwrap() == "/" {
                    // We found the end of a block! Consume the "*" and "/" (latter if we are done)
                    // Decrease the nesting_level
                    // Check if we are out of nesting by making sure nesting_level == 0, otherwise continue
                    self.advance();
                    
                    nesting_level -= 1;
                    if nesting_level == 0 {
                        self.advance();
                        return Ok(());
                    }
                }
            }
            self.advance();
        }
        
        // If we finished reading the source but never reached the end of a multiline - it's an error!
        if nesting_level != 0 {
            Err(self.produce_error_with_location(
                ScannerErrorType::UnterminatedMultilineComment,
                multiline_comment_start
            ))
        } else {
            unreachable!();
        }
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

        advance_current!(self);
        true
    }
    
    fn add_token_type(&mut self, token_type: TokenType) {
        let lexeme_size = self.current - self.start;
        self.tokens.push(Token { token_type, line: self.line, 
            line_offset: self.current_from_line_start - lexeme_size + 1, lexeme_size
        });
    }

    fn get_current_lexeme(&mut self) -> String {
        let take_count = self.current - self.start;
        self.source.graphemes(true).skip(self.start).take(take_count).collect()
    }

    fn advance(&mut self) -> Option<&str> {        
        let char_at_current = Self::peek_no_borrow(&self.source, self.current);
        advance_current!(self);

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
    fn get_line_with_marker(source: &str, marker_position: usize, line_number: &mut usize) -> anyhow::Result<String> {
        let mut current_position: usize = 0;
        let mut line_start_position: usize = 0;
        let mut line_num: usize = 1;
        let mut chars = source.graphemes(true);

        // Find the line start
        while current_position != marker_position {
            let current_char = chars.next();
            match current_char {
                None => bail!("Reached the end of the string before finding marker_position"),
                Some(current_char) => {
                    if Self::is_newline(current_char) {
                        line_start_position = current_position + 1;
                        line_num += 1;
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
        let line_number_string = format!("{line_num}. ");
        let spaces = " ".repeat(line_number_string.len() + marker_offset - 1);

        *line_number = line_num;
        Ok(format!("{line_number_string}{line}\n{spaces}^"))
    }

    /// Peeks current character.
    ///
    /// Returns `None` if we are already at the end of the string/asked it
    /// to return outside bounds
    fn peek_current(&self) -> Option<&str> {
        Self::peek_no_borrow(&self.source, self.current)
    }
    
    fn peek_no_borrow(source: &String, peek_index: usize) -> Option<&str> {
        source.graphemes(true).nth(peek_index)
    }
    
    fn is_newline(string: &str) -> bool {
        match string {
            "\r\n" | "\n" => true, 
            _ => false
        }
    }

    fn produce_error_with_location(&self, scanner_error_type: ScannerErrorType, location: usize) -> ScannerError {
        ScannerError::new_with_location(scanner_error_type, location, &*self.source)
    }

    fn produce_error(&self, scanner_error_type: ScannerErrorType) -> ScannerError {
        ScannerError::new(scanner_error_type, self.line)
    }
}

#[derive(Debug)]
pub struct ScannerError {
    error_type: ScannerErrorType,
    line: usize,
    location: Option<String>
}

impl ScannerError {
    pub fn new(error_type: ScannerErrorType, line: usize) -> Self {
        ScannerError { error_type, line, location: None }
    }

    pub fn new_with_location(error_type: ScannerErrorType, marker_position: usize, source: &str) -> Self {
        let mut line_number: usize = 0;

        let location = Scanner::get_line_with_marker(source, marker_position, &mut line_number)
            .expect("Couldn't produce line with a marker");
        ScannerError { error_type, line: line_number, location: Some(location) }
    }
}

#[derive(Debug)]
pub enum ScannerErrorType {
    UnknownToken(String),
    UnterminatedString,
    // String is a "reason"
    IncorrectNumberToken(String),
    NoMoreTokens,
    UnterminatedMultilineComment
}

impl Error for ScannerError {}
impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Scanner Error", self.line);

        let output = match &self.error_type {
            ScannerErrorType::UnknownToken(lexeme) => format!("Unknown token ('{}')", lexeme),
            ScannerErrorType::UnterminatedString => "Unterminated string".parse().unwrap(),
            ScannerErrorType::IncorrectNumberToken(reason) 
                => format!("The number was written in an incorrect format: {}", reason),
            ScannerErrorType::NoMoreTokens => "There were no more tokens to parse".parse().unwrap(),
            ScannerErrorType::UnterminatedMultilineComment => "Multiline comment was not terminated".parse().unwrap()
        };

        write!(f, "{error_header}: {}", output)?;
        if let Some(location) = &self.location {
            write!(f, "\n\nHappened here:\n{}", location)?;
        }
        
        Ok(())
    }
}