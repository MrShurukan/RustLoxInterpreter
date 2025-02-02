﻿use crate::lox_interpreter::expression::Expression;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{KeywordType as KT, LiteralType, PunctuationType as PT, PunctuationType, TokenType as TT, TokenType};
use std::error::Error;
use std::fmt::{format, Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use anyhow::bail;
use unicode_segmentation::UnicodeSegmentation;

pub struct Parser {
    tokens: Vec<Token>,
    /// Reference to an original string to point out errors with a location
    source: String
}


macro_rules! parse_binary {
    ($self:ident, $enum_types:pat_param, $expression:ident, $tokens:ident) => {
        while let TT::Punctuation(op @ $enum_types) = &Self::peek($tokens)?.token_type {
            $tokens.next();
            let right = $self.comparison($tokens)?;

            $expression = Expression::Binary {
                left: Box::new($expression),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String) -> Self {
        Self { tokens, source }
    }

    pub fn parse(&self) -> Result<Vec<Expression>, ParserError> {
        Ok(vec![self.expression(&mut self.tokens.iter().peekable())?])
    }

    fn expression<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        self.equality(tokens)
    }

    // != ==
    fn equality<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {

        let mut expr = self.comparison(tokens)?;
        parse_binary!(self, (PT::BangEqual | PT::EqualEqual), expr, tokens);

        Ok(expr)
    }

    // > >= < <=
    fn comparison<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = self.term(tokens)?;
        parse_binary!(self, (PT::Greater | PT::GreaterEqual | PT::Less | PT::LessEqual), expr, tokens);

        Ok(expr)
    }

    // + -
    fn term<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = self.factor(tokens)?;
        parse_binary!(self, (PT::Plus | PT::Minus), expr, tokens);

        Ok(expr)
    }

    // * /
    fn factor<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = self.unary(tokens)?;
        parse_binary!(self, (PunctuationType::Star | PunctuationType::Slash), expr, tokens);

        Ok(expr)
    }

    // ! - (as unary operator)
    fn unary<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        if let TT::Punctuation(op @ (PT::Bang | PT::Minus)) = &Self::peek(tokens)?.token_type {
            tokens.next();
            let right = self.unary(tokens)?;

            return Ok(Expression::Unary {
                operator: op.to_owned(),
                right: Box::new(right)
            });
        }

        self.primary(tokens)
    }

    // Literals, groupings
    fn primary<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let token = Self::peek(tokens)?;

        let result = match &token.token_type {
            TT::Literal(literal_type) => { Some(Expression::Literal { value: literal_type }) }
            TT::Keyword(KT::True) => { Some(Expression::Literal { value: &LiteralType::Boolean(true) }) }
            TT::Keyword(KT::False) => { Some(Expression::Literal { value: &LiteralType::Boolean(false) }) }
            TT::Keyword(KT::Nil) => { Some(Expression::Literal { value: &LiteralType::Nil }) }
            TT::Punctuation(PT::LeftParen) => {
                tokens.next();
                let expr = self.expression(tokens)?;
                let next_token = tokens.peek();
                if next_token.is_none() {
                    Self::report_non_critical(
                        ParserError::new(ParserErrorType::UnclosedParenthesis, (*token).clone())
                    )
                }
                else {
                    let next_token = next_token.unwrap();
                    match next_token.token_type {
                        TT::Punctuation(PT::RightParen) => {},
                        
                        _ => Self::report_non_critical(
                            ParserError::new(ParserErrorType::UnclosedParenthesis, (*next_token).clone())
                        )
                    }
                }

                Some(Expression::Grouping { expression: Box::new(expr) })
            },
            _ => { None }
        };

        if result.is_some() { tokens.next(); }
        result.ok_or_else(|| Self::get_error_marked_line(ParserErrorType::IncorrectToken, (*token).clone(), &self.source))
    }

    fn peek<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<&'a Token, ParserError> {
        let token = tokens.peek();
        if token.is_none() {
            Err(ParserError::new_without_token(ParserErrorType::RanOutOfTokens))
        }
        else {
            Ok(token.unwrap())
        }
    }

    fn report_non_critical(error: ParserError) {
        println!("{error}")
    }

    fn get_error_marked_line(error_type: ParserErrorType, token: Token, source: &str) -> ParserError {
        let line = source.split("\n").nth(token.line - 1);
        if line.is_none() {
            return ParserError {
                error_type: ParserErrorType::IncorrectLineNumberProvided(token.line), token,
                location: None };
        }

        let line = line.unwrap();
        let line_number_string = format!("{}. ", token.line);
        let spaces = " ".repeat(line_number_string.len() + token.line_offset - 1);
        let marker = "^".repeat(token.lexeme_size);

        let location = format!("{line_number_string}{line}\n{spaces}{marker}");
        ParserError { location: Some(location), error_type, token }
    }
}

#[derive(Debug)]
pub struct ParserError {
    error_type: ParserErrorType,
    token: Token,
    location: Option<String>
}

#[derive(Debug)]
pub enum ParserErrorType {
    UnclosedParenthesis,
    RanOutOfTokens,
    IncorrectToken,
    /// Internal, not supposed to occur
    IncorrectLineNumberProvided(usize)
}

impl ParserError {
    pub fn new(error_type: ParserErrorType, token: Token) -> ParserError {
        ParserError { error_type, token, location: None }
    }

    pub fn new_without_token<'a>(error_type: ParserErrorType) -> ParserError {
        ParserError {
            error_type,
            token: Token { token_type: TokenType::EOF, line: 0, lexeme_size: 0, line_offset: 0 },
            location: None
        }
    }
}

impl Error for ParserError {}
impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Parser Error", self.token.line);

        let output = match &self.error_type {
            ParserErrorType::UnclosedParenthesis => "Unclosed parenthesis",
            ParserErrorType::RanOutOfTokens => "No more tokens to parse",
            ParserErrorType::IncorrectToken => "Incorrect token in this place",
            ParserErrorType::IncorrectLineNumberProvided(line) =>
                &*format!("[Internal parser error] Couldn't get {} as a line number (token: {:?})", line, self.token)
        };

        write!(f, "{error_header}: {}", output)?;
        if let Some(location) = &self.location {
            write!(f, "\n\nHappened here:\n{}", location)?;
        }

        Ok(())
    }
}