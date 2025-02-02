use crate::lox_interpreter::expression::Expression;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{KeywordType as KT, LiteralType, PunctuationType as PT, PunctuationType, TokenType as TT, TokenType};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser<'a> {
    tokens: &'a [Token]
}


macro_rules! parse_binary {
    ($enum_types:pat_param, $expression:ident, $tokens:ident) => {
        while let TT::Punctuation(op @ $enum_types) = &Self::peek($tokens)?.token_type {
            $tokens.next();
            let right = Self::comparison($tokens)?;
            
            $expression = Expression::Binary {
                left: Box::new($expression),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }
    };
}

impl Parser<'_> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser { tokens }
    }

    pub fn parse(&self) -> Vec<Expression> {
        todo!()
    }

    fn expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        Self::equality(tokens)
    }

    // != ==
    fn equality<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = Self::comparison(tokens)?;
        parse_binary!((PT::BangEqual | PT::EqualEqual), expr, tokens);
        
        Ok(expr)
    }

    // > >= < <=
    fn comparison<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = Self::term(tokens)?;
        parse_binary!((PT::Greater | PT::GreaterEqual | PT::Less | PT::LessEqual), expr, tokens);

        Ok(expr)
    }

    // + -
    fn term<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = Self::factor(tokens)?;
        parse_binary!((PT::Plus | PT::Minus), expr, tokens);
        
        Ok(expr)
    }

    // * /
    fn factor<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        let mut expr = Self::unary(tokens)?;
        parse_binary!((PunctuationType::Star | PunctuationType::Slash), expr, tokens);
        
        Ok(expr)
    }

    // ! - (as unary operator)
    fn unary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        if let TT::Punctuation(op @ (PT::Bang | PT::Minus)) = &Self::peek(tokens)?.token_type {
            tokens.next();
            let right = Self::unary(tokens)?;

            return Ok(Expression::Unary {
                operator: op.to_owned(),
                right: Box::new(right)
            });
        }

        Self::primary(tokens)
    }

    // Literals, groupings
    fn primary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError<'a>> {
        let token = Self::peek(tokens)?;
        let result = match &token.token_type {
            TT::Literal(literal_type) => { Some(Expression::Literal { value: literal_type }) }
            TT::Keyword(KT::True) => { Some(Expression::Literal { value: &LiteralType::Boolean(true) }) }
            TT::Keyword(KT::False) => { Some(Expression::Literal { value: &LiteralType::Boolean(false) }) }
            TT::Keyword(KT::Nil) => { Some(Expression::Literal { value: &LiteralType::Nil }) }
            TT::Punctuation(PT::LeftParen) => {
                tokens.next();
                let expr = Self::expression(tokens)?;
                let next_token = tokens.peek();
                if next_token.is_none() {
                    Self::report_non_critical(
                        ParserError::new(ParserErrorType::UnclosedBraces, tokens.last().unwrap())
                    )
                }
                else {
                    let next_token = next_token.unwrap();
                    if let TT::Punctuation(PT::RightBrace) = next_token.token_type {
                        // Consume the ')'
                        tokens.next();
                    }
                    else {
                        Self::report_non_critical(
                            ParserError::new(ParserErrorType::UnclosedBraces, next_token)
                        )
                    }
                }
                
                Some(Expression::Grouping { expression: Box::new(expr) })
            },
            _ => { None }
        };

        if result.is_some() { tokens.next(); }
        result.ok_or_else(|| ParserError::new(ParserErrorType::UnknownToken, token) )
    }

    fn peek<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<&'a Token, ParserError<'a>> {
        let token = tokens.peek();
        if token.is_none() {
            Err(ParserError::new_without_token(ParserErrorType::RanOutOfTokens))
        } 
        else {
            Ok(token.unwrap())
        }
    }
    
    fn report_non_critical(error: ParserError) {
        println!("{error:?}")
    }
}

#[derive(Debug)]
pub struct ParserError<'a> {
    error_type: ParserErrorType,
    token: &'a Token
}

#[derive(Debug)]
pub enum ParserErrorType {
    UnclosedBraces,
    RanOutOfTokens,
    UnknownToken
}

impl ParserError<'_> {
    pub fn new(error_type: ParserErrorType, token: &Token) -> ParserError {
        ParserError { error_type, token }
    }

    pub fn new_without_token<'a>(error_type: ParserErrorType) -> ParserError<'a> {
        ParserError { 
            error_type, 
            token: &Token { token_type: TokenType::EOF, line: 0 } 
        }
    }
}

impl Error for ParserError<'_> {}
impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            format!("[line {}] Parser Error", self.token.line);

        let output = match &self.error_type {
            ParserErrorType::UnclosedBraces => "Unclosed braces",
            ParserErrorType::RanOutOfTokens => "No more tokens to parse",
            ParserErrorType::UnknownToken => "Unknown token in this place (don't know how to parse)"
        };

        write!(f, "{error_header}: {}", output)?;
        // if let Some(location) = &self.location {
        //     write!(f, "\n\nHappened here:\n{}", location)?;
        // }

        Ok(())
    }
}