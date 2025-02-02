use crate::lox_interpreter::expression::Expression;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{KeywordType as KT, LiteralType, PunctuationType as PT, PunctuationType, TokenType as TT, TokenType};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser {
    tokens: Vec<Token>,
    // /// Reference to an original string to point out errors with a location
    // source: String
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

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Result<Vec<Expression>, ParserError> {
        Ok(vec![Self::expression(&mut self.tokens.iter().peekable())?])
    }

    fn expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        Self::equality(tokens)
    }

    // != ==
    fn equality<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {

        let mut expr = Self::comparison(tokens)?;
        parse_binary!((PT::BangEqual | PT::EqualEqual), expr, tokens);

        Ok(expr)
    }

    // > >= < <=
    fn comparison<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = Self::term(tokens)?;
        parse_binary!((PT::Greater | PT::GreaterEqual | PT::Less | PT::LessEqual), expr, tokens);

        Ok(expr)
    }

    // + -
    fn term<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = Self::factor(tokens)?;
        parse_binary!((PT::Plus | PT::Minus), expr, tokens);

        Ok(expr)
    }

    // * /
    fn factor<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
        let mut expr = Self::unary(tokens)?;
        parse_binary!((PunctuationType::Star | PunctuationType::Slash), expr, tokens);

        Ok(expr)
    }

    // ! - (as unary operator)
    fn unary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
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
    fn primary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression<'a>, ParserError> {
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
                        ParserError::new(ParserErrorType::UnclosedBraces, (*token).clone())
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
                            ParserError::new(ParserErrorType::UnclosedBraces, (*next_token).clone())
                        )
                    }
                }

                Some(Expression::Grouping { expression: Box::new(expr) })
            },
            _ => { None }
        };

        if result.is_some() { tokens.next(); }
        result.ok_or_else(|| ParserError::new(ParserErrorType::UnknownToken, (*token).clone()) )
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
        println!("{error:?}")
    }
}

#[derive(Debug)]
pub struct ParserError {
    error_type: ParserErrorType,
    token: Token
}

#[derive(Debug)]
pub enum ParserErrorType {
    UnclosedBraces,
    RanOutOfTokens,
    UnknownToken
}

impl ParserError {
    pub fn new(error_type: ParserErrorType, token: Token) -> ParserError {
        ParserError { error_type, token }
    }

    pub fn new_without_token<'a>(error_type: ParserErrorType) -> ParserError {
        ParserError {
            error_type,
            token: Token { token_type: TokenType::EOF, line: 0 }
        }
    }
}

impl Error for ParserError {}
impl Display for ParserError {
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

#[cfg(test)]
mod tests {
    use crate::lox_interpreter::parser::Parser;
    use crate::lox_interpreter::token::Token;
    use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};

    #[test]
    fn medium_expression_test() {
        let tokens = vec![Token {
            token_type: TokenType::Literal(LiteralType::Number(123.0)),
            line: 1
        }, Token {
            token_type: TokenType::Punctuation(PunctuationType::EqualEqual),
            line: 1
        }, Token {
            token_type: TokenType::Literal(LiteralType::Number(321.0)),
            line: 1
        }, Token {
            token_type: TokenType::Punctuation(PunctuationType::BangEqual),
            line: 1
        }, Token {
            token_type: TokenType::Literal(LiteralType::String("Test".parse().unwrap())),
            line: 1
        }, Token {
            token_type: TokenType::EOF,
            line: 1
        }];

        let parser = Parser::new(tokens);
        println!("{:?}", parser.parse());
    }

    #[test]
    fn small_expression_test() {
        let tokens = vec![Token {
            token_type: TokenType::Literal(LiteralType::Number(123.0)),
            line: 1
        }, Token {
            token_type: TokenType::EOF,
            line: 1
        }];

        let parser = Parser::new(tokens);
        println!("{:?}", parser.parse());
    }
}