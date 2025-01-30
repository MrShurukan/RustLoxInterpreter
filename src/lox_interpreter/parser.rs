use std::any::Any;
use std::iter::Peekable;
use std::slice::Iter;
use crate::lox_interpreter::expression::Expression;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{KeywordType as KT, KeywordType, LiteralType, PunctuationType as PT, PunctuationType, TokenType as TT, TokenType};

pub struct Parser<'a> {
    tokens: &'a [Token]
}

// TODO: Try to improve on redundant boilerplate code
impl Parser<'_> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser { tokens }
    }

    pub fn parse(&self) -> Vec<Expression> {
        todo!()
    }

    fn expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        Self::equality(tokens)
    }

    // != ==
    fn equality<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        let mut expr = Self::comparison(tokens)?;

        while let TT::Punctuation(op @ (
            PT::BangEqual | PT::EqualEqual)) = &tokens.peek()?.token_type {

            tokens.next();
            let right = Self::comparison(tokens)?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }

        Some(expr)
    }

    // > >= < <=
    fn comparison<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        let mut expr = Self::term(tokens)?;

        while let TT::Punctuation(op @ (
            PT::Greater | PT::GreaterEqual | PT::Less | PT::LessEqual)) = &tokens.peek()?.token_type {

            tokens.next();
            let right = Self::term(tokens)?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }

        Some(expr)
    }

    // + -
    fn term<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        let mut expr = Self::factor(tokens)?;

        while let TT::Punctuation(op @ (
            PunctuationType::Plus | PunctuationType::Minus)) = &tokens.peek()?.token_type {

            tokens.next();
            let right = Self::factor(tokens)?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }

        Some(expr)
    }

    // * /
    fn factor<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        let mut expr = Self::unary(tokens)?;

        while let TT::Punctuation(op @ (
            PunctuationType::Star | PunctuationType::Slash)) = &tokens.peek()?.token_type {

            tokens.next();
            let right = Self::unary(tokens)?;

            expr = Expression::Binary {
                left: Box::new(expr),
                operator: op.to_owned(),
                right: Box::new(right)
            }
        }

        Some(expr)
    }

    // ! - (as unary operator)
    fn unary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        if let TT::Punctuation(op @ (
            PunctuationType::Bang | PunctuationType::Minus)) = &tokens.peek()?.token_type {

            tokens.next();
            let right = Self::unary(tokens)?;

            return Some(Expression::Unary {
                operator: op.to_owned(),
                right: Box::new(right)
            });
        }

        Self::primary(tokens)
    }

    // Literals, groupings
    fn primary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Option<Expression<'a>> {
        let result = match &tokens.peek()?.token_type {
            TT::Literal(literal_type) => { Some(Expression::Literal { value: literal_type }) }
            TT::Keyword(KeywordType::True) => { Some(Expression::Literal { value: &LiteralType::Boolean(true) }) }
            TT::Keyword(KeywordType::False) => { Some(Expression::Literal { value: &LiteralType::Boolean(false) }) }
            TT::Keyword(KeywordType::Nil) => { Some(Expression::Literal { value: &LiteralType::Nil }) }
            TT::Punctuation(PT::LeftParen) => {
                tokens.next();
                let expr = Self::expression(tokens)?;
                if let TT::Punctuation(PT::RightBrace) = &tokens.peek()?.token_type {
                }
                else {
                    println!("Expect ')' after expression.");
                }
                Some(Expression::Grouping { expression: Box::new(expr) })
            },
            _ => { None }
        };

        if result.is_some() { tokens.next(); }
        result
    }
}

