use std::ops::Deref;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::LiteralType;

#[derive(Debug)]
pub enum Expression<'a> {
    Binary { left: &'a Expression<'a>, operator: &'a Token, right: &'a Expression<'a> },
    Grouping { expression: &'a Expression<'a> },
    Literal { value: &'a LiteralType },
    Unary { operator: &'a Token, right: &'a Expression<'a> }
}

impl Expression<'_> {
    pub fn lisp_like_print(&self) -> String {
        match self {
            Self::Binary { left, operator, right } => {
                Self::parenthesize(
                    format!("{:?}", operator.token_type).as_str(),
                    vec![left, right]
                )
            }
            Self::Grouping { expression } => {
                Self::parenthesize(
                    "group",
                    vec![expression]
                )
            }
            Self::Literal { value } => {
                match value {
                    LiteralType::Identifier(identifier) => { identifier.to_owned() }
                    LiteralType::String(string) => { string.to_owned() }
                    LiteralType::Number(number) => { number.to_string() }
                    LiteralType::Nil => { "nil".parse().unwrap() }
                }
            }
            Self::Unary { operator, right } => {
                Self::parenthesize(
                    format!("{:?}", operator.token_type).as_str(),
                    vec![right]
                )
            }
        }
    }

    fn parenthesize(name: &str, expressions: Vec<&Expression>) -> String {
        let mut output = String::new();

        output.push('(');
        output.push_str(name);

        for expression in expressions {
            output.push(' ');
            output.push_str(expression.lisp_like_print().as_str())
        }

        output.push(')');

        output
    }
}

#[cfg(test)]
mod tests {
    use crate::lox_interpreter::token_type::{PunctuationType, TokenType};
    use super::*;

    #[test]
    fn it_works() {
        let expression = Expression::Binary {
            left: &Expression::Unary {
                operator: &Token { token_type: TokenType::Punctuation(PunctuationType::Minus), line: 0 },
                right: &Expression::Literal { value: &LiteralType::Number(123.0) }
            },
            operator: &Token { token_type: TokenType::Punctuation(PunctuationType::Star), line: 0 },
            right: &Expression::Grouping {
                expression: &Expression::Literal { value: &LiteralType::Number(45.67) }
            },
        };

        println!("{}", expression.lisp_like_print());
    }
}