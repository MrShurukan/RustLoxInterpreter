use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};

#[derive(Debug)]
pub enum Expression<'a> {
    Binary { left: Box<Expression<'a>>, operator: PunctuationType, right: Box<Expression<'a>> },
    Grouping { expression: Box<Expression<'a>> },
    Literal { value: &'a LiteralType },
    Unary { operator: PunctuationType, right: Box<Expression<'a>> }
}

impl Expression<'_> {
    pub fn lisp_like_print(&self) -> String {
        match self {
            Self::Binary { left, operator, right } => {
                Self::parenthesize(
                    format!("{:?}", operator).as_str(),
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
                    LiteralType::Nil => { "nil".parse().unwrap() },
                    LiteralType::Boolean(bool) => { bool.to_string() }
                }
            }
            Self::Unary { operator, right } => {
                Self::parenthesize(
                    format!("{:?}", operator).as_str(),
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
    fn small_expression_test() {
        let expression = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: PunctuationType::Minus,
                right: Box::new(Expression::Literal { value: &LiteralType::Number(123.0) })
            }),
            operator: PunctuationType::Star,
            right: Box::new(Expression::Grouping {
                expression: Box::new(Expression::Literal { value: &LiteralType::Number(45.67) })
            }),
        };

        assert_eq!("(Punctuation(Star) (Punctuation(Minus) 123) (group 45.67))", expression.lisp_like_print());
    }
}