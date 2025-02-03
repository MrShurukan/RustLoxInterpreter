use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::lox_interpreter::parser::{ParserError, ParserErrorType};
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};
use crate::lox_interpreter::value::Value;

#[derive(Debug, Clone)]
pub struct Expression {
    expression_type: ExpressionType,
    pub line_start: usize,
    pub line_end: usize,
    /// Offset in grapheme count from the start of the first line
    pub start_offset: usize,
    /// Offset in grapheme count from the start of the last file
    pub end_offset: usize
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Binary { left: Box<Expression>, operator: PunctuationType, right: Box<Expression> },
    Ternary { first: Box<Expression>, second: Box<Expression>, third: Box<Expression> },
    Grouping { expression: Box<Expression> },
    Literal { value: LiteralType },
    Unary { operator: PunctuationType, right: Box<Expression> }
}

impl Expression {
    pub fn new_literal(token: &Token) -> Self {
        if let Token { token_type: TokenType::Literal(literal_type), .. } = token {
            Self {
                expression_type: ExpressionType::Literal { value: literal_type.to_owned() },
                line_start: token.line,
                line_end: token.line,
                start_offset: token.line_offset,
                end_offset: token.line_offset + token.lexeme_size,
            }
        }
        else {
            panic!("Non-literal token passed to new_literal expression function");
        }
    }

    pub fn new_literal_custom_type(token: &Token, literal_type: LiteralType) -> Self {
        Self {
            expression_type: ExpressionType::Literal { value: literal_type },
            line_start: token.line,
            line_end: token.line,
            start_offset: token.line_offset,
            end_offset: token.line_offset + token.lexeme_size,
        }
    }
    
    pub fn new_grouping(expression: Expression) -> Self {
        Self {
            line_start: expression.line_start,
            line_end: expression.line_end,
            start_offset: expression.start_offset - 1,
            end_offset: expression.end_offset + 1,
            expression_type: ExpressionType::Grouping { expression: Box::new(expression) },
        }
    }
    
    pub fn new_unary(operator: &Token, expression: Expression) -> Self {
        if let Token { token_type: TokenType::Punctuation(op), .. } = operator {
            Self {
                line_start: operator.line,
                line_end: expression.line_end,
                start_offset: operator.line_offset,
                end_offset: expression.end_offset,
                expression_type: ExpressionType::Unary { operator: op.to_owned(), right: Box::new(expression) },
            }
        }
        else {
            panic!("Non-punctuation token passed to new_unary expression function");
        }
    }
    
    pub fn new_ternary(first: Expression, second: Expression, third: Expression) -> Self {
        Self {
            line_start: first.line_start,
            line_end: third.line_end,
            start_offset: first.start_offset,
            end_offset: third.end_offset,
            expression_type: ExpressionType::Ternary { 
                first: Box::new(first),
                second: Box::new(second),
                third: Box::new(third)
            },
        }
    }
    
    pub fn new_binary(left: Expression, operator: &Token, right: Expression) -> Self {
        if let Token { token_type: TokenType::Punctuation(op), .. } = operator {
            Self {
                line_start: left.line_start,
                line_end: right.line_end,
                start_offset: left.start_offset,
                end_offset: right.end_offset,
                expression_type: ExpressionType::Binary {
                    left: Box::new(left),
                    operator: op.to_owned(),
                    right: Box::new(right)
                },
            }
        }
        else {
            panic!("Non-punctuation token passed to new_binary expression function");
        }
    }
    
    pub fn evaluate(&self) -> Result<Value, EvaluationError> {
        match &self.expression_type {
            ExpressionType::Literal { value } => {
                match value {
                    LiteralType::Identifier(_) => { todo!("Can't evaluate identifiers yet") },
                    LiteralType::String(str) => Ok(Value::String(str.to_owned())),
                    LiteralType::Number(num) => Ok(Value::Number(*num)),
                    LiteralType::Boolean(bool) => Ok(Value::Boolean(*bool)),
                    LiteralType::Nil => Ok(Value::Nil)
                }
            },
            ExpressionType::Grouping { expression } => {
                expression.evaluate()
            },
            ExpressionType::Unary { operator, right } => {
                let right = right.evaluate()?;

                match operator {
                    PunctuationType::Minus => {
                        let number = self.ensure_number_for_arithmetic(right, "negation")?;
                        Ok(Value::Number(-number))
                    },
                    PunctuationType::Bang => {
                        let truthy = right.is_truthy();
                        Ok(Value::Boolean(!truthy))
                    },
                    _ => { Err(self.error(EvaluationErrorType::IncorrectUnaryOperator(operator.to_owned()))) }
                }
            },
            ExpressionType::Binary { left, operator, right } => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;

                match operator {
                    // - * / +
                    PunctuationType::Minus => {
                        let left = self.ensure_number_for_arithmetic(left, "subtraction")?;
                        let right = self.ensure_number_for_arithmetic(right, "subtraction")?;

                        Ok(Value::Number(left - right))
                    },
                    PunctuationType::Star => {
                        let left = self.ensure_number_for_arithmetic(left, "multiplication")?;
                        let right = self.ensure_number_for_arithmetic(right, "multiplication")?;

                        Ok(Value::Number(left * right))
                    },
                    PunctuationType::Slash => {
                        let left = self.ensure_number_for_arithmetic(left, "division")?;
                        let right = self.ensure_number_for_arithmetic(right, "division")?;

                        if right == 0.0 {
                            return Err(self.error(EvaluationErrorType::DivisionByZero));
                        }
                        
                        Ok(Value::Number(left / right))
                    },
                    PunctuationType::Plus => {
                        let left_num = self.ensure_number_for_arithmetic_copy(&left, "addition");
                        if left_num.is_err() {
                            let left = self.ensure_string_for_concat(left)?;
                            let right = self.ensure_string_for_concat(right)?;

                            Ok(Value::String(format!("{left}{right}")))
                        }
                        else {
                            let left_num = left_num.expect("");
                            let right_num = self.ensure_number_for_arithmetic(right, "addition")?;

                            Ok(Value::Number(left_num + right_num))
                        }
                    },

                    // > >= < <=
                    PunctuationType::Greater => {
                        let left = self.ensure_number_for_comparison(left)?;
                        let right = self.ensure_number_for_comparison(right)?;
                        
                        Ok(Value::Boolean(left > right))
                    },
                    PunctuationType::GreaterEqual => {
                        let left = self.ensure_number_for_comparison(left)?;
                        let right = self.ensure_number_for_comparison(right)?;

                        Ok(Value::Boolean(left >= right))
                    },
                    PunctuationType::Less => {
                        let left = self.ensure_number_for_comparison(left)?;
                        let right = self.ensure_number_for_comparison(right)?;

                        Ok(Value::Boolean(left < right))
                    },
                    PunctuationType::LessEqual => {
                        let left = self.ensure_number_for_comparison(left)?;
                        let right = self.ensure_number_for_comparison(right)?;

                        Ok(Value::Boolean(left <= right))
                    },
                    
                    // == !=
                    PunctuationType::EqualEqual => {
                        Ok(Value::Boolean(left == right))
                    },
                    PunctuationType::BangEqual => {
                        Ok(Value::Boolean(left != right))
                    }

                    _ => Err(self.error(EvaluationErrorType::IncorrectBinaryOperator(operator.to_owned())))
                }
            },
            ExpressionType::Ternary { first, second, third } => { 
                let first = first.evaluate()?;
                if first.is_truthy() {
                    Ok(second.evaluate()?)
                }
                else {
                    Ok(third.evaluate()?)
                }
            },
        }
    }

    fn ensure_number_for_arithmetic(&self, value: Value, operation: &'static str) -> Result<f64, EvaluationError>  {
        if let Value::Number(number) = value {
            Ok(number)
        }
        else {
            Err(self.error(EvaluationErrorType::IncorrectTypeForArithmeticOperation { value, operation }))
        }
    }

    fn ensure_number_for_comparison(&self, value: Value) -> Result<f64, EvaluationError>  {
        if let Value::Number(number) = value {
            Ok(number)
        }
        else {
            Err(self.error(EvaluationErrorType::IncorrectTypeForComparison(value)))
        }
    }

    fn ensure_number_for_arithmetic_copy(&self, value: &Value, operation: &'static str) -> Result<f64, EvaluationError>  {
        if let Value::Number(number) = value {
            Ok(number.to_owned())
        }
        else {
            Err(self.error(
                EvaluationErrorType::IncorrectTypeForArithmeticOperation { 
                    value: value.to_owned(), operation 
                })
            )
        }
    }

    fn ensure_string_for_concat(&self, value: Value) -> Result<String, EvaluationError>  {
        if let Value::String(str) = value {
            Ok(str)
        }
        else {
            Err(self.error(EvaluationErrorType::IncorrectTypeForConcatenation(value)))
        }
    }
    
    fn error(&self, evaluation_error_type: EvaluationErrorType) -> EvaluationError {
        EvaluationError { expression: self.to_owned(), evaluation_error_type }
    }

    pub fn lisp_like_print(&self) -> String {
        match &self.expression_type {
            ExpressionType::Binary { left, operator, right } => {
                Self::parenthesize(
                    format!("{}", operator).as_str(),
                    vec![left, right]
                )
            }
            ExpressionType::Grouping { expression } => {
                Self::parenthesize(
                    "group",
                    vec![expression]
                )
            }
            ExpressionType::Literal { value } => {
                match value {
                    LiteralType::Identifier(identifier) => { identifier.to_owned() }
                    LiteralType::String(string) => { string.to_owned() }
                    LiteralType::Number(number) => { number.to_string() }
                    LiteralType::Nil => { "nil".parse().unwrap() },
                    LiteralType::Boolean(bool) => { bool.to_string() }
                }
            }
            ExpressionType::Unary { operator, right } => {
                Self::parenthesize(
                    format!("{}", operator).as_str(),
                    vec![right]
                )
            }
            ExpressionType::Ternary { first, second, third } => {
                format!("{} ? {} : {}",
                        first.lisp_like_print(),
                        second.lisp_like_print(),
                        third.lisp_like_print())
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

#[derive(Debug)]
pub struct EvaluationError {
    pub evaluation_error_type: EvaluationErrorType,
    pub expression: Expression
}

#[derive(Debug)]
pub enum EvaluationErrorType {
    IncorrectUnaryOperator(PunctuationType),
    IncorrectTypeForArithmeticOperation { operation: &'static str, value: Value },
    IncorrectTypeForComparison(Value),
    IncorrectTypeForConcatenation(Value),
    IncorrectBinaryOperator(PunctuationType),
    DivisionByZero
}

impl Error for EvaluationError {}
impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_header =
            if self.expression.line_start == self.expression.line_end
                { format!("[line {}] Runtime Expression Error", self.expression.line_start) }
            else
                { format!("[lines {}-{}] Runtime Expression Error", self.expression.line_start, self.expression.line_end) };
        
        let message = match &self.evaluation_error_type {
            EvaluationErrorType::IncorrectUnaryOperator(op) =>
                &format!("Can't use {} as a unary operator", op),
            EvaluationErrorType::IncorrectBinaryOperator(op) =>
                &format!("Can't use {} as a binary operator", op),
            EvaluationErrorType::IncorrectTypeForArithmeticOperation { operation, value} =>
                &format!("Expected a number for {operation}, found {}", value.get_type_name()),
            EvaluationErrorType::IncorrectTypeForComparison(value) =>
                &format!("Expected a number for comparison, found {}", value.get_type_name()),
            EvaluationErrorType::IncorrectTypeForConcatenation(value) =>
                &format!("Expected a string for concatenation, found {}", value.get_type_name()),
            EvaluationErrorType::DivisionByZero =>
                "Division by zero",
        };

        write!(f, "{error_header}\n{message}")?;

        Ok(())
    }
}