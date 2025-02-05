use crate::lox_interpreter::environment::{EnvironmentError, EnvironmentErrorType};
use crate::lox_interpreter::environment_stack::EnvironmentStack;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{LiteralType, PunctuationType, TokenType};
use crate::lox_interpreter::value::Value;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression_type: ExpressionType,
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
    Unary { operator: PunctuationType, right: Box<Expression> },
    Assignment { identifier: String, expression: Box<Expression> }
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
            unreachable!("Non-literal token passed to new_literal expression function");
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
            unreachable!("Non-punctuation token passed to new_unary expression function");
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
        if let TokenType::Punctuation(op) = &operator.token_type {
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
            unreachable!("Non-punctuation token passed to new_binary expression function");
        }
    }

    pub fn new_assignment(identifier: Expression, expression: Expression) -> Self {
        if let ExpressionType::Literal { value: LiteralType::Identifier(name) } = &identifier.expression_type {
            Self {
                line_start: identifier.line_start,
                line_end: expression.line_end,
                start_offset: identifier.start_offset,
                end_offset: expression.end_offset,
                expression_type: ExpressionType::Assignment {
                    identifier: name.to_owned(),
                    expression: Box::new(expression),
                },
            }
        }
        else {
            unreachable!("Non-identifier token passed to new_assignment expression function");
        }
    }

    pub fn evaluate(&self, environments: &mut EnvironmentStack) -> Result<Value, EvaluationError> {
        match &self.expression_type {
            ExpressionType::Literal { value } => {
                match value {
                    LiteralType::String(str) => Ok(Value::String(str.to_owned())),
                    LiteralType::Number(num) => Ok(Value::Number(*num)),
                    LiteralType::Boolean(bool) => Ok(Value::Boolean(*bool)),
                    LiteralType::Nil => Ok(Value::Nil),

                    LiteralType::Identifier(name) => {
                        let value = environments.get(name).ok_or_else(|| {
                            self.error(EvaluationErrorType::UndefinedVariable(name.to_owned()))
                        })?;
                        
                        if value.is_not_init() {
                            Err(self.error(EvaluationErrorType::NotInitVariableEval(name.to_owned())))
                        }
                        else {
                            Ok(value)
                        }
                    }
                }
            },
            ExpressionType::Grouping { expression } => {
                expression.evaluate(environments)
            },
            ExpressionType::Unary { operator, right } => {
                let right = right.evaluate(environments)?;

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
                let left = left.evaluate(environments)?;
                let right = right.evaluate(environments)?;

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
                            if let Value::String(_) = left {
                                Ok(Value::String(self.concat_convert(left, right)?))
                            }
                            else {
                                Err(left_num.err().unwrap())
                            }
                        }
                        else {
                            match &right {
                                Value::String(_) => {
                                    Ok(Value::String(self.concat_convert(left, right)?))
                                },
                                Value::Number(right_num) => {
                                    Ok(Value::Number(left_num.ok().unwrap() + right_num))
                                },
                                _ => Err(self.error(
                                    EvaluationErrorType::IncorrectTypeForArithmeticOperation {
                                        value: right,
                                        operation: "addition"
                                    }))
                            }
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
                let first = first.evaluate(environments)?;
                if first.is_truthy() {
                    Ok(second.evaluate(environments)?)
                }
                else {
                    Ok(third.evaluate(environments)?)
                }
            },
            ExpressionType::Assignment { identifier, expression } => {
                let value = expression.evaluate(environments)?;
                let assign_result = environments.assign(identifier.to_owned(), value.to_owned());

                if let Err(EnvironmentError { ref environment_error_type }) = assign_result {
                    match environment_error_type {
                        EnvironmentErrorType::UndefinedVariable(name) => {
                            Err(self.error(EvaluationErrorType::UndefinedVariable(name.to_owned())))
                        },
                        EnvironmentErrorType::LastEnvironmentRemoved => {
                            println!("{:?}", assign_result.err().unwrap());
                            panic!("Last environment was removed from the environment stack");
                        }
                    }
                }
                else {
                    Ok(value)
                }
            }
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

    fn concat_convert(&self, left: Value, right: Value) -> Result<String, EvaluationError> {
        let left = self.convert_to_str(left);
        let right = self.convert_to_str(right);

        Ok(left + &right)
    }

    fn convert_to_str(&self, value: Value) -> String {
        format!("{value}")
    }

    fn error(&self, evaluation_error_type: EvaluationErrorType) -> EvaluationError {
        EvaluationError { expression: self.to_owned(), evaluation_error_type }
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
    IncorrectBinaryOperator(PunctuationType),
    DivisionByZero,
    UndefinedVariable(String),
    NotInitVariableEval(String)
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
            EvaluationErrorType::DivisionByZero =>
                "Division by zero",
            EvaluationErrorType::UndefinedVariable(name) =>
                &format!("Undefined variable '{name}'"),
            EvaluationErrorType::NotInitVariableEval(name) =>
                &format!("Variable '{name}' wasn't initialized")
        };

        write!(f, "{error_header}\n{message}")?;

        Ok(())
    }
}