use crate::lox_interpreter::environment::Environment;
use crate::lox_interpreter::expression::EvaluationError;
use crate::lox_interpreter::statement::Statement;
use std::error::Error;
use std::fmt::{Display, Formatter};

pub struct Interpreter<'a> {
    /// Source file, used to print errors
    pub source: &'a str,
    /// Environment stack. The latest environment on the stack represents the
    /// latest code block. You can access outer environments by moving back from 
    /// the end
    environment: Vec<Environment>
}

impl Interpreter<'_> {
    pub fn new(source: &str) -> Interpreter {
        Interpreter { source, environment: vec![Environment::new()] }
    } 
    
    pub fn interpret(&mut self, statements: &[Statement]) -> Result<(), RuntimeError> {
        for statement in statements {
            statement.execute(&mut self.environment)
                .or_else(|err| {
                    Err(RuntimeError {
                        location: Some(Self::get_error_marked_line(&err, self.source)),
                        runtime_error_type: RuntimeErrorType::Evaluation(err)
                    })
                })?;
        }
        
        Ok(())
    }

    // TODO: Refactor that a bit
    /// Gets a helper string for error displaying with a marker on the second line.
    /// ### Example
    /// This function is used to create a string that looks something like this:
    /// ```
    /// 14. var stuff = (1 + 3 +
    ///                  *******
    /// 15.              13 + "hi" +
    ///                  ************
    /// 16.              19);
    ///                  **
    /// ```
    /// Resulting string consists of multiple lines and has a marker denoted by a sequence of
    /// `*` on every other line. The lines are parsed from `expression` by the `expression.line_start` and
    /// the `expression.line_end` numbers. Lines are fetched from `source` string.
    pub fn get_error_marked_line(error: &EvaluationError, source: &str) -> String {
        let mut result_string = String::new();
        for line_number in error.expression.line_start..=error.expression.line_end {
            let line = source.split("\n").nth(line_number - 1);
            if line.is_none() {
                return format!("[Internal Runtime Error] Couldn't read line {} from source file", line_number);
            }

            // Prep the line itself
            // i.e. `14. var stuff = (1 + 3 +`
            let line = line.unwrap();
            let line_number_string = format!("{}. ", line_number);
            result_string += "\n";
            result_string += &line_number_string;
            result_string += line;

            // On the first line we calculate the spaces as an offset to an expression.
            // Marker is calculated until the end of the line unless this line is also the end
            // of the expression
            if line_number == error.expression.line_start {
                let spaces = " ".repeat(line_number_string.len() + error.expression.start_offset - 1);

                if line_number == error.expression.line_end {
                    let marker = "*".repeat(error.expression.end_offset - error.expression.start_offset);
                    result_string += &format!("\n{spaces}{marker}");
                    return result_string;
                }

                let marker = "*".repeat(line.len() - error.expression.start_offset);
                result_string += &format!("\n{spaces}{marker}");
            }
            // On every other line we skip the whitespace at the beginning
            // and then start drawing the marker. If this is the last line
            // then we draw according to the offset. Otherwise we draw until the end of the line
            else {
                let spaces = " ".repeat(line_number_string.len() +
                    line.chars().take_while(|c| c.is_ascii_whitespace()).count());

                if line_number == error.expression.line_end {
                    let marker = "*".repeat(error.expression.end_offset - spaces.len() + 2);
                    result_string += &format!("\n{spaces}{marker}");
                    return result_string;
                }

                let marker = "*".repeat(line.len() - spaces.len());
                result_string += &format!("\n{spaces}{marker}");
            }
        }


        result_string
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    runtime_error_type: RuntimeErrorType,
    location: Option<String>
}

#[derive(Debug)]
pub enum RuntimeErrorType {
    Evaluation(EvaluationError)
}

impl Display for RuntimeErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeErrorType::Evaluation(ev) => write!(f, "[Evaluation err]\n{ev}")?
        }
        Ok(())
    }
}

impl Error for RuntimeError {}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.runtime_error_type)?;
        if let Some(location) = &self.location {
            write!(f, "\n\nHappened here:\n{}", location)?;
        }

        Ok(())
    }
}