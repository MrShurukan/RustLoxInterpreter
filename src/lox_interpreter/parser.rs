use crate::lox_interpreter::expression::Expression;
use crate::lox_interpreter::token::Token;
use crate::lox_interpreter::token_type::{ConstantKeywordType, KeywordType as KT, LiteralType, PunctuationType as PT, PunctuationType, TokenType as TT, TokenType};
use std::error::Error;
use std::fmt::{format, Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use anyhow::bail;
use unicode_segmentation::UnicodeSegmentation;

pub struct Parser {
    tokens: Vec<Token>,
    /// Reference to an original string to point out errors with a location
    pub source: String
}


macro_rules! parse_binary {
    ($self:ident, $enum_types:pat_param, $expression:ident, $tokens:ident, $next_precedence:ident) => {
        let token = &Self::peek($tokens)?;
        while let TT::Punctuation($enum_types) = token.token_type {
            $tokens.next();
            let right = $self.$next_precedence($tokens)?;

            $expression = Expression::new_binary($expression, token, right);
        }
    };
}

/// This macro is used to create error producing.
///
/// It's used to capture erroneous tokens to alert user of a potential problem.
/// For instance, catching something like `* 3 + 1` and warning that the user didn't
/// provide a value on the left side of the star.
///
/// Otherwise, this error will go all the way to the bottom of precedence and just get
/// reported as "Incorrect token in this place"
macro_rules! binary_error_produce {
    ($self:ident, $error_enum_types:pat_param, $tokens:ident, $next_precedence:ident) => {
        let current_token = &Self::peek($tokens)?;
        if let TT::Punctuation($error_enum_types) = current_token.token_type {
            $tokens.next();
            // Discard the right part
            _ = $self.$next_precedence($tokens)?;

            // Discard the expression entirely. Report an error
            return Err(Parser::get_error_marked_line(ParserErrorType::BinaryOperatorNoLeftPart,
                (*current_token).to_owned(), &$self.source));
        }
    };
}

macro_rules! parse_binary_with_error_produce {
    ($self:ident, $enum_types:pat_param, $tokens:ident, $next_precedence:ident) => {
        {
            binary_error_produce!($self, $enum_types, $tokens, $next_precedence);

            let mut expr = $self.$next_precedence($tokens)?;
            parse_binary!($self, $enum_types, expr, $tokens, $next_precedence);

            expr
        }
    };

    ($self:ident, $enum_types:pat_param, $error_enum_types:pat_param, $tokens:ident, $next_precedence:ident) => {
        {
            binary_error_produce!($self, $error_enum_types, $tokens, $next_precedence);

            let mut expr = $self.$next_precedence($tokens)?;
            parse_binary!($self, $enum_types, expr, $tokens, $next_precedence);

            expr
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

    /// The goal of this method is to align parser with a next statement
    /// after error was found somewhere.
    ///
    /// Say, an incorrect ternary `1 ? : 2` - this won't parse very well.
    /// If we just continue marching forward from encountering that ":" - we will report
    /// a lot of errors down the line that may not be truthful (phantom errors).
    ///
    /// This method attempts to minimize those by aligning with the next statement.
    /// It's not 100% correct, but it's good enough
    fn synchronize(tokens: &mut Peekable<Iter<Token>>) {
        let current_token = tokens.peek();
        // If we ran out of tokens - we're done already
        if current_token.is_none() {
            return;
        }

        let mut old_token_type = current_token.unwrap().token_type.to_owned();
        while let Some(token) = tokens.next() {
            match old_token_type {
                // A semicolon is a very clear statement boundary
                TokenType::Punctuation(PT::Semicolon) => { return; },
                // Continue otherwise
                _ => {}
            }

            match token.token_type {
                // Keyword (not a constant like true or false) is a nice way to know we are aligned
                TokenType::Keyword(KT::Regular(_)) => { return; }
                // Just keep on marching otherwise
                _ => {}
            }

            old_token_type = token.token_type.to_owned();
        }

        tokens.next();
    }

    fn expression<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        self.ternary(tokens)
    }

    // expr ? expr : expr
    fn ternary<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let mut expr = self.comma(tokens)?;

        let question_token = &Self::peek(tokens)?;
        if let TT::Punctuation(PT::Question) = question_token.token_type {
            tokens.next();
            let second = self.comma(tokens)?;

            let third_token = &Self::peek(tokens)?;
            if let TT::Punctuation(PT::Colon) = third_token.token_type {
                tokens.next();
                let third = self.comma(tokens)?;

                expr = Expression::new_ternary(expr, second, third);
            }
            else {
                return Err(
                    Self::get_error_marked_line(ParserErrorType::UnfinishedTernary, (*question_token).clone(), &self.source)
                );
            }
        }


        Ok(expr)
    }

    // expr, expr
    fn comma<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let expr = parse_binary_with_error_produce!(self, PT::Comma, tokens, equality);

        Ok(expr)
    }

    // != ==
    fn equality<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let expr = parse_binary_with_error_produce!(self, (PT::BangEqual | PT::EqualEqual), tokens, comparison);

        Ok(expr)
    }

    // > >= < <=
    fn comparison<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let expr = parse_binary_with_error_produce!(self,
            (PT::Greater | PT::GreaterEqual | PT::Less | PT::LessEqual), tokens, term);

        Ok(expr)
    }

    // + -
    fn term<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        // Only a plus should produce an error, because a minus could still be a unary operator
        let expr = parse_binary_with_error_produce!(self,
            (PT::Plus | PT::Minus), (PT::Plus), tokens, factor);

        Ok(expr)
    }

    // * /
    fn factor<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let expr = parse_binary_with_error_produce!(self, (PT::Star | PT::Slash), tokens, unary);

        Ok(expr)
    }

    // ! - (as unary operator)
    fn unary<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let token = &Self::peek(tokens)?;
        if let TT::Punctuation(PT::Bang | PT::Minus) = &token.token_type {
            tokens.next();
            let right = self.unary(tokens)?;

            return Ok(Expression::new_unary(token, right));
        }

        self.primary(tokens)
    }

    // Literals, groupings
    fn primary<'a>(&self, tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParserError> {
        let token = Self::peek(tokens)?;

        let result = match &token.token_type {
            TT::Literal(literal_type) => { Some(Expression::new_literal(token)) }
            TT::Keyword(KT::Constant(constant)) => {
                match constant {
                    ConstantKeywordType::True => Some(Expression::new_literal_custom_type(token, LiteralType::Boolean(true))),
                    ConstantKeywordType::False => Some(Expression::new_literal_custom_type(token, LiteralType::Boolean(false))),
                    ConstantKeywordType::Nil => Some(Expression::new_literal_custom_type(token, LiteralType::Nil))
                }
            }
            TT::Punctuation(PT::LeftParen) => {
                tokens.next();
                let expr = self.expression(tokens)?;
                let next_token = tokens.peek();
                if next_token.is_none() {
                    return Err(Self::get_error_marked_line(
                        ParserErrorType::UnclosedParenthesis, (*token).clone(), &self.source
                    ));
                }
                else {
                    let next_token = next_token.unwrap();
                    match next_token.token_type {
                        TT::Punctuation(PT::RightParen) => {},

                        _ => {
                            return Err(Self::get_error_marked_line(
                                ParserErrorType::UnclosedParenthesis, (*next_token).clone(), &self.source
                            ));
                        }
                    }
                }

                Some(Expression::new_grouping(expr))
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

    /// Gets a helper string for error displaying with a marker on the second line.
    /// ### Example
    /// This function is used to create a string that looks something like this:
    /// ```
    /// 14. var stuff = (1 + class) * 3
    ///                      ^^^^^
    /// ```
    /// Resulting string consists of two lines and has a marker denoted by a sequence of
    /// `^` on the second line. The line is parsed from `token` by the `token.line_offset` and
    /// the `token.lexeme_size` string. Line is fetched from `source` string.
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
    IncorrectLineNumberProvided(usize),
    UnfinishedTernary,
    BinaryOperatorNoLeftPart,
    NoPlusUnaryOperator
}

impl ParserError {
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
                &*format!("[Internal parser error] Couldn't get {} as a line number (token: {:?})", line, self.token),
            ParserErrorType::UnfinishedTernary => "Incorrect ternary operator",
            ParserErrorType::BinaryOperatorNoLeftPart => "Binary operator without a left part",
            ParserErrorType::NoPlusUnaryOperator => "Unary plus is not supported in Lox"
        };

        write!(f, "{error_header}: {}", output)?;
        if let Some(location) = &self.location {
            write!(f, "\n\nHappened here:\n{}", location)?;
        }

        if let Token { token_type: TokenType::EOF, .. } = self.token {
            write!(f, " (pointing at EOF, something went wrong)")?;
        }

        Ok(())
    }
}