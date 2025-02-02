use std::collections::HashMap;
use lazy_static::lazy_static;

#[derive(Debug, Clone)]
pub enum TokenType {
    Literal(LiteralType),
    // static lifetime, as we will read from KEYWORDS HashMap
    // see that hashmap below in lazy_static! block
    Keyword(&'static KeywordType),
    Punctuation(PunctuationType),

    EOF
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64),
    Boolean(bool),
    Nil
}

#[derive(Debug)]
pub enum KeywordType {
    Constant(ConstantKeywordType),
    Regular(RegularKeywordType)
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, KeywordType> = {
        let mut m = HashMap::new();
        
        m.insert("and",     KeywordType::Regular(RegularKeywordType::And));
        m.insert("class",   KeywordType::Regular(RegularKeywordType::Class));
        m.insert("else",    KeywordType::Regular(RegularKeywordType::Else));
        m.insert("fun",     KeywordType::Regular(RegularKeywordType::Fun));
        m.insert("for",     KeywordType::Regular(RegularKeywordType::For));
        m.insert("if",      KeywordType::Regular(RegularKeywordType::If));
        m.insert("or",      KeywordType::Regular(RegularKeywordType::Or));
        m.insert("print",   KeywordType::Regular(RegularKeywordType::Print));
        m.insert("return",  KeywordType::Regular(RegularKeywordType::Return));
        m.insert("super",   KeywordType::Regular(RegularKeywordType::Super));
        m.insert("this",    KeywordType::Regular(RegularKeywordType::This));
        m.insert("var",     KeywordType::Regular(RegularKeywordType::Var));
        m.insert("while",   KeywordType::Regular(RegularKeywordType::While));
        m.insert("false",   KeywordType::Constant(ConstantKeywordType::False));
        m.insert("nil",     KeywordType::Constant(ConstantKeywordType::Nil));
        m.insert("true",    KeywordType::Constant(ConstantKeywordType::True));
        
        m
    };
}

#[derive(Debug)]
pub enum ConstantKeywordType {
    True, False, Nil
}

#[derive(Debug)]
pub enum RegularKeywordType {
    And, Class, Else, Fun, For, If, Or,
    Print, Return, Super, This, Var, While
}

#[derive(Debug, Clone)]
pub enum PunctuationType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Question, Colon,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
}