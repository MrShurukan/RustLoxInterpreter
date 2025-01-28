use std::collections::HashMap;
use lazy_static::lazy_static;

#[derive(Debug)]
pub enum TokenType {
    Literal(LiteralType),
    // static lifetime, as we willread from KEYWORDS HashMap
    // see that hashmap below in lazy_static! block
    Keyword(&'static KeywordType),
    Punctuation(PunctuationType),

    EOF
}

#[derive(Debug)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64)
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, KeywordType> = {
        let mut m = HashMap::new();
        
        m.insert("and",     KeywordType::And);
        m.insert("class",   KeywordType::Class);
        m.insert("else",    KeywordType::Else);
        m.insert("false",   KeywordType::False);
        m.insert("fun",     KeywordType::Fun);
        m.insert("for",     KeywordType::For);
        m.insert("if",      KeywordType::If);
        m.insert("nil",     KeywordType::Nil);
        m.insert("or",      KeywordType::Or);
        m.insert("print",   KeywordType::Print);
        m.insert("return",  KeywordType::Return);
        m.insert("super",   KeywordType::Super);
        m.insert("this",    KeywordType::This);
        m.insert("true",    KeywordType::True);
        m.insert("var",     KeywordType::Var);
        m.insert("while",   KeywordType::While);
        
        m
    };
}

#[derive(Debug)]
pub enum KeywordType {
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
}

#[derive(Debug)]
pub enum PunctuationType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
}