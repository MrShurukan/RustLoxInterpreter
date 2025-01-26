#[derive(Debug)]
pub enum TokenType {
    Literal(LiteralType),
    Keyword(KeywordType),
    Punctuation(PunctuationType),

    EOF
}

#[derive(Debug)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64)
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