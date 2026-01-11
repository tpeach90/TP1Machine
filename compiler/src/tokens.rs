use crate::common::CodeLocation;



#[derive(Clone)]
pub struct Token {
    pub loc: CodeLocation,
    pub t: TokenDetail,
    pub text: String,
}

#[derive(Clone)]
pub enum TokenDetail {
    Comment(String),
    SemiColon,
    Colon,
    Keyword(Keyword),
    Identifier(String),
    Equals,
    LeftBracket,
    RightBracket,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Comma,
    Operator(Operator),
    Number(u8),
    EOF,
}

#[derive(Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Tilde,
    Exclaimation,
    Asterix,
    Arobase,
    Caret,
    ForwardSlash,
    Percent,
    Ampersand,
    Bar,
    Underscore,
    DoubleBar,
    DoubleAmpersand,
    Equality,
    UnsignedLessThan,
    UnsignedGreaterThan,
    UnsignedLessThanOrEqualTo,
    UnsignedGreaterThanOrEqualTo,
    SignedLessThan,
    SignedGreaterThan,
    SignedLessThanOrEqualTo,
    SignedGreaterThanOrEqualTo,
}

#[derive(Clone, Copy)]
pub enum Keyword {
    Main,
    Const,
    Let,
    Byte,
    Void,
    Forever,
    While,
    Do,
    If,
    Else,
    Flag,
}