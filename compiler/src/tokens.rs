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
    // Arobase,
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
    Break,
    Continue
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match &self.t {
            TokenDetail::Comment(text) => format!("Comment: {}", text),
            TokenDetail::SemiColon => format!("Semicolon"),
            TokenDetail::Colon => format!("Colon"),
            TokenDetail::Keyword(keyword) => format!("Keyword: {}", match keyword {
                Keyword::Main => "Main",
                Keyword::Const => "Const",
                Keyword::Let => "Let",
                Keyword::Byte => "Byte",
                Keyword::Void => "Void",
                Keyword::Forever => "Forever",
                Keyword::While => "While",
                Keyword::Do => "Do",
                Keyword::If => "If",
                Keyword::Else => "Else",
                Keyword::Flag => "Flag",
                Keyword::Break => "Break",
                Keyword::Continue => "Continue"
            }),
            TokenDetail::Identifier(ident) => format!("Identifier: {}", ident),
            TokenDetail::Equals => format!("Equals"),
            TokenDetail::LeftBracket => format!("LeftBracket"),
            TokenDetail::RightBracket => format!("RightBracket"),
            TokenDetail::LeftParenthesis => format!("LeftParenthesis"),
            TokenDetail::RightParenthesis => format!("RightParenthesis"),
            TokenDetail::LeftBrace => format!("LeftBrace"),
            TokenDetail::RightBrace => format!("RightBrace"),
            TokenDetail::Comma => format!("Comma"),
            TokenDetail::Operator(operator) => format!("Operator: {}", match operator {
                Operator::Plus => "Plus",
                Operator::Minus => "Minus",
                Operator::Tilde => "Tilde",
                Operator::Exclaimation => "Exclaimation",
                Operator::Asterix => "Asterix",
                // Operator::Arobase => "Arobase",
                Operator::Caret => "Caret",
                Operator::ForwardSlash => "ForwardSlash",
                Operator::Percent => "Percent",
                Operator::Ampersand => "Ampersand",
                Operator::Bar => "Bar",
                Operator::Underscore => "Underscore",
                Operator::DoubleBar => "DoubleBar",
                Operator::DoubleAmpersand => "DoubleAmpersand",
                Operator::Equality => "Equality",
                Operator::UnsignedLessThan => "UnsignedLessThan",
                Operator::UnsignedGreaterThan => "UnsignedGreaterThan",
                Operator::UnsignedLessThanOrEqualTo => "UnsignedLessThanOrEqualTo",
                Operator::UnsignedGreaterThanOrEqualTo => "UnsignedGreaterThanOrEqualTo",
                Operator::SignedLessThan => "SignedLessThan",
                Operator::SignedGreaterThan => "SignedGreaterThan",
                Operator::SignedLessThanOrEqualTo => "SignedLessThanOrEqualTo",
                Operator::SignedGreaterThanOrEqualTo => "SignedGreaterThanOrEqualTo",
            }),
            TokenDetail::Number(number) => format!("Number: {}", number),
            TokenDetail::EOF => format!("EOF"),
        };
        write!(f, "{}", str)

    }
}