
mod tokens;
use tokens::{Token, TokenDetail::*, Operator::*};

use common::CodeLocation;

use crate::{parser::Parser, tokens::TokenDetail};

mod ast;
mod common;
mod parser;
mod lexer;


fn main() {

    let tokens: Vec<Token> = vec![
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("5")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Plus) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("6")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(DoubleAmpersand) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("7")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Minus) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Minus) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("8")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Plus) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("54")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Asterix) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Tilde) },
        Token{loc:CodeLocation::default(), text:String::default(), t: LeftParenthesis },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("2")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Ampersand) },
        Token{loc:CodeLocation::default(), text:String::default(), t: Integer(String::from("6")) },
        Token{loc:CodeLocation::default(), text:String::default(), t: RightParenthesis },
        Token{loc:CodeLocation::default(), text:String::default(), t: EOF }
    ];

    let mut parser = Parser::new(&tokens);

    match parser.parse_expression() {
        Err(e) => println!("{:?}, {}", e.loc, e.message),
        Ok(node) => println!("{}", node)
    }




}
