
mod tokens;
use tokens::{Token, TokenDetail::*};

use common::CodeLocation;

mod ast;
mod common;
mod parser;

fn main() {
    println!("Hello, world!");
    let token = Token {loc: CodeLocation::default(), t: Comma};

    match token.t {
        Comma => {
    
        }
        Comment(_) => todo!(),
        SemiColon => todo!(),
        Keyword(_) => todo!(),
        Identifier(_) => todo!(),
        Equals => todo!(),
        LeftBracket => todo!(),
        RightBracket => todo!(),
        LeftParenthesis => todo!(),
        RightParenthesis => todo!(),
        LeftBrace => todo!(),
        RightBrace => todo!(),
        Operator(_) => todo!(),
        Integer(_) => todo!(),
    }
}
