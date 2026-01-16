
mod tokens;
use std::{env, fs, process::ExitCode};

use crate::{lexer::{LexError, extract_tokens}, parser::{ParseError, Parser}, tokens::TokenDetail};

mod ast;
mod common;
mod parser;
mod lexer;
mod ir;
mod irgen;

fn main() -> ExitCode {

    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let filepath = &args[1];
        let input = match fs::read_to_string(filepath) {
            Ok(input) => input,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                return (1).into();
            } 
        };
        let tokens = match extract_tokens(input) {
            Ok(tokens) => tokens,
            Err(LexError { loc, message }) => {
                eprintln!("Lex error: {}-{}: {}", loc.start_index, loc.end_index, message);
                return 1.into();
            }
        };
        for token in &tokens {
            println!("{}", token);
        }
        let mut tokens_no_comments = vec![];
        for token in tokens {
            if !matches!(token.t, TokenDetail::Comment(_)) {
                tokens_no_comments.push(token);
            }
        }
        println!();
        let mut parser = Parser::new(&tokens_no_comments);
        let parsed = match parser.parse_program() {
            Ok(node) => node,
            Err(ParseError{loc, message}) => {
                eprintln!("Parse error: {}-{}: {}", loc.start_index, loc.end_index, message);
                return 1.into();
            }
        };
        println!("{}", parsed);


    }

    return 0.into();

    // let tokens: Vec<Token> = vec![
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(5) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Plus) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(6) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(DoubleAmpersand) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(7) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Minus) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Minus) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(8) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Plus) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(54) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Asterix) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Tilde) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: LeftParenthesis },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(2) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Operator(Ampersand) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: Number(6) },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: RightParenthesis },
    //     Token{loc:CodeLocation::default(), text:String::default(), t: EOF }
    // ];

    // let mut parser = Parser::new(&tokens);

    // match parser.parse_expression() {
    //     Err(e) => println!("{:?}, {}", e.loc, e.message),
    //     Ok(node) => println!("{}", node)
    // }




}
