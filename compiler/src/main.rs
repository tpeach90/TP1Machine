
mod tokens;
use std::{env, fs, process::ExitCode};

use argparse::{ArgumentParser, StoreTrue, Store, StoreOption};

use crate::{irgen::{SemanticError, gen_program}, lexer::{LexError, extract_tokens}, parser::{ParseError, Parser}, tokens::TokenDetail};

mod ast;
mod common;
mod parser;
mod lexer;
mod ir;
mod irgen;

fn main() -> ExitCode {

    let mut print_tokens = false;
    let mut print_ast = false;
    let mut ir_graph_path: Option<String> = None;
    let mut input_path = String::new();

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("tp1c compiler for the TP1 machine.");
        ap.refer(&mut print_tokens)
            .add_option(&["-t", "--print-tokens"], StoreTrue, "Prints the list of tokens after lexing");
        ap.refer(&mut print_ast)
            .add_option(&["-a", "--print-ast"], StoreTrue, "Print the abstract syntax tree after parsing");
        ap.refer(&mut ir_graph_path)
            .add_option(&["-g", "--output-cfg"], StoreOption, "Path at which to generate a context flow graph in Graphviz DOT language after intermediate code generation");
        ap.refer(&mut input_path)
            .add_argument("input file", Store, "Path to tp1c text file to compile");
        ap.parse_args_or_exit();
    }

    let input = match fs::read_to_string(input_path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return (1).into();
        } 
    };

    // lex
    let tokens = match extract_tokens(input) {
        Ok(tokens) => tokens,
        Err(LexError { loc, message }) => {
            eprintln!("Lex error: {}-{}: {}", loc.start_index, loc.end_index, message);
            return 1.into();
        }
    };
    if print_tokens {
        for token in &tokens {
            println!("{}", token);
        }
        println!();
    }
    let mut tokens_no_comments = vec![];
    for token in tokens {
        if !matches!(token.t, TokenDetail::Comment(_)) {
            tokens_no_comments.push(token);
        }
    }
    
    // parse
    let mut parser = Parser::new(&tokens_no_comments);
    let parsed = match parser.parse_program() {
        Ok(node) => node,
        Err(ParseError{loc, message}) => {
            eprintln!("Parse error: {}-{}: {}", loc.start_index, loc.end_index, message);
            return 1.into();
        }
    };
    if print_ast {
        println!("{}", parsed);
        println!();
    }

    // irgen
    let ir = match gen_program(&parsed) {
        Ok(ir) => ir,
        Err(SemanticError{loc, message}) => {
            eprintln!("Semantic error: {}-{}: {}", loc.start_index, loc.end_index, message);
            return 1.into();
        }
    };
    match ir_graph_path {
        None => (),
        Some(path) => {
            let dot_code = ir.to_dot_code();
            match fs::write(path, dot_code) {
                Ok(_) => (),
                Err(_) => {
                    eprintln!("Error writing context flow graph");
                    return 1.into();
                },
            }
        },
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
