use std::collections::HashMap;
use std::{process, usize, vec};
use std::io::Read;

fn main() {
    // read from stdin until EOF
    let mut input =  Vec::new();
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    match handle.read_to_end(&mut input) {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        },
        _ => ()
    }
    let input = match String::from_utf8(input) {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        },
        Ok(string) => string
    };

    let (tokens, tokenize_error) = tokenize(&input);
    let (instructions, parse_errors) = parse(&tokens);
    let bin_result = binary_gen(&instructions);
    

    if tokenize_error.is_some() {
        print_input_error(&tokenize_error.unwrap());
    }
    for parse_error in &parse_errors {
        print_input_error(parse_error);
    }

    match bin_result {
        Err(e) => eprintln!("Error: {}", e),
        Ok((out, size , labels)) => {
            let mut label_tuple_vec = labels.iter()
                .collect::<Vec<(&String, &u8)>>();

            label_tuple_vec.sort_by_key(|(_, value)| *value );

            for (label, value) in label_tuple_vec {
                println!("Label: {}: {}", label, value);
            }
            println!("Result: {:?}", &out[..size]);
            println!("Size: {} bytes", size);
        }
    }




}

fn print_input_error(err: &InputError) {
    eprintln!("Error: line {}: {}", err.line, err.detail);
}

#[derive(Debug, Clone)]
enum Token {
    NewLine,
    Register(String),
    LeftBracket,
    RightBracket,
    Equals,
    Minus,
    Number(String),
    Command(String),
    Label(String),
    Colon,
    Operator(String)
}

#[derive(Debug, Clone, PartialEq)]
enum Location {
    NextRomByte(Constant),
    PC,
    D0,
    D1,
    IN,
    D0Rom,
}

#[derive(Debug, Clone, PartialEq)]
enum Constant {
    Literal(u8),
    Label(String)
}

#[derive(Debug, Clone, Copy)]
enum ALUMode {
    Decrement = 0,
    Increment = 1,
    LeftShift = 2,
    RightShift = 3,
    BitwiseNOT = 4,
    TwosComplement = 5,
    Log2 = 6,
    Compare = 7,
    Add = 8,
    Subtract = 9,
    Multiply = 10,
    BitwiseXOR = 11,
    Divide = 12,
    Modulo = 13,
    BitwiseAND = 14,
    BitwiseOR = 15,
}

#[derive(Debug, Clone, Copy)]
enum BranchMode {
    BZ = 0,
    BNZ = 1,
    BC = 2,
    BNC = 3,
    BN = 4,
    BNN = 5,
    BO = 6,
    BNO = 7,
}

#[derive(Debug, Clone)]
struct Instruction {
    labels: Vec<String>,
    detail:InstructionDetail,
}

#[derive(Debug, Clone)]
enum InstructionDetail {
    Move{source:Location, deref_source: bool, destination: Location, deref_destination:bool},
    ALU{target: Location, discard: bool, mode: ALUMode},
    Branch{target: Location, mode:BranchMode},
    Wait,
    UpdateScreen,
    Stop,
    Constant{data: Vec<Constant>}
}



fn tokenize(input: &str) -> (Vec<Token>, Option<InputError>) {

    let mut tokens: Vec<Token> = vec![];

    let mut line = 1;
    let mut iter = input.chars().peekable();
    
    loop {
        let next_ch = iter.peek();
        match next_ch {
            None => return (tokens, None),
            Some(ch) => {
                match ch {
                    ' ' | '\t' | '\r' => {
                        iter.next();
                    },
                    '\n' => {
                        tokens.push(Token::NewLine);
                        line += 1;
                        iter.next();
                    },
                    ';' => {
                        iter.next();
                        while iter.peek().is_some_and(|x| x != &'\n') {
                            iter.next();
                        }
                    },
                    '[' => {
                        tokens.push(Token::LeftBracket);
                        iter.next();
                    },
                    ']' => {
                        tokens.push(Token::RightBracket);
                        iter.next();
                    }
                    '=' => {
                        tokens.push(Token::Equals);
                        iter.next();
                    },
                    '-' => {
                        tokens.push(Token::Minus);
                        iter.next();
                    }
                    '+' | '<' | '>' | '~' | '*' | '^' | '/' | '%' | '&' | '|' | '_' => {
                        tokens.push(Token::Operator(ch.to_string()));
                        iter.next();
                    }
                    ':' => {
                        tokens.push(Token::Colon);
                        iter.next();
                    }
                    'a'..='z' => {
                        let mut id = ch.to_string();
                        iter.next();
                        let mut done = false;
                        while !done {
                            match iter.peek() {
                                Some(ch) => {
                                    match ch {
                                        'a'..='z' | 'A'..='Z' => {
                                            id = id + &ch.to_string();
                                            iter.next();
                                        }
                                        _ => done = true
                                    }
                                }
                                None => done = true,
                            }
                        }
                        tokens.push(Token::Label(id));

                    }
                    '0'..='9' => {
                        let mut number= ch.to_string();
                        iter.next();
                        while iter.peek().is_some_and(|x| match x {'0'..='9' => true, _ => false}) {
                            number = number + &iter.peek().unwrap().to_string();
                            iter.next();
                        }
                        tokens.push(Token::Number(number));

                    }
                    '{' => {
                        let patterns = vec!["{D0}"];
                        // keep reading chars until }
                        iter.next();
                        let mut register = String::from('{');
                        while iter.peek().is_some_and(|x| x != &'}') {
                            register = register + &iter.peek().unwrap().to_string();
                            iter.next();
                        }
                        if iter.peek().is_some() /*'}'*/ {
                            register = register + "}";
                            iter.next();
                        } else {
                            // error - no matching }
                            return (tokens, Some(InputError { detail: "No matching }".to_string(), line: line }));
                        }
                        // check pattern is valid
                        if patterns.contains(&register.as_str()) {
                            tokens.push(Token::Register(register));
                        } else {
                            // error
                            return (tokens, Some(InputError {detail: String::from("Invalid register: ") + &register, line: line}));
                        }
                    }
                    'A'..='Z' => {
                        // could be Register or Command
                        // work out after parsing
                        let mut phrase = ch.to_string();
                        iter.next();
                        while iter.peek().is_some_and(|x| match x {'A'..='Z'|'0'..='9' => true, _ => false}) {
                            phrase = phrase + &iter.peek().unwrap().to_string();
                            iter.next();
                        }
                        // check if register
                        let registers = vec!["PC", "D0", "D1", "IN"];
                        if registers.contains(&phrase.as_str()) {
                            tokens.push(Token::Register(phrase));
                        } else {
                            tokens.push(Token::Command(phrase));
                        }
                    }
                    _ => {
                        return (tokens, Some(InputError { detail: "Unexpected symbol: ".to_string() + &ch.to_string(), line: line }))
                    }

                }
            }
            
        }
    }



}

#[derive(Debug)]
struct InputError {
    detail: String,
    line: usize,
}


fn parse(tokens: &Vec<Token>) -> (Vec<Instruction>, Vec<InputError>) {

    let mut errors: Vec<InputError> = vec![];

    let mut instructions: Vec<Instruction> = vec![];
    let mut i = 0;
    let mut line = 1;
    
    // instructions have this form:
    // ((Label Colon) | Newline)* InstructionDetail Newline 
    // collect labels

    let mut current_instruction_labels: Vec<String> = vec![];

    while i < tokens.len() {
        match &tokens[i..] {
            [Token::NewLine, ..] => {
                i += 1; 
                line += 1
            }
            [Token::Label(x), Token::Colon, ..] => {
                current_instruction_labels.push(x.to_string());
                i += 2;
            }
            _ => {
                let (result, new_i) = parse_instruction_detail(&tokens, i);
                match result {
                    Ok(detail) => {
                        instructions.push(Instruction { labels: current_instruction_labels, detail });
                    }
                    Err(err) => {
                        errors.push(InputError { detail: err, line: line });
                    }
                }
                current_instruction_labels = vec![];
                i = new_i;
            }
        };
    }

    return (instructions, errors);
}

fn parse_instruction_detail(tokens: &Vec<Token>, offset: usize) -> (Result<InstructionDetail, String>, usize) {

    // collect tokens up to next NewLine
    let mut instruction_tokens: Vec<Token> = vec![];
    let mut i = offset;
    while i < tokens.len() && !matches!(&tokens[i], Token::NewLine) {
        instruction_tokens.push(tokens[i].clone());
        i += 1;
    }

    if matches!(instruction_tokens[0], Token::Command(_)) {
        match parse_command(&instruction_tokens) {
            Ok(detail) => (Ok(detail), i),
            Err(err) => (Err(err), i)
        }
    } else {
        match parse_alu_or_move_instruction_detail(&instruction_tokens) {
            Ok(detail) => (Ok(detail), i),
            Err(err) => (Err(err), i),
        }
    }

}

fn parse_command(instruction_tokens: &Vec<Token>) -> Result<InstructionDetail, String> {
    
    if instruction_tokens.len() == 0 {
        return Err("Expected a command".to_string());
    }
    
    match &instruction_tokens[0] {
        Token::Command(command_word) => {
            let branch_mode = match command_word.as_str() {
                "BZ" => Some(BranchMode::BZ),
                "BNZ" => Some(BranchMode::BNZ),
                "BC" => Some(BranchMode::BC),
                "BNC" => Some(BranchMode::BNC),
                "BN" => Some(BranchMode::BN),
                "BNN" => Some(BranchMode::BNN),
                "BO" => Some(BranchMode::BO),
                "BNO" => Some(BranchMode::BNO),
                _ => None
            };
            if branch_mode.is_some() {
                // expect a register or constant next.
                if instruction_tokens.len() < 2 {
                    return Err("Branch instruction with no target".to_string())
                }
                let target = match &instruction_tokens[1] {
                    Token::Register(register) => {
                        match register.as_str() {
                            "D0" => {
                                if instruction_tokens.len() != 1 {
                                    Err("Extra token at end of branch instruction".to_string())
                                } else {
                                    Ok(Location::D0)
                                }
                            }
                            _ => Err("Branch target can only be D0 or constant".to_string())
                        }
                    }
                    Token::Minus | 
                    Token::Number(_) | 
                    Token::Label(_) => {
                        let (constant, new_i) = parse_constant(instruction_tokens, 1)?;

                        // check this is the end of the instruction
                        if new_i < instruction_tokens.len() {
                            Err("Extra token at end of branch instruction".to_string())
                        } else {
                            Ok(Location::NextRomByte(constant))
                        }
   
                    }
                    _ => Err("Branch target can only be D0 or constant".to_string())
                }?;

                return Ok(InstructionDetail::Branch { target, mode: branch_mode.unwrap() });


            }

            // the rest ...
            return match command_word.as_str() {
                "CONST" => {
                    // keep parsing constants until end of instruction
                    let mut data: Vec<Constant> = vec![];
                    let mut i = 1;
                    while i != instruction_tokens.len() {
                        let (constant, new_i) = parse_constant(instruction_tokens, i)?;
                        data.push(constant);
                        i = new_i;
                    }
                    Ok(InstructionDetail::Constant { data })
                },
                "UPDATESCREEN" => {
                    if instruction_tokens.len() != 1 {
                        return Err("UPDATESCREEN should take no arguments".to_string())
                    }
                    Ok(InstructionDetail::UpdateScreen)
                },
                "STOP" => {
                    if instruction_tokens.len() != 1 {
                        return Err("STOP should take no arguments".to_string())
                    }
                    Ok(InstructionDetail::Stop)
                },
                "WAIT" => {
                    if instruction_tokens.len() != 1 {
                        return Err("WAIT should take no arguments".to_string())
                    }
                    Ok(InstructionDetail::Wait)  
                }
                _ => Err("Unknown command".to_string())
            }
        }
        _ => Err("Expected a command".to_string())

    }
}


fn parse_constant(tokens: &Vec<Token>, offset: usize) -> Result<(Constant, usize), String> {
    if offset == tokens.len() {
        return Err("Expected a constant".to_string())
    }

    match &tokens[offset] {
        Token::Number(number_str) => {
            match number_str.parse::<u8>() {
                Ok(number) => Ok((Constant::Literal(number), offset+1)),
                Err(_) => Err("Could not parse number. Allowed range: [-128, 255]".to_string())
            }
        }
        Token::Minus => {
            // expect next token to be a number
            if offset + 1 >= tokens.len() {
                return Err("Expected a Number".to_string())
            }
            match &tokens[offset+1] {
                Token::Number(number_str) => {
                    match (String::from("-") + &number_str).parse::<i8>() {
                        Ok(number) => Ok((Constant::Literal(number as u8), offset+2)),
                        Err(_) => Err("Could not parse number. Allowed range: [-128, 255]".to_string())
                    }
                }
                _ => Err("Expected a Number".to_string())
            }
        }
        Token::Label(label) => {
            Ok((Constant::Label(label.to_owned()), offset+1))
        }
        _ => Err("Expected a constant".to_string())
    }
}


fn parse_alu_or_move_instruction_detail(instruction_tokens: &Vec<Token>) -> Result<InstructionDetail, String> {

    if instruction_tokens.len() == 0 {
        return Err("Expected an ALU or Move instruction".to_string());
    }

    match instruction_tokens[0] {
        // Unexpected
        Token::Colon | 
        Token::NewLine | 
        Token::RightBracket | 
        Token::Command(_) | 
        Token::Label(_) | 
        Token::Number(_) => 
            Err("Unexpected token at start of instruction".to_string()),
        // Definitely ALU
        Token::Equals |
        Token::Operator(_) |
        Token::Minus => 
            parse_alu_instruction_detail(&instruction_tokens),
        // Definitely Move
        Token::LeftBracket =>
            parse_move_instruction_detail(&instruction_tokens),
        // Could be either
        Token::Register(_) => {
            // 1. no equals -> ALU
            // 2. contains a (non minus) operator -> ALU
            // 3. contains register followed by minus -> ALU
            // 4. contains minus followed by a register -> ALU
            // else MOVE

            // 1. and 2.
            if 
                !instruction_tokens.iter().any(|t| match t {Token::Equals => true, _ => false}) 
                || instruction_tokens.iter().any(|t| match t {Token::Operator(_) => true, _ => false})
            {
                // if instruction does not contain an equals then it is an ALU instruction
                return parse_alu_instruction_detail(&instruction_tokens)
            } 
            // 3. and 4.
            let minus_loc = instruction_tokens.iter().position(|x|matches!(x, Token::Minus));
            let is_alu = match minus_loc {
                None => false,
                Some(i) => 
                    // check for token before and after the minus
                    i > 0 && matches!(instruction_tokens[i-1], Token::Register(_))
                    || i < instruction_tokens.len()-1 && matches!(instruction_tokens[i+1], Token::Register(_))
            };
            if is_alu {
                return parse_alu_instruction_detail(&instruction_tokens)
            }
            return parse_move_instruction_detail(&instruction_tokens)
        }
    }
}

fn parse_alu_instruction_detail(instruction_tokens: &Vec<Token>) -> Result<InstructionDetail, String> {

    if instruction_tokens.len() == 0 {
        return Err("Expected an ALU instruction".to_string());
    }

    let lhs_error_message_const = "LHS of ALU instruction must be blank or match first operand";
    // check if contains equals, then parse RHS, then check LHS if it exists
    match instruction_tokens.iter().position(|t| matches!(t, Token::Equals)) {
        None => {
            let (target, alu_mode) = parse_alu_instruction_detail_rhs(&instruction_tokens, 0)?;
            Ok(InstructionDetail::ALU { target: target, discard: true, mode: alu_mode })
        }
        Some(i) => {
            if i + 1 < instruction_tokens.len() {
                let (target, alu_mode) = parse_alu_instruction_detail_rhs(&instruction_tokens, i+1)?;
                if i == 1  {
                    // if a LHS is provided it must match the target from the RHS
                    let lhs_matches = match &instruction_tokens[0] {
                        Token::Register(register) => {
                            match register.as_str() {
                                "D0" => Ok(target == Location::D0),
                                "D1" => Ok(target == Location::D1),
                                _ => Err(lhs_error_message_const.to_string())
                            }
                        },
                        _ => Err(lhs_error_message_const.to_string())
                    }?;
                    if !lhs_matches {
                        return Err(lhs_error_message_const.to_string())
                    }
                }
                if i > 1 {
                    return Err(lhs_error_message_const.to_string())
                }
                Ok(InstructionDetail::ALU { target, discard: false, mode: alu_mode })
            } else {
                Err("No RHS of ALU instruction".to_string())
            }
        }
    }

}

fn parse_alu_instruction_detail_rhs(instruction_tokens: &Vec<Token>, offset: usize) -> Result<(Location, ALUMode), String> {

    if offset == instruction_tokens.len() {
        return Err("Expected right hand side of ALU instruction".to_string())
    }

    match &instruction_tokens[offset..] {
        [Token::Operator(op), Token::Register(reg)] => {
            let loc = match reg.as_str() {
                "D0" => Ok(Location::D0),
                "D1" => Ok(Location::D1),
                _ => Err("Only D0 and D1 registers supported in ALU instruction")
            }?;
            let alu_mode = match op.as_str() {
                "<" => Ok(ALUMode::LeftShift),
                ">" => Ok(ALUMode::RightShift),
                "~" => Ok(ALUMode::BitwiseNOT),
                "_" => Ok(ALUMode::Log2),
                _ => Err(format!("Unexpected unary operator: {}", op.to_owned()))
            }?;
            Ok ((loc, alu_mode))
        }
        [Token::Minus, Token::Register(reg)] => {
            let loc = match reg.as_str() {
                "D0" => Ok(Location::D0),
                "D1" => Ok(Location::D1),
                _ => Err("Only D0 and D1 registers supported in ALU instruction")
            }?;
            Ok((loc, ALUMode::TwosComplement))
        }
        [Token::Register(reg), tail @ ..] => {
            let (loc, expect_reg_b) = match reg.as_str() {
                "D0" => Ok((Location::D0, "D1")),
                "D1" => Ok((Location::D1, "D0")),
                _ => Err("Only D0 and D1 registers supported in ALU instruction")
            }?;
            match tail {
                [] => {
                    Ok((loc, ALUMode::Compare))
                }
                [Token::Operator(op), Token::Register(reg_b)] => {
                    if reg_b == expect_reg_b {
                        let alu_mode = match op.as_str() {
                            "+" => Ok(ALUMode::Add),
                            "*" => Ok(ALUMode::Multiply),
                            "^" => Ok(ALUMode::BitwiseXOR),
                            "/" => Ok(ALUMode::Divide),
                            "%" => Ok(ALUMode::Modulo),
                            "&" => Ok(ALUMode::BitwiseAND),
                            "|" => Ok(ALUMode::BitwiseOR),
                            _ => Err(format!("Unexpected binary operator: {}", op)),
                        }?;
                        Ok((loc, alu_mode))
                    } else {
                        Err(format!("Expected register {}", expect_reg_b))
                    }
                }
                [Token::Minus, Token::Register(reg_b)] => {
                    if reg_b == expect_reg_b {
                        Ok((loc, ALUMode::Subtract))
                    } else {
                        Err(format!("Expected register {}", expect_reg_b))
                    }
                }
                [Token::Operator(op), Token::Number(number)] => {
                    if op != "+" {
                        Err(format!("Applying positive constant with {} not supported", op))
                    } else if !number.parse::<usize>().is_ok_and(|num| num == 1) {
                        Err("Can only add constant '1'".to_string())
                    } else {
                        Ok((loc, ALUMode::Increment))
                    }
                }
                [Token::Minus, Token::Number(number)] => {
                    if !number.parse::<usize>().is_ok_and(|num| num == 1) {
                        Err("Can only subtract constant '1'".to_string())
                    } else {
                        Ok((loc, ALUMode::Decrement))
                    }
                }
                [Token::Operator(op), Token::Minus, Token::Number(number)] => {
                    if op != "*" {
                        Err(format!("Applying negative constant with {} not supported", op.as_str()))
                    } else if !number.parse::<usize>().is_ok_and(|num| num == 1) {
                        Err("Can only multiply by constant '-1'".to_string())
                    } else {
                        Ok((loc, ALUMode::TwosComplement))
                    }
                }
                _ => Err("Malformed ALU instruction".to_string())
            }
        }
        _ => Err("Malformed ALU instruction".to_string())
    }

}

fn parse_move_instruction_detail(instruction_tokens: &Vec<Token>) -> Result<InstructionDetail, String> {

    if instruction_tokens.len() == 0 {
        return Err("Expected a move instruction".to_string());
    }

    let (loc_left, deref_left, equals_offset) = parse_maybe_dereference(instruction_tokens, 0)?;

    if equals_offset >= instruction_tokens.len() || !matches!(instruction_tokens[equals_offset], Token::Equals) {
        return Err("Expected an equals".to_string());
    }

    let (loc_right, deref_right, end_offset) = parse_maybe_dereference(instruction_tokens, equals_offset + 1)?;

    if end_offset < instruction_tokens.len() {
        return Err("Unexpected token(s) at end of move instruction".to_string());
    }

    // check that instruction is implemented
    if deref_left && deref_right {
        return Err("Only 1 dereference allowed per move instruction".to_string());
    }
    if matches!(loc_left, Location::D0Rom | Location::IN) {
        return Err("This destination location (left-hand side) is not allowed".to_string());
    }
    if !deref_left && matches!(loc_left, Location::NextRomByte(_)) {
        return Err("Cannot write to ROM".to_string());
    }
    if deref_right && matches!(loc_right, Location::D0Rom | Location::IN) {
        return Err("This source location (right-hand side) does not support dereferencing".to_string())
    }
    if matches!(loc_left, Location::NextRomByte(_)) && matches!(loc_right, Location::NextRomByte(_)) {
        return Err("Only 1 constant allowed per move instruction".to_string())
    }
    if matches!(loc_right, Location::IN) && (deref_left || !matches!(loc_left, Location::D0 | Location::D1)) {
        return Err("IN can only be copied into D0 or D1".to_string());
    }
    if loc_left == loc_right && deref_left == deref_right {
        return Err("Instruction has no effect".to_string())
    }

    return Ok(InstructionDetail::Move { source: loc_right, deref_source: deref_right, destination: loc_left, deref_destination: deref_left })

    
}

fn parse_location(tokens: &Vec<Token>, offset: usize) -> Result<(Location, usize), String> {
    if offset >= tokens.len() {
        return Err("Expected a location".to_string());
    }

    match &tokens[offset] {
        Token::Minus | Token::Number(_) | Token::Label(_) => {
            let (constant, new_offset) = parse_constant(tokens, offset)?;
            Ok((Location::NextRomByte(constant), new_offset))
        }
        Token::Register(reg) => {
            match reg.as_str() {
                "D0" => Ok((Location::D0, offset+1)),
                "D1" => Ok((Location::D1, offset+1)),
                "IN" => Ok((Location::IN, offset+1)),
                "PC" => Ok((Location::PC, offset+1)),
                "{D0}" => Ok((Location::D0Rom, offset+1)),
                _ => Err("Invalid register".to_string()),
            }
        }
        _ => Err("Could not parse location".to_string())
    }
}

// (location, dereference?, new_offset)
fn parse_maybe_dereference(tokens: &Vec<Token>, offset:usize) -> Result<(Location, bool, usize), String> {

    if offset >= tokens.len() {
        return Err("Expected a location or dereference".to_string())
    }

    match &tokens[offset] {
        Token::Minus | Token::Number(_) | Token::Label(_) | Token::Register(_) => {
            let (location, new_offset) = parse_location(tokens, offset)?;
            Ok((location, false, new_offset))
        }
        Token::LeftBracket => {
            // expect a location followed by RightBracket
            let (location, right_bracket_offset) = parse_location(tokens, offset+1)?;
            if right_bracket_offset < tokens.len() && matches!(tokens[right_bracket_offset], Token::RightBracket) {
                Ok((location, true, right_bracket_offset + 1))
            } else {
                Err("Expected a right bracket".to_string())
            }
        }
        _ => Err("Expected a location or dereference".to_string())

    }
}

// adds, but gives error if result greater than ROM size
fn advance_rom(a: usize, b:usize) -> Result<usize, String> {
    let sum = a.wrapping_add(b);
    if sum > 255 {
        Err("Out of ROM".to_string())
    } else {
        Ok(sum)
    }
}

// (data, num. bytes used, labels)
fn binary_gen(instructions: &Vec<Instruction>) -> Result<([u8; 256], usize, HashMap<String, u8>), String> {

    // generate instruction binary first, then backfill labels

    let mut label_values: HashMap<String, u8> = HashMap::new();
    let mut label_references: Vec<(u8, String)> = vec![];

    let mut bin: [u8;256] = [0;256];

    // inc just before writing a new byte
    // inc with wrapping add
    let mut ptr: usize = usize::MAX; 

    for instruction in instructions {
        for label in instruction.labels.iter() {
            label_values.insert(label.to_owned(), u8::try_from(ptr.wrapping_add(1)).unwrap());
        }
        match &instruction.detail {
            InstructionDetail::Stop => {
                ptr = advance_rom(ptr, 1)?;
                bin[ptr] = 0b00000000;
            }
            InstructionDetail::Wait => {
                ptr = advance_rom(ptr, 1)?;
                bin[ptr] = 0b11000000;
            }
            InstructionDetail::UpdateScreen => {
                ptr = advance_rom(ptr, 1)?;
                bin[ptr] = 0b11100000;

            }
            InstructionDetail::Constant { data } => {
                for constant in data {
                    ptr = advance_rom(ptr, 1)?;
                    put_bin_constant(constant, &mut bin, &mut label_references, ptr);
                }
            }
            InstructionDetail::Branch { target, mode } => {
                match target {
                    Location::NextRomByte(constant) => {
                        ptr = advance_rom(ptr, 1)?;
                        bin[ptr] = 0b10000000 + *mode as u8;
                        ptr = advance_rom(ptr, 1)?;
                        put_bin_constant(constant, &mut bin, &mut label_references, ptr);
                    }
                    Location::D0 => {
                        ptr = advance_rom(ptr, 1)?;
                        bin[ptr] = 0b10010000 + *mode as u8;
                    }
                    _ => {return Err("Invalid branch target".to_string())}
                };
            }
            InstructionDetail::ALU { target, discard, mode } => {
                let target_bits = match target {
                    Location::D0 => Ok(0b00000000),
                    Location::D1 => Ok(0b00100000),
                    _ => Err("Invalid ALU target")
                }?;
                let discard_bits = *discard as i32 * 0b00010000;
                ptr = advance_rom(ptr, 1)?;
                bin[ptr] = (0b01000000 + target_bits + discard_bits + *mode as i32) as u8;
            }
            InstructionDetail::Move { source, deref_source, destination, deref_destination } => {
                if matches!(source, Location::IN) && !deref_source && !deref_destination {
                    // _ = IN
                    match destination {
                        Location::D0 => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = 0b11000100;
                        }
                        Location::D1 => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = 0b11000110;
                        }
                        _ => {return Err("Copy IN to invalid register".to_string())}
                    }
                } else if matches!(source, Location::D0Rom) && !deref_source {
                    // _ = {D0}
                    let deref_destination_bits = *deref_destination as i32 * 0b00001000;
                    match destination {
                        Location::NextRomByte(constant) => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = (0b00000000 + deref_destination_bits) as u8;
                            ptr = advance_rom(ptr, 1)?;
                            put_bin_constant(constant, &mut bin, &mut label_references, ptr);
                        }
                        Location::PC => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = (0b00010000 + deref_destination_bits) as u8;
                        }
                        Location::D0 => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = (0b00100000 + deref_destination_bits) as u8;
                        }
                        Location::D1 => {
                            ptr = advance_rom(ptr, 1)?;
                            bin[ptr] = (0b00110000 + deref_destination_bits) as u8;
                        }
                        _ => {return Err("Invalid destination".to_string())}
                    }
                } else {
                    // all other moves
                    let mut next_rom_byte: Option<&Constant> = None;
                    let source_bits = match source {
                        Location::NextRomByte(constant) => {
                            next_rom_byte = Some(constant);
                            Ok(0b00000000)
                        }
                        Location::PC => Ok(0b00010000),
                        Location::D0 => Ok(0b00100000),
                        Location::D1 => Ok(0b00110000),
                        _ => Err("Invalid source")
                    }?;
                    let deref_source_bits = *deref_source as i32 * 0b00001000;
                    let destination_bits = match destination {
                        Location::NextRomByte(constant) => {
                            if next_rom_byte.is_some() {
                                Err("Two constants in move instruction")
                            } else {
                                next_rom_byte = Some(constant);
                                Ok(0b00000000)
                            }
                        }
                        Location::PC => Ok(0b00000010),
                        Location::D0 => Ok(0b00000100),
                        Location::D1 => Ok(0b00000110),
                        _ => Err("Invalid destination")
                    }?;
                    let deref_destination_bits = *deref_destination as i32 * 0b00000001;

                    ptr = advance_rom(ptr, 1)?;
                    bin[ptr] = (0b00000000 + source_bits + deref_source_bits + destination_bits + deref_destination_bits) as u8;
                    match next_rom_byte {
                        Some(constant) => {
                            ptr = advance_rom(ptr, 1)?;
                            put_bin_constant(constant, &mut bin, &mut label_references, ptr);
                        }
                        None => ()
                    }

                }

            }
        };
    };

    // fill in labels
    let mut ram_ptr: usize = 0;
    for (label_ptr, label) in label_references {
        match label_values.get(&label) {
            Some(value) => {
                bin[label_ptr as usize] = *value;
            },
            None => {
                // label does not exist. assume user wants to create a new pointer to RAM
                bin[label_ptr as usize] = u8::try_from(ram_ptr).unwrap();
                label_values.insert(label, u8::try_from(ram_ptr).unwrap());
                ram_ptr += 1;
                if ram_ptr > 255 {
                    return Err("Out of RAM".to_string())
                }
            }
        }
    }

    Ok((bin, ptr.wrapping_add(1), label_values))



}

fn put_bin_constant(constant:&Constant, bin: &mut[u8;256], label_references: &mut Vec<(u8, String)>, ptr: usize) -> () {
    match constant {
        Constant::Label(label) => {
            label_references.push((u8::try_from(ptr).unwrap(), label.to_owned()));
        }
        Constant::Literal(num) => {
            bin[ptr] = *num;
        }
    }
}