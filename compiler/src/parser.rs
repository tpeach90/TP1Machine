use std::usize;

use crate::{ast::{ExpressionDetail, ExpressionNode, NumberNode}, common::CodeLocation, tokens::{Operator, Token, TokenDetail}};

pub struct Parser<'b> {
    tokens: &'b Vec<Token>,
    offset: usize,
}

#[derive(Debug)]
pub struct ParseError {
    pub loc: CodeLocation,
    pub message: String,
}

impl Parser <'_> {

    pub fn new<'b>(tokens: &'b Vec<Token>) -> Parser<'b> {
        Parser { tokens, offset: 0 }
    }

    pub fn parse_expression(&mut self) -> Result<Box<ExpressionNode>, ParseError> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser
        let lhs = self.parse_primary()?;
        self.parse_expression_1(lhs, 0)
    }

    fn parse_expression_1(&mut self, lhs: Box<ExpressionNode>, min_precedence: usize) -> Result<Box<ExpressionNode>, ParseError> {

        let mut lookahead = self.tokens[self.offset].clone();
        let mut op1: Operator = Operator::Plus; // init to something arbitrary otherwise errors.
        let mut lhs = lhs;
        let mut op1_token: Token;

        while match lookahead.t {
            // while lookahead is a binary operator whose precedence is >= min_precedence
            TokenDetail::Operator(_op) => 
                if is_maybe_binary(&_op) && binary_precedence(&_op) >= min_precedence {
                    op1 = _op;
                    true
                } else {
                    false
                },
            _ => false
        } {
            op1_token = lookahead.clone();
            self.offset += 1;
            let mut rhs = self.parse_primary()?;
            lookahead = self.tokens[self.offset].clone();

            while match lookahead.t {
                // while lookahead is a binary operator whose precedence is greater than op's, or a right-associative operator
                TokenDetail::Operator(op2) => 
                    is_maybe_binary(&op2) && binary_precedence(&op2) > binary_precedence(&op1),
                _ => false
            } {
                rhs = self.parse_expression_1(rhs, binary_precedence(&op1)+1)?;
                lookahead = self.tokens[self.offset].clone();
            }

            let combined_location = CodeLocation{startIndex: lhs.loc.startIndex, endIndex: rhs.loc.endIndex};
            let combined_expression_detail = match op1 {
                Operator::Exclaimation |
                Operator::Tilde |
                Operator::Arobase |
                Operator::Underscore => Err(ParseError{loc: op1_token.loc, message:format!("{} is not a binary operator", op1_token.text)}),
                
                Operator::Plus => Ok(ExpressionDetail::Add { left: lhs, right: rhs }),
                Operator::Minus => Ok(ExpressionDetail::Subtract { left: lhs, right: rhs }),
                Operator::Asterix => Ok(ExpressionDetail::Multiply { left: lhs, right: rhs }),
                Operator::Caret => Ok(ExpressionDetail::BitwiseXOR { left: lhs, right: rhs }),
                Operator::ForwardSlash => Ok(ExpressionDetail::Divide { left: lhs, right: rhs }),
                Operator::Percent => Ok(ExpressionDetail::Modulo { left: lhs, right: rhs }),
                Operator::Bar => Ok(ExpressionDetail::BitwiseOR { left: lhs, right: rhs }),
                Operator::Ampersand => Ok(ExpressionDetail::BitwiseAND { left: lhs, right: rhs }),
                Operator::DoubleBar => Ok(ExpressionDetail::LogicalOR { left: lhs, right: rhs }),
                Operator::DoubleAmpersand => Ok(ExpressionDetail::LogicalAND { left: lhs, right: rhs }),
                Operator::Equality => Ok(ExpressionDetail::EqualTo { left: lhs, right: rhs }),
                Operator::UnsignedLessThan => Ok(ExpressionDetail::UnsignedLessThan { left: lhs, right: rhs }),
                Operator::UnsignedGreaterThan => Ok(ExpressionDetail::UnsignedGreaterThan{ left: lhs, right: rhs }),
                Operator::UnsignedLessThanOrEqualTo => Ok(ExpressionDetail::UnsignedLessThanOrEqualTo{ left: lhs, right: rhs }),
                Operator::UnsignedGreaterThanOrEqualTo => Ok(ExpressionDetail::UnsignedGreaterThanOrEqualTo{ left: lhs, right: rhs }),
                Operator::SignedLessThan => Ok(ExpressionDetail::SignedLessThan{ left: lhs, right: rhs }),
                Operator::SignedGreaterThan => Ok(ExpressionDetail::SignedGreaterThan{ left: lhs, right: rhs }),
                Operator::SignedLessThanOrEqualTo => Ok(ExpressionDetail::SignedLessThanOrEqualTo{ left: lhs, right: rhs }),
                Operator::SignedGreaterThanOrEqualTo => Ok(ExpressionDetail::SignedGreaterThanOrEqualTo{ left: lhs, right: rhs }),
            }?;

            lhs = Box::new(ExpressionNode { loc: combined_location, d: combined_expression_detail })

        }


        Ok(lhs)
    }

    fn parse_primary(& mut self) -> Result<Box<ExpressionNode>, ParseError> {
        let lookahead = self.tokens[self.offset].clone();

        match lookahead.t {

            // Number
            TokenDetail::Integer(_) |
            TokenDetail::Operator(Operator::Minus) => {
                let node = self.parse_number()?;
                let loc = CodeLocation{ 
                    startIndex: lookahead.loc.startIndex, 
                    endIndex: self.tokens[self.offset].loc.startIndex
                };
                Ok(Box::new(ExpressionNode{loc:loc, d:ExpressionDetail::Number { val: node }}))
            }
            
            // bracketed expression
            TokenDetail::LeftParenthesis => {
                self.offset += 1;
                let node = self.parse_expression()?;
                if matches!(self.tokens[self.offset].t, TokenDetail::RightParenthesis) {
                    self.offset += 1;
                    Ok(node)
                } else {
                    Err(ParseError { loc: lookahead.loc, message: format!("Missing a closing parenthesis") })
                }
            }

            // memory location
            TokenDetail::Identifier(_) |
            TokenDetail::Operator(Operator::Arobase) |
            TokenDetail::Operator(Operator::Asterix) |
            TokenDetail::LeftBracket => todo!(),

            // reference
            TokenDetail::Operator(Operator::Ampersand) => todo!(),

            // array
            TokenDetail::LeftBrace => todo!(),


            _ => Err(ParseError { loc: lookahead.loc, message: format!("Expected a number, bracketed expression, memory location or reference") })
        }


    }

    fn parse_number(& mut self) -> Result<Box<NumberNode>, ParseError> {
        let lookahead = self.tokens[self.offset].clone();
        match lookahead.t {
            TokenDetail::Integer(num) => {
                match num.parse::<u8>() {
                    Ok(number) => {
                        self.offset += 1;
                        Ok (Box::new(NumberNode { loc: lookahead.loc, val: number }))
                    },
                    Err(_) => Err(ParseError { loc: lookahead.loc, message: format!("Number out of allowed range [-128, 255]") })
                }
            },
            TokenDetail::Operator(Operator::Minus) => {
                self.offset += 1;
                let lookahead2 = self.tokens[self.offset].clone();
                // expect an integer next
                match lookahead2.t {
                    TokenDetail::Integer(num) => {
                        match num.parse::<u8>() {
                            Ok(number) => {
                                if number <= 128 {
                                    self.offset += 1;
                                    // 2's complement
                                    Ok (Box::new(NumberNode { loc: lookahead.loc, val: (!number).wrapping_add(1) }))
                                } else {
                                    Err(ParseError { loc: lookahead.loc, message: format!("Number out of allowed range [-128, 255]") })
                                }
                            },
                            Err(_) => Err(ParseError { loc: lookahead.loc, message: format!("Number out of allowed range [-128, 255]") })
                        }
                    }
                    _ => Err(ParseError { loc: lookahead2.loc, message: format!("Expected a number") })
                }
            }
            _ => Err(ParseError { loc: lookahead.loc, message: format!("Expected a number") })
        }
    }




}

fn binary_precedence(op: &Operator) -> usize {
    match op {
        
        // unary ops not relevant
        Operator::Exclaimation |
        Operator::Tilde |
        Operator::Arobase |
        Operator::Underscore => 0,
        
        Operator::Asterix | // multiply
        Operator::ForwardSlash | // divide
        Operator::Percent => 90, // modulo
        
        Operator::Plus |
        Operator::Minus => 80,
        
        Operator::UnsignedLessThan |
        Operator::UnsignedGreaterThan |
        Operator::UnsignedLessThanOrEqualTo |
        Operator::UnsignedGreaterThanOrEqualTo |
        Operator::SignedLessThan |
        Operator::SignedGreaterThan |
        Operator::SignedLessThanOrEqualTo |
        Operator::SignedGreaterThanOrEqualTo => 70,
        
        Operator::Equality => 60,
        
        Operator::Ampersand => 50, // bitwise AND
        
        Operator::Caret => 40, // bitwise XOR
        
        Operator::Bar => 30, // bitwise OR
        
        Operator::DoubleAmpersand => 20, // logical AND
        
        Operator::DoubleBar => 10, // logical OR
        
    }
}

fn is_maybe_binary(op: &Operator) -> bool {
    match op {
        Operator::Tilde |
        Operator::Exclaimation |
        Operator::Arobase |
        Operator::Underscore => false,
        
        Operator::Plus |
        Operator::Minus |
        Operator::Asterix |
        Operator::Caret |
        Operator::ForwardSlash |
        Operator::Percent |
        Operator::Ampersand |
        Operator::Bar |
        Operator::DoubleBar |
        Operator::DoubleAmpersand |
        Operator::Equality |
        Operator::UnsignedLessThan |
        Operator::UnsignedGreaterThan |
        Operator::UnsignedLessThanOrEqualTo |
        Operator::UnsignedGreaterThanOrEqualTo |
        Operator::SignedLessThan |
        Operator::SignedGreaterThan |
        Operator::SignedLessThanOrEqualTo |
        Operator::SignedGreaterThanOrEqualTo => true,
    }
} 

