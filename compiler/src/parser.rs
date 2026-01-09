use std::usize;

use crate::{ast::{ExpressionDetail, ExpressionNode, Node}, common::CodeLocation, tokens::{Operator, Token, TokenDetail}};
use bumpalo::Bump;

pub struct Parser<'b> {
    tokens: &'b Vec<Token>,
    offset: usize,
    nodes: Bump,
}

impl Parser <'_> {

    fn parse_expression(&mut self) -> Result<& ExpressionNode, String> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser
        let lhs = self.parse_primary()?;
        self.parse_expression_1(lhs, 0)
    }

    fn parse_expression_1(&mut self, lhs: &ExpressionNode, min_precedence: usize) -> Result<&ExpressionNode, String> {

        let mut lookahead = &self.tokens[self.offset];
        let mut op1: &Operator = &Operator::Plus; // init to something arbitrary otherwise errors.

        while match &lookahead.t {
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
            self.offset += 1;
            let mut rhs = self.parse_primary()?;
            lookahead = &self.tokens[self.offset];

            while match &lookahead.t {
                // while lookahead is a binary operator whose precedence is greater than op's, or a right-associative operator
                TokenDetail::Operator(op2) => 
                    is_maybe_binary(op2) && binary_precedence(op2) > binary_precedence(op1),
                _ => false
            } {
                rhs = self.parse_expression_1(rhs, binary_precedence(op1)+1)?;
            }
            

        }


        Err(todo!())
    }

    fn parse_primary(& mut self) -> Result<& ExpressionNode, String> {
        let node = self.nodes.alloc(ExpressionNode {
            loc: todo!(), 
            d: ExpressionDetail::Number { val: 0 }
        });
        Ok(&node)
    }
}

fn binary_precedence(op: &Operator) -> usize {
    match op {
        
        // unary ops not relevant
        Operator::Exclaimation => usize::MAX,
        Operator::Tilde => usize::MAX,
        Operator::Arobase => usize::MAX,
        Operator::Underscore => usize::MAX,
        
        Operator::Asterix => 90, // multiply
        Operator::ForwardSlash => 90, // divide
        Operator::Percent => 90, // modulo
        
        Operator::Plus => 80,
        Operator::Minus => 80,
        
        Operator::UnsignedLessThan => 70,
        Operator::UnsignedGreaterThan => 70,
        Operator::UnsignedLessThanOrEqualTo => 70,
        Operator::UnsignedGreaterThanOrEqualTo => 70,
        Operator::SignedLessThan => 70,
        Operator::SignedGreaterThan => 70,
        Operator::SignedLessThanOrEqualTo => 70,
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
        Operator::Underscore => true,
        
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
        Operator::SignedGreaterThanOrEqualTo => false,
    }
} 

