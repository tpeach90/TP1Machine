use std::{cell::RefCell, usize};

use crate::{ast::{BlockNode, ConditionDetail, ConditionNode, ConstantDetail, ConstantNode, ExpressionDetail, ExpressionNode, InnerStatementDetail, InnerStatementNode, MemoryLocationDetail, MemoryLocationNode, NumberNode, OuterStatementDetail, OuterStatementNode, ProgramNode, TypeBodyDetail, TypeBodyNode, TypeDetail, TypeNode}, common::{BranchFlag, CodeLocation}, tokens::{Keyword, Operator, Token, TokenDetail}};

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

    fn advance_token(&mut self) -> Token {
        self.offset += 1;
        self.tokens[self.offset].clone()
    }

    fn current_token(&self) -> Token {
        self.tokens[self.offset].clone()
    }

    pub fn parse_program(&mut self) -> Result<Box<ProgramNode>, ParseError> {

        let lookahead = self.current_token();
        let mut outer_statements = vec![];
        while !matches!(self.current_token().t, TokenDetail::Keyword(Keyword::Main)) {
            outer_statements.push(self.parse_outer_statement()?);
            let lookahead2 = self.current_token();
            if !matches!(lookahead2.t, TokenDetail::SemiColon) {
                return Err(ParseError { loc: lookahead.loc, message: "Expected a semi colon".to_string() })
            }
            self.advance_token();
        }
        self.advance_token();
        let main = self.parse_block()?;
        Ok(Box::new(ProgramNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: main.loc.end_index }, outer_statements, main }))

    }

    fn parse_outer_statement(&mut self) -> Result<Box<OuterStatementNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Keyword(Keyword::Let) => self.parse_outer_declaration(),
            _ => Err(ParseError { loc: lookahead.loc, message: "Expected a declaration or main block".to_string() })
        }
    }

    fn parse_outer_declaration(&mut self) -> Result<Box<OuterStatementNode>, ParseError> {
        let expect_let_keyword = self.current_token();
        if !matches!(expect_let_keyword.t, TokenDetail::Keyword(Keyword::Let)) {
            return Err(ParseError { loc: expect_let_keyword.loc, message: "Expected a declaration".to_string() })
        }
        self.advance_token();
        let type_node = self.parse_type()?;

        let expect_identifier = self.current_token();
        let identifier = match expect_identifier.t {
            TokenDetail::Identifier(_identifier) => Ok(_identifier),
            _ => Err(ParseError{loc: expect_identifier.loc, message: "Expected an identifier".to_string()})
        }?;
        
        let expect_equals = self.advance_token();
        if !matches!(expect_equals.t, TokenDetail::Equals) {
            return Err(ParseError { loc: expect_equals.loc, message: "Expected an initial value".to_string() })
        }
        self.advance_token();
        let value = self.parse_constant()?;
        Ok(Box::new(OuterStatementNode { loc: CodeLocation { start_index: expect_let_keyword.loc.start_index, end_index: value.loc.end_index }, d: OuterStatementDetail::DeclarationStatement { r#type: type_node, identifier, val: value }}))
    }

    fn parse_block(&mut self) -> Result<Box<BlockNode>, ParseError> {
        let lookahead = self.current_token();
        if !matches!(lookahead.t, TokenDetail::LeftBrace) {
            return Err(ParseError { loc: lookahead.loc, message: "Expected a block".to_string() });
        }
        self.advance_token();

        let mut statements: Vec<Box<InnerStatementNode>> = vec![];
        while !matches!(self.current_token().t, TokenDetail::RightBrace) {

            if matches!(self.current_token().t, TokenDetail::EOF) {
                return Err(ParseError { loc: lookahead.loc, message: "Missing a closing brace".to_string() })
            }

            let node = self.parse_inner_statement()?;
            statements.push(node);
            if matches!(self.current_token().t, TokenDetail::SemiColon) {
                self.advance_token();
            } else {
                return Err(ParseError { loc: self.current_token().loc, message: "Expected a semi colon".to_string() });
            }
        }

        let loc = CodeLocation {start_index: lookahead.loc.start_index, end_index: self.current_token().loc.end_index};
        self.advance_token();
        Ok(Box::new(BlockNode { loc, statements }))
    }

    fn parse_inner_statement(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Keyword(Keyword::Forever) => self.parse_forever_loop(),
            TokenDetail::Keyword(Keyword::While) => self.parse_while_loop(),
            TokenDetail::Keyword(Keyword::Do) => self.parse_do_while_loop(),
            TokenDetail::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenDetail::Keyword(Keyword::Let) => self.parse_inner_declaration_statement(),
            TokenDetail::Keyword(Keyword::Break) => {
                self.advance_token();
                Ok(Box::new(InnerStatementNode { loc: lookahead.loc, d: InnerStatementDetail::BreakStatement {  } }))
            },
            TokenDetail::Keyword(Keyword::Continue) => {
                self.advance_token();
                Ok(Box::new(InnerStatementNode { loc: lookahead.loc, d: InnerStatementDetail::ContinueStatement {  } }))
            }

            TokenDetail::Identifier(_) | 
            TokenDetail::LeftBracket => {
                // could be expression or assignment
                // look to see if there's an Equals before the next SemiColon or EOF
                for i in self.offset..self.tokens.len() {
                    if matches!(self.tokens[i].t, TokenDetail::SemiColon) {
                        return self.parse_expression_statement();
                    } else if matches!(self.tokens[i].t, TokenDetail::Equals) {
                        return self.parse_assignment_statement();
                    }
                }
                return self.parse_expression_statement();
            }

            TokenDetail::LeftParenthesis |
            TokenDetail::Operator(Operator::Minus) |
            TokenDetail::Number(_) |
            TokenDetail::Operator(Operator::Tilde) | 
            TokenDetail::Operator(Operator::Exclaimation) | 
            TokenDetail::Operator(Operator::Underscore) => {
                self.parse_expression_statement()
            }

            TokenDetail::LeftBrace => {
                let node = self.parse_block()?;
                Ok (Box::new(InnerStatementNode { loc: node.loc, d: InnerStatementDetail::Block { body: node } }))
            },

            _ => Err(ParseError { loc: lookahead.loc, message: "Expected a statement".to_string() })

        }
    }

    fn parse_forever_loop(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let lookahead = self.current_token();
        if !matches!(lookahead.t, TokenDetail::Keyword(Keyword::Forever)) {
            return Err(ParseError { loc: self.current_token().loc, message: "Expected a forever loop".to_string()});
        }
        self.advance_token();
        
        let block = self.parse_block()?;
        return Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: block.loc.end_index }, d: InnerStatementDetail::ForeverLoop { body: block } }));
        
    }
    fn parse_while_loop(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let lookahead = self.current_token();
        if !matches!(lookahead.t, TokenDetail::Keyword(Keyword::While)) {
            return Err(ParseError { loc: self.current_token().loc, message: "Expected a while loop".to_string()});
        }
        self.advance_token();

        let condition: Box<ConditionNode> = self.parse_condition()?;
        let body = self.parse_block()?;

        return Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: body.loc.end_index }, d: InnerStatementDetail::WhileLoop { condition, body } }));

    }
    fn parse_do_while_loop(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let expect_do_keyword = self.current_token();
        if !matches!(expect_do_keyword.t, TokenDetail::Keyword(Keyword::Do)) {
            return Err(ParseError { loc: expect_do_keyword.loc, message: "Expected a do-while loop".to_string()});
        }
        self.advance_token();

        let body = self.parse_block()?;
        let expect_while_keyword = self.current_token();
        if !matches!(expect_while_keyword.t, TokenDetail::Keyword(Keyword::While)) {
            return Err(ParseError { loc: expect_while_keyword.loc, message: "Expected a while condition".to_string()});
        }
        self.advance_token();
        let condition = self.parse_condition()?;

        return Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: expect_do_keyword.loc.start_index, end_index: condition.loc.end_index }, d: InnerStatementDetail::DoWhileLoop  { body, condition } }));

    }
    fn parse_if_statement(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let expect_if_keyword = self.current_token();
        if !matches!(expect_if_keyword.t, TokenDetail::Keyword(Keyword::If)) {
            return Err(ParseError { loc: expect_if_keyword.loc, message: "Expected an if statement".to_string()});
        }
        self.advance_token();

        let condition = self.parse_condition()?;
        let body_if_true = self.parse_block()?;

        let maybe_else_keyword = self.current_token();
        if matches!(maybe_else_keyword.t, TokenDetail::Keyword(Keyword::Else)) {
            self.advance_token();
            // could be an "else" or an "else if" here
            let lookahead = self.current_token();
            if matches!(lookahead.t, TokenDetail::Keyword(Keyword::If)) {
                // "else if"
                // treat this as if the second if statement were a single statement inside the else block.
                let next_if_statement = self.parse_if_statement()?;
                let body_if_false = Box::new(BlockNode{loc: next_if_statement.loc, statements:vec![next_if_statement]});
                Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: expect_if_keyword.loc.start_index, end_index: body_if_false.loc.end_index }, d: InnerStatementDetail::IfElseStatement { condition, r#true: body_if_true, r#false: body_if_false } }))
               
            } else if matches!(lookahead.t, TokenDetail::LeftBrace) {
                // "else"
                let body_if_false = self.parse_block()?;
                Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: expect_if_keyword.loc.start_index, end_index: body_if_false.loc.end_index }, d: InnerStatementDetail::IfElseStatement { condition, r#true: body_if_true, r#false: body_if_false } }))
            } else {
                Err(ParseError { loc: lookahead.loc, message: "Expected a block or another if statement".to_string() })
            }
        } else {
            // no else
            Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: expect_if_keyword.loc.start_index, end_index: body_if_true.loc.end_index }, d: InnerStatementDetail::IfStatement { condition: condition, r#true: body_if_true } }))
        }

    }
    fn parse_inner_declaration_statement(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let expect_let_keyword = self.current_token();
        if !matches!(expect_let_keyword.t, TokenDetail::Keyword(Keyword::Let)) {
            return Err(ParseError { loc: expect_let_keyword.loc, message: "Expected a declaration".to_string() })
        }
        self.advance_token();
        let type_node = self.parse_type()?;
        let expect_identifier = self.current_token();
        let identifier = match expect_identifier.t {
            TokenDetail::Identifier(_identifier) => Ok(_identifier),
            _ => Err(ParseError{loc: expect_identifier.loc, message: "Expected an identifier".to_string()})
        }?;
        
        let lookahead = self.advance_token();
        if matches!(lookahead.t, TokenDetail::Equals) {
            // initial value
            self.advance_token();
            let initial_value = self.parse_expression()?;
            Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: expect_let_keyword.loc.start_index, end_index: initial_value.loc.end_index }, d: InnerStatementDetail::DeclarationStatementWithInitialValue { r#type: type_node, identifier, val: initial_value } }))
        } else {
            // no initial value
            Ok(Box::new(InnerStatementNode{loc: CodeLocation { start_index: expect_let_keyword.loc.start_index, end_index: expect_identifier.loc.end_index }, d: InnerStatementDetail::DeclarationStatement { r#type: type_node, identifier }}))
        }



    }
    fn parse_assignment_statement(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let lvalue = self.parse_memory_location()?;
        let expect_equals = self.current_token();
        if !matches!(expect_equals.t, TokenDetail::Equals) {
            return Err(ParseError { loc: expect_equals.loc, message: "Expected an equals".to_string() })
        }
        self.advance_token();
        let rvalue = self.parse_expression()?;
        Ok(Box::new(InnerStatementNode { loc: CodeLocation { start_index: lvalue.loc.start_index, end_index: rvalue.loc.end_index }, d: InnerStatementDetail::AssignmentStatement { lvalue, rvalue } }))

    }

    fn parse_expression_statement(&mut self) -> Result<Box<InnerStatementNode>, ParseError> {
        let node = self.parse_expression()?;
        Ok(Box::new(InnerStatementNode { loc: node.loc, d: InnerStatementDetail::ExpressionStatement { val: node } }))
    }

    fn parse_condition(& mut self) -> Result<Box<ConditionNode>, ParseError> {
        let lookahead = self.current_token();
        if matches!(lookahead.t, TokenDetail::Keyword(Keyword::Flag)) {
            let flag_token = self.advance_token();
            let flag = match flag_token.t {
                TokenDetail::Identifier(ident) => match ident.as_str() {
                    "BZ" => Ok(BranchFlag::BZ),
                    "BNZ" => Ok(BranchFlag::BNZ),
                    "BC" => Ok(BranchFlag::BC),
                    "BNC" => Ok(BranchFlag::BNC),
                    "BN" => Ok(BranchFlag::BN),
                    "BNN" => Ok(BranchFlag::BNN),
                    "BO" => Ok(BranchFlag::BO),
                    "BNO" => Ok(BranchFlag::BNO),
                    "BGTE" => Ok(BranchFlag::BGTE),
                    "BNGTE" => Ok(BranchFlag::BNGTE),
                    _ => Err(ParseError{loc: flag_token.loc, message: "Expected a flag".to_string()})
                }
            _ => Err(ParseError{loc: flag_token.loc, message: "Expected a flag".to_string()})
            }?;
            self.advance_token();
            Ok(Box::new(ConditionNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: flag_token.loc.end_index }, d: ConditionDetail::Branch { flag } }))

        } else if matches!(lookahead.t, TokenDetail::LeftParenthesis) {
            self.advance_token();
            let expr = self.parse_expression()?;
            let expect_right_parenthesis = self.current_token();
            if !matches!(expect_right_parenthesis.t, TokenDetail::RightParenthesis) {
                Err(ParseError { loc: lookahead.loc, message: "Condition missing closing parenthesis".to_string() })
            } else {
                self.advance_token();
                Ok(Box::new(ConditionNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: expect_right_parenthesis.loc.end_index }, d: ConditionDetail::Expression { val: expr } }))
            }
        } else {
            Err(ParseError { loc: lookahead.loc, message: "Expected a condition. Expressions should be surrounded with parentheses.".to_string() })
        }
    }

    fn parse_memory_location(&mut self) -> Result<Box<MemoryLocationNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Identifier(ident) => {
                self.advance_token();
                Ok(Box::new(MemoryLocationNode { loc: lookahead.loc, d: MemoryLocationDetail::Identifier { val: ident } }))
            },
            TokenDetail::LeftBracket => {
                self.advance_token();
                // could be [expr], [expr:]

                // [expr
                let left = self.parse_expression()?;
                let lookahead2 = self.current_token();
                if matches!(lookahead2.t, TokenDetail::Colon) {
                    // [expr:
                    
                    let lookahead3 = self.advance_token();
                    if !matches!(lookahead3.t, TokenDetail::RightBracket) {
                        return Err(ParseError{loc: lookahead.loc, message: "Missing a closing bracket".to_string()})
                    }

                    self.advance_token();
                    let inner_location = self.parse_memory_location()?;
                    Ok(Box::new(MemoryLocationNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: inner_location.loc.end_index }, d: MemoryLocationDetail::ArraySlice { start: left, arr: inner_location } }))

                } else if matches!(lookahead2.t, TokenDetail::RightBracket) {
                    // [expr]
                    self.advance_token();
                    let inner_location = self.parse_memory_location()?;
                    Ok(Box::new(MemoryLocationNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: inner_location.loc.end_index }, d: MemoryLocationDetail::ArrayIndex { i: left, arr: inner_location } }))
                } else {
                    Err(ParseError { loc: lookahead2.loc, message: "Expected a colon or closing bracket".to_string() })
                }
             


            }
            _ => Err(ParseError { loc: lookahead.loc, message: "Expected a memory location".to_string() })
        }
    }

    fn parse_type(& mut self) -> Result<Box<TypeNode>, ParseError> {
        let lookahead = self.current_token();
        if matches!(lookahead.t, TokenDetail::Keyword(Keyword::Const)) {
            self.advance_token();
            let body = self.parse_type_body()?;
            Ok(Box::new(TypeNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: body.loc.end_index }, d: TypeDetail::ConstType { t: body } }))
        } else {
            let body = self.parse_type_body()?;
            Ok(Box::new(TypeNode { loc: body.loc, d: TypeDetail::NonConstType { t: body } }))
        }
    }

    fn parse_type_body(&mut self) -> Result<Box<TypeBodyNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Keyword(Keyword::Byte) => {
                self.advance_token();
                Ok(Box::new(TypeBodyNode { loc: lookahead.loc, d: TypeBodyDetail::Byte {  } }))
            },
            TokenDetail::LeftBracket => {
                self.advance_token();
                let size = self.parse_number()?;
                if !matches!(self.current_token().t, TokenDetail::RightBracket) {
                    return Err(ParseError { loc: lookahead.loc, message: "Missing a closing bracket".to_string() })
                }
                self.advance_token();
                let inner_type = self.parse_type_body()?;
                Ok(Box::new(TypeBodyNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: inner_type.loc.end_index }, d: TypeBodyDetail::Array { size, t: inner_type } }))
            },
            _ => {
                Err(ParseError { loc: lookahead.loc, message: "Expected a type body".to_string() })
            }
        }
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

            let combined_location = CodeLocation{start_index: lhs.loc.start_index, end_index: rhs.loc.end_index};
            let combined_expression_detail = match op1 {
                Operator::Exclaimation |
                Operator::Tilde |
                // Operator::Arobase |
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

            lhs = Box::new(ExpressionNode { loc: combined_location, d: combined_expression_detail, type_annotation: RefCell::new(None) })

        }


        Ok(lhs)
    }

    fn parse_primary(& mut self) -> Result<Box<ExpressionNode>, ParseError> {
        let lookahead = self.tokens[self.offset].clone();

        match lookahead.t {

            // Number
            TokenDetail::Number(_) |
            TokenDetail::Operator(Operator::Minus) => {
                let node = self.parse_number()?;
                let loc = CodeLocation{ 
                    start_index: lookahead.loc.start_index, 
                    end_index: self.tokens[self.offset].loc.start_index
                };
                Ok(Box::new(ExpressionNode{loc:loc, d:ExpressionDetail::Number { val: node }, type_annotation: RefCell::new(None)}))
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

            TokenDetail::Identifier(_) => {
                // could be function call or memory location
                if matches!(self.tokens[self.offset+1].t, TokenDetail::LeftParenthesis) {
                    self.parse_function_call()
                } else {
                    let node = self.parse_memory_location()?;
                    Ok(Box::new(ExpressionNode { loc: node.loc, d: ExpressionDetail::MemoryValue { val: node }, type_annotation: RefCell::new(None) }))
                }
            }

            // // memory location
            // TokenDetail::Operator(Operator::Arobase) |
            // TokenDetail::Operator(Operator::Asterix) |
            // TokenDetail::LeftBracket => {
            //     let node = self.parse_memory_location()?;
            //     Ok(Box::new(ExpressionNode { loc: node.loc, d: ExpressionDetail::MemoryValue { val: node } }))
            // }

            // // reference
            // TokenDetail::Operator(Operator::Ampersand) => {
            //     self.advance_token();
            //     let node = self.parse_memory_location()?;
            //     Ok(Box::new(ExpressionNode { loc: node.loc, d: ExpressionDetail::MemoryReference { val: node } }))
            // }

            // array
            TokenDetail::LeftBrace => self.parse_array(),

            // unary operators
            TokenDetail::Operator(Operator::Tilde) | 
            TokenDetail::Operator(Operator::Exclaimation) | 
            TokenDetail::Operator(Operator::Underscore) => self.parse_unary(),
            


            _ => Err(ParseError { loc: lookahead.loc, message: format!("Expected a number, array, unary operator, bracketed expression, memory location, or reference") })
        }


    }

    fn parse_function_call(&mut self) -> Result<Box<ExpressionNode>, ParseError> {
        let expect_identifier = self.current_token();
        let ident = match expect_identifier.t {
            TokenDetail::Identifier(_ident) => Ok(_ident),
            _ => Err(ParseError{loc: expect_identifier.loc, message: "Expected a function call".to_string()})
        }?;
        let expect_left_parenthesis = self.advance_token();
        if !matches!(expect_left_parenthesis.t, TokenDetail::LeftParenthesis) {
            return Err(ParseError { loc: expect_left_parenthesis.loc, message: "Expected a list of function arguments".to_string() });
        }
        
        let lookahead = self.advance_token();
        if matches!(lookahead.t, TokenDetail::RightParenthesis) {
            Ok(Box::new(ExpressionNode { loc: CodeLocation { start_index: expect_identifier.loc.start_index, end_index: lookahead.loc.end_index }, d: ExpressionDetail::FunctionCall { ident, args: vec![] }, type_annotation: RefCell::new(None)}))
        } else {
            let mut args = vec![self.parse_expression()?];
            while !matches!(self.current_token().t, TokenDetail::RightParenthesis) {
                let lookahead2 = self.current_token();
                if !matches!(lookahead2.t, TokenDetail::Comma) {
                    return Err(ParseError { loc: lookahead2.loc, message: "Expected a comma or end of function arguments".to_string() });
                }
                self.advance_token();
                args.push(self.parse_expression()?);
            }
            let final_token_loc = self.current_token().loc;
            self.advance_token();
            Ok(Box::new(ExpressionNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: final_token_loc.end_index }, d: ExpressionDetail::FunctionCall { ident, args } , type_annotation: RefCell::new(None)}))
        }


    }

    fn parse_constant(&mut self) -> Result<Box<ConstantNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Operator(Operator::Minus) |
            TokenDetail::Number(_) => {
                let number = self.parse_number()?;
                Ok(Box::new(ConstantNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: number.loc.end_index }, d: ConstantDetail::Number { val: number } }))
            },
            TokenDetail::LeftBrace => {
                self.parse_constant_array()
            }
            _ => Err(ParseError { loc: lookahead.loc, message: "Expected a constant".to_string() })
        }

    }

    fn parse_constant_array(&mut self) -> Result<Box<ConstantNode>, ParseError> {
        let lookahead = self.current_token();
        if !matches!(lookahead.t, TokenDetail::LeftBrace) {
            return Err(ParseError { loc: lookahead.loc, message: "Expected an array".to_string() });
        }
        let lookahead2 = self.advance_token();
        if matches!(lookahead2.t, TokenDetail::RightBrace) {
            Ok(Box::new(ConstantNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: lookahead.loc.end_index }, d: ConstantDetail::Array { val: vec![] } }))
        } else {
            let mut array_contents = vec![self.parse_constant()?];
            while !matches!(self.current_token().t, TokenDetail::RightBrace) {
                let lookahead3 = self.current_token();
                if !matches!(lookahead3.t, TokenDetail::Comma) {
                    return Err(ParseError { loc: lookahead3.loc, message: "Expected a comma or end of array contents".to_string() });
                }
                self.advance_token();
                array_contents.push(self.parse_constant()?);
            }
            let final_token_loc = self.current_token().loc;
            self.advance_token();
            Ok(Box::new(ConstantNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: final_token_loc.end_index }, d: ConstantDetail::Array { val: array_contents } }))
        }
    }

    fn parse_array(&mut self) -> Result<Box<ExpressionNode>, ParseError> {
        let lookahead = self.current_token();
        if !matches!(lookahead.t, TokenDetail::LeftBrace) {
            return Err(ParseError { loc: lookahead.loc, message: "Expected an array".to_string() });
        }
        let lookahead2 = self.advance_token();
        if matches!(lookahead2.t, TokenDetail::RightBrace) {
            Ok(Box::new(ExpressionNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: lookahead2.loc.end_index }, d: ExpressionDetail::Array { val: vec![] } , type_annotation: RefCell::new(None)}))
        } else {
            let mut array_contents = vec![self.parse_expression()?];
            while !matches!(self.current_token().t, TokenDetail::RightBrace) {
                let lookahead3 = self.current_token();
                if !matches!(lookahead3.t, TokenDetail::Comma) {
                    return Err(ParseError { loc: lookahead3.loc, message: "Expected a comma or end of array contents".to_string() });
                }
                self.advance_token();
                array_contents.push(self.parse_expression()?);
            }
            let final_token_loc = self.current_token().loc;
            self.advance_token();
            Ok(Box::new(ExpressionNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: final_token_loc.end_index }, d: ExpressionDetail::Array { val: array_contents } , type_annotation: RefCell::new(None)}))
        }
    }

    fn parse_unary(&mut self) -> Result<Box<ExpressionNode>, ParseError> {
        let lookahead = self.tokens[self.offset].clone();
        self.offset += 1;

        let node = self.parse_primary()?;
        let unary_expr_loc = CodeLocation {start_index: lookahead.loc.start_index, end_index: node.loc.end_index};
        
        match lookahead.t {
            TokenDetail::Operator(Operator::Tilde) => Ok(Box::new(ExpressionNode { loc: unary_expr_loc, d: ExpressionDetail::BitwiseNOT { val: node } , type_annotation: RefCell::new(None)})),
            TokenDetail::Operator(Operator::Exclaimation) => Ok(Box::new(ExpressionNode { loc: unary_expr_loc, d: ExpressionDetail::LogicalNOT { val: node } , type_annotation: RefCell::new(None)})),
            TokenDetail::Operator(Operator::Underscore) => Ok(Box::new(ExpressionNode { loc: unary_expr_loc, d: ExpressionDetail::Log2 { val: node }, type_annotation: RefCell::new(None) })),
            _ => Err(ParseError { loc: lookahead.loc, message: "Expected a unary operator".to_string() })
        }
    }

    fn parse_number(& mut self) -> Result<Box<NumberNode>, ParseError> {
        let lookahead = self.current_token();
        match lookahead.t {
            TokenDetail::Number(num) => {
                self.advance_token();
                Ok (Box::new(NumberNode { loc: lookahead.loc, val: num }))
            },
            TokenDetail::Operator(Operator::Minus) => {
                let lookahead2 = self.advance_token();
                // expect a number next
                match lookahead2.t {
                    TokenDetail::Number(num) => {
                        if num > 128 {
                            Err(ParseError { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: lookahead2.loc.end_index }, message: "Number must be at least -128".to_string() })
                        } else {
                            self.advance_token();
                            // 2's compelement
                            Ok (Box::new(NumberNode { loc: CodeLocation { start_index: lookahead.loc.start_index, end_index: lookahead2.loc.end_index }, val: (!num).wrapping_add(1) }))
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
        // Operator::Arobase |
        Operator::Underscore => 0,
        
        Operator::Asterix | // multiply
        Operator::ForwardSlash | // divide
        Operator::Percent => 9, // modulo
        
        Operator::Plus |
        Operator::Minus => 8,
        
        Operator::UnsignedLessThan |
        Operator::UnsignedGreaterThan |
        Operator::UnsignedLessThanOrEqualTo |
        Operator::UnsignedGreaterThanOrEqualTo |
        Operator::SignedLessThan |
        Operator::SignedGreaterThan |
        Operator::SignedLessThanOrEqualTo |
        Operator::SignedGreaterThanOrEqualTo => 7,
        
        Operator::Equality => 6,
        
        Operator::Ampersand => 5, // bitwise AND
        
        Operator::Caret => 4, // bitwise XOR
        
        Operator::Bar => 3, // bitwise OR
        
        Operator::DoubleAmpersand => 2, // logical AND
        
        Operator::DoubleBar => 1, // logical OR
        
    }
}

fn is_maybe_binary(op: &Operator) -> bool {
    match op {
        Operator::Tilde |
        Operator::Exclaimation |
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

