
use std::{cmp::min, collections::HashMap, iter::zip, ptr, vec};

// major todo: implement const ROM variables

use itertools::enumerate;

use crate::{ast::*, common::{BranchFlag, CodeLocation}, ir::{BasicBlock, Binary, BlockPointer, Branch, IntermediateRepresentation, Nullary, Stmt, StmtArg, StmtKind, Unary, VarID}};

pub struct SemanticError {
    pub loc: CodeLocation,
    pub message: String,
}

struct Context {
    declaration_stack: ContextStack,
    loop_stack: LoopStack
}

impl Context {
    fn new() -> Context {
        Context { declaration_stack: vec![], loop_stack: vec![]}
    }

    fn new_frame(&mut self) {
        self.declaration_stack.push(ContextFrame { declarations: HashMap::new() });
    }

    fn pop_frame(&mut self) -> Option<ContextFrame> {
        self.declaration_stack.pop()
    }

    fn get_variable_declaration(&self, ident: &str) -> Option<SymbolInfo> {
        for frame in self.declaration_stack.iter().rev() {
            match frame.declarations.get(ident) {
                Some(info) => return Some(info.clone()),
                None => ()
            }
        }
        return None;
    }

    fn get_variable_declaration_in_outermost_frame(&self, ident: &str) -> Option<SymbolInfo> {
        match self.declaration_stack.last() {
            None => None,
            Some(frame) => frame.declarations.get(ident).cloned()
        }
    }

    fn add_declaration(&mut self, ident: &str, info: SymbolInfo) {
        self.declaration_stack.last_mut().unwrap().declarations.insert(ident.to_owned(), info);
    }

    fn add_loop(&mut self, continue_block: BlockPointer, break_block: BlockPointer) {
        self.loop_stack.push(LoopFrame { continue_block, break_block });
    }
    fn pop_loop(&mut self) -> Option<LoopFrame> {
        self.loop_stack.pop()
    }
    fn get_continue_block(&mut self) -> Option<BlockPointer> {
        self.loop_stack.last().and_then(|frame| Some(frame.continue_block))
    }
    fn get_break_block(&mut self) -> Option<BlockPointer> {
        self.loop_stack.last().and_then(|frame| Some(frame.break_block))
    }

}

type ContextStack = Vec<ContextFrame>;
struct ContextFrame {
    declarations: HashMap<String/*ident*/, SymbolInfo>,
}

type LoopStack = Vec<LoopFrame>;
struct LoopFrame {
    continue_block: BlockPointer,
    break_block: BlockPointer,
}

#[derive(Clone)]
struct SymbolInfo {
    id: VarID,
    r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Type {
    // innermost byte[1] is implied.
    nesting: Vec<usize>, 

}

impl Type {

    fn size(&self) -> usize {
        let mut size = 1;
        for level in &self.nesting {
            size *= level;
        }
        return size;
    }

    fn depth(&self) -> usize {
        self.nesting.len()
    }

    fn inner(&self) -> Option<Type> {
        match &self.nesting[..] {
            [_, tail @ ..] => {
                Some(Type{
                    nesting: tail.to_vec()
                })
            },
            _ => None
        }
    }

    fn strip_layers(&self, n:usize) -> Option<Type> {
        if self.nesting.len() > n {
            None
        } else {
            Some(Type{
                nesting: self.nesting[n..].to_vec()
            })
        }
    }

    fn enlarged_to_fit(&self, other: &Type) -> Type {
        let mut nesting = vec![];
        for (s, o) in zip(&self.nesting, &other.nesting) {
            nesting.push(if *s > *o {*s} else {*o});
        }
        Type {
            nesting,
        }
    }

}

const BYTE: Type = Type {nesting: vec![]};


struct IRBuilder {
    pub ir: IntermediateRepresentation,
    curr_block: BlockPointer,
    insert_line: usize,
    id_counter: VarID,
}


impl IRBuilder {
    fn new() -> IRBuilder {
        let mut ir = IntermediateRepresentation::new();
        ir.basic_blocks.push(BasicBlock::new());
        IRBuilder { ir: IntermediateRepresentation::new(), curr_block: 0, insert_line: 0, id_counter: 0 }
    }

    fn new_block(&mut self) -> BlockPointer {
        self.ir.basic_blocks.push(BasicBlock::new());
        return self.ir.basic_blocks.len() - 1;
    }

    fn set_insert_point(&mut self, block_ptr: BlockPointer, insert_line: usize) {
        self.curr_block = block_ptr;
        self.insert_line = insert_line;
    }

    fn set_insert_point_at_block_end(&mut self, block_ptr: BlockPointer) {
        let insert_line = self.ir.basic_blocks[block_ptr].statements.len();
        self.set_insert_point(block_ptr, insert_line);
    }

    fn insert_statement(&mut self, statement: Stmt) {
        self.ir.basic_blocks[self.curr_block].statements.insert(self.insert_line, statement);
        self.insert_line += 1;
    }

    fn set_branch(&mut self, branch: Option<Branch>) {
        self.ir.basic_blocks[self.curr_block].branch = branch;
    }

    fn set_continue(&mut self, continue_to: Option<BlockPointer>) {
        self.ir.basic_blocks[self.curr_block].continue_to = continue_to;
    }

    fn add_global_constant(&mut self, ident: &str, value: Vec<u8>) -> VarID {
        let id = self.id_counter;
        self.id_counter += 1;
        self.ir.id_to_ident.insert(id, ident.to_owned());
        self.ir.global_constants.insert(id, value);
        return id;
    }

    fn new_variable(&mut self, size: usize, ident: &str) -> VarID {
        let id = self.new_nameless_variable(size);
        self.ir.id_to_ident.insert(id, ident.to_owned());
        return id;
    }

    fn new_nameless_variable(&mut self, size: usize) -> VarID {
        let id = self.id_counter;
        self.id_counter += 1;
        self.ir.id_to_size.insert(id, size);
        return id;
    }


}


pub enum Function {
    Len,
    Wait,
    Input,
    UpdateScreen,
    RAMWrite,
}

pub fn ident_to_function(ident: &str) -> Option<Function> {
    match ident {
        "len" => Some(Function::Len),
        "wait" => Some(Function::Wait),
        "input" => Some(Function::Input),
        "updateScreen" => Some(Function::UpdateScreen),
        "ramWrite" => Some(Function::RAMWrite),
        _ => None
    }
}

pub struct FunctionSignature {
    num_args: usize,
    return_type: Type,
}

pub fn function_to_signature(function: &Function) -> FunctionSignature {
    match function {
        Function::Len => FunctionSignature {
            num_args: 1,
            return_type: Type { nesting: vec![]}
        },
        Function::Wait => FunctionSignature{
            num_args: 0,
            return_type: Type { nesting: vec![]}
        },
        Function::Input => FunctionSignature {
            num_args: 0,
            return_type: Type { nesting: vec![]}
        },
        Function::UpdateScreen => FunctionSignature{
            num_args: 0,
            return_type: Type { nesting: vec![]}
        },
        Function::RAMWrite => FunctionSignature{
            num_args: 2,
            return_type: Type { nesting: vec![]}
        },
        
    }
}



pub fn gen_program(node: &ProgramNode) -> Result<IntermediateRepresentation, SemanticError> {



    let mut context = Context::new();
    context.new_frame();

    let mut builder = IRBuilder::new();


    for stmt in &node.outer_statements {
        match &stmt.d {
            OuterStatementDetail::DeclarationStatement { r#type, identifier, val } => {
                if context.get_variable_declaration_in_outermost_frame(identifier).is_some() {
                    return Err(SemanticError{loc: node.loc, message: "Re-declaration of constant".to_string()})
                }

                let expected_type = r#type.get_type(&context)?;
                // if !expected_type.is_const {
                //     return Err(SemanticError { loc: r#type.loc, message: "Only const declarations allowed outside main block".to_string() })
                // }

                let actual_type = val.get_type(&context)?;
                if expected_type != actual_type {
                    return Err(SemanticError{loc: val.loc, message: format!("Mismatching types: expected {} got {}", expected_type, actual_type)});
                }

                let id = builder.add_global_constant(&identifier, flatten_constant(val));
                context.add_declaration(identifier, SymbolInfo { id, r#type: expected_type });
            },
        }
    }

    gen_block(&node.main, &mut builder, &mut context)?;

    return Ok(builder.ir);

}

fn gen_block(node: &Box<BlockNode>, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {
    
    context.new_frame();
    for stmt in &node.statements {
        gen_inner_statement(stmt, builder, context)?;
    }
    context.pop_frame();
    return Ok(())
}

fn gen_inner_statement(node: &Box<InnerStatementNode>, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {
    match &node.d {
        InnerStatementDetail::ForeverLoop { body } => {
            let loop_body = builder.new_block();
            let loop_after = builder.new_block();
            context.add_loop(loop_body, loop_after);
            builder.set_continue(Some(loop_body));
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            builder.set_continue(Some(loop_body));
            context.pop_loop();
            builder.set_insert_point_at_block_end(loop_after);
            Ok(())
        },
        InnerStatementDetail::WhileLoop { condition, body } => {
            let loop_condition = builder.new_block();
            let loop_body = builder.new_block();
            let loop_after = builder.new_block();
            context.add_loop(loop_condition, loop_after);
            builder.set_continue(Some(loop_condition));
            builder.set_insert_point_at_block_end(loop_condition);
            gen_condition(condition, builder, context, loop_body, loop_after)?;
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            builder.set_continue(Some(loop_condition));
            context.pop_loop();
            builder.set_insert_point_at_block_end(loop_after);
            Ok(())

        },
        InnerStatementDetail::DoWhileLoop { body, condition } => {
            let loop_body = builder.new_block();
            let loop_condition = builder.new_block();
            let loop_after = builder.new_block();
            context.add_loop(loop_condition, loop_after);
            builder.set_continue(Some(loop_body));
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            builder.set_continue(Some(loop_condition));
            builder.set_insert_point_at_block_end(loop_condition);
            gen_condition(condition, builder, context, loop_body, loop_after)?;
            context.pop_loop();
            builder.set_insert_point_at_block_end(loop_after);
            Ok(())
        },
        InnerStatementDetail::IfStatement { condition, r#true } => {
            let if_body = builder.new_block();
            let if_after = builder.new_block();
            gen_condition(condition, builder, context, if_body, if_after)?;
            builder.set_insert_point_at_block_end(if_body);
            gen_block(r#true, builder, context)?;
            builder.set_continue(Some(if_after));
            builder.set_insert_point_at_block_end(if_after);
            Ok(())
        },
        InnerStatementDetail::IfElseStatement { condition, r#true, r#false } => {
            let if_true_body = builder.new_block();
            let if_false_body = builder.new_block();
            let if_after = builder.new_block();
            gen_condition(condition, builder, context, if_true_body, if_false_body)?;
            builder.set_insert_point_at_block_end(if_true_body);
            gen_block(r#true, builder, context)?;
            builder.set_continue(Some(if_after));
            builder.set_insert_point_at_block_end(if_false_body);
            gen_block(r#false, builder, context)?;
            builder.set_continue(Some(if_after));
            builder.set_insert_point_at_block_end(if_after);
            Ok(())
        },
        InnerStatementDetail::DeclarationStatement { r#type, identifier } => {
            let parsed_type = r#type.get_type(context)?;
            let var_id = builder.new_variable(parsed_type.size(), identifier);
            context.add_declaration(identifier, SymbolInfo { id: var_id, r#type: parsed_type });
            Ok(())
        }
        InnerStatementDetail::DeclarationStatementWithInitialValue { r#type, identifier, val } => {
            let parsed_type = r#type.get_type(context)?;
            let var_id = builder.new_variable(parsed_type.size(), identifier);
            context.add_declaration(identifier, SymbolInfo { id: var_id, r#type: parsed_type.clone() });
            gen_fill_from_expression(val, &parsed_type, None, var_id, builder, context)?;
            Ok(())
        },
        InnerStatementDetail::AssignmentStatement { lvalue, rvalue } => {
            let (var_id, offset, r#type) = gen_get_memory_variable_offset_and_type(lvalue, builder, context)?;
            let dest_ptr = match offset {
                Some(offset) => {
                    builder.insert_statement(Stmt{
                        // offset += location of destination variable
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(offset)),
                        arg2: Some(StmtArg::LocationOfVariable(var_id)),
                        result: Some(offset),
                        loc: lvalue.loc
                    });
                    Some(offset)
                },
                None => None,
            };
            gen_fill_from_expression(rvalue, &r#type, dest_ptr, var_id, builder, context)?;
            Ok(())
        },
        InnerStatementDetail::ExpressionStatement { val } => {
            gen_expression_to_temporary(val, &val.get_type(context)?, builder, context)?;
            Ok(())
        }
        InnerStatementDetail::BreakStatement {  } => {
            let break_block = match context.get_break_block() {
                Some(block) => block,
                None => return Err(SemanticError { loc: node.loc, message: "No loop to break out of".to_string() })
            };
            builder.set_continue(Some(break_block));
            // statements following this are dead code
            // create a new block with no parents - any following statements will go into this and (hopefully) be optimized out
            let dead_code = builder.new_block();
            builder.set_insert_point_at_block_end(dead_code);
            Ok(())
        }
        InnerStatementDetail::ContinueStatement {  } => {
            let continue_block = match context.get_continue_block() {
                Some(block) => block,
                None => return Err(SemanticError { loc: node.loc, message: "No loop to continue to next iteration of".to_string() })
            };
            builder.set_continue(Some(continue_block));
            let dead_code = builder.new_block();
            builder.set_insert_point_at_block_end(dead_code);
            Ok(())
        }
        InnerStatementDetail::Block { body } => {
            gen_block(body, builder, context)?;
            Ok(())
        }
    }
}

fn gen_condition(condition: &Box<ConditionNode>, builder: &mut IRBuilder, context: &mut Context, block_if_true: BlockPointer, block_if_false: BlockPointer) -> Result<(), SemanticError> {
    match &condition.d {
        ConditionDetail::Branch { flag } => {
            builder.set_branch(Some(Branch { flag: *flag, to: block_if_true }));
            builder.set_continue(Some(block_if_false));
            Ok(())
        },
        ConditionDetail::Expression { val } => {
            gen_expression_as_boolean(val, block_if_true, block_if_false, builder, context)?;
            Ok(())
        }
    }
}

fn gen_fill_from_expression(node: &Box<ExpressionNode>, dest_type: &Type, dest_ptr: Option<VarID>, dest: VarID, builder: &mut IRBuilder, context: &mut Context ) -> Result<(), SemanticError> {

    match &node.d {

        ExpressionDetail::Array { val: children } => {

            // filling an array
            // source and destination arrays may be of different types
            // but as long as the depth is the same it's fine
            // we just skip indeces in the source or destination arrays when necessary

            let actual_type = node.get_type(context)?;
            if actual_type.depth() != dest_type.depth() {
                return Err(SemanticError { loc: node.loc, message: format!("Expected type {}, got {}", dest_type, actual_type) });
            }

            // create a pointer if necessary
            let ptr = match dest_ptr {
                Some(ptr) => Some(ptr),
                None => if actual_type.size() == 1 {
                    None
                } else {
                    let ptr = builder.new_nameless_variable(1);
                    builder.insert_statement(Stmt {
                        // ptr = loc of dest
                        kind: StmtKind::Unary(Unary::Copy),
                        arg1: Some(StmtArg::LocationOfVariable(dest)),
                        arg2: None,
                        result: Some(ptr),
                        loc: node.loc
                    });
                    Some(ptr)
                }
            };


            let dest_inner_type = dest_type.inner().unwrap();
            
            if actual_type.nesting.first().unwrap() >= dest_type.nesting.first().unwrap() {
                // exactly correct, or too many values to fit in the destination. ignore the out of bounds ones.
                for child in &children[0..*dest_type.nesting.first().unwrap()] {
                    gen_fill_from_expression(child, &dest_inner_type, ptr, dest, builder, context)?;
                    match ptr {
                        None => (),
                        Some(ptr) => builder.insert_statement(Stmt{
                            // ptr++
                            kind: StmtKind::Unary(Unary::Increment),
                            arg1: Some(StmtArg::Variable(ptr)),
                            arg2: None,
                            result: Some(ptr),
                            loc: child.loc
                        }),
                    }
                }
            } else {
                // too few values to fit in the destination.
                // we may be inside a nested array definition, so increase the pointer at the end so we stay in line with the flattened array.
                if children.len() != 0 {
                    for (i, child) in enumerate(children) {
                        gen_fill_from_expression(child, &dest_inner_type, dest_ptr, dest, builder, context)?;
                        // don't increment the pointer in the last iteration
                        if i < children.len() - 1 {
                            match ptr {
                                None => (),
                                Some(ptr) => builder.insert_statement(Stmt{
                                    // ptr++
                                    kind: StmtKind::Unary(Unary::Increment),
                                    arg1: Some(StmtArg::Variable(ptr)),
                                    arg2: None,
                                    result: Some(ptr),
                                    loc: child.loc
                                }),
                            }
                        }
                    }
                    let pointer_increase_amount = (dest_type.nesting.first().unwrap() - children.len()) * dest_inner_type.size() + 1;
                    match ptr {
                        None => (),
                        Some(ptr) => builder.insert_statement(Stmt{
                            // ptr += pointer_increase_amount
                            kind: StmtKind::Binary(Binary::Add),
                            arg1: Some(StmtArg::Variable(ptr)),
                            arg2: Some(StmtArg::Literal(pointer_increase_amount as u8)),
                            result: Some(ptr),
                            loc: node.loc
                        }),
                    }
                }
            }

            Ok(())
            
        },
        ExpressionDetail::FunctionCall { ident, args } => {
            gen_fill_from_function_call(ident, args, dest_type, dest_ptr, dest, node.loc, builder, context)
        },

        ExpressionDetail::MemoryValue { val } => {
            gen_fill_from_memory_value(val, dest, None, dest_ptr, dest_type, builder, context)?;
            Ok(())
        },

        ExpressionDetail::Number { val:_ } |
        ExpressionDetail::LeftShift { val:_ } |
        ExpressionDetail::RightShift { val:_ } |
        ExpressionDetail::BitwiseNOT { val:_ } |
        ExpressionDetail::LogicalNOT { val:_ } |
        ExpressionDetail::Log2 { val:_ } |
        ExpressionDetail::Add { left:_, right:_ } |
        ExpressionDetail::Subtract { left:_, right:_ } |
        ExpressionDetail::Multiply { left:_, right:_ } |
        ExpressionDetail::Divide { left:_, right:_ } |
        ExpressionDetail::Modulo { left:_, right:_ } |
        ExpressionDetail::BitwiseXOR { left:_, right:_ } |
        ExpressionDetail::BitwiseAND { left:_, right:_ } |
        ExpressionDetail::BitwiseOR { left:_, right:_ } |
        ExpressionDetail::EqualTo { left:_, right:_ } |
        ExpressionDetail::LogicalOR { left:_, right:_ } |
        ExpressionDetail::LogicalAND { left:_, right:_ } |
        ExpressionDetail::SignedGreaterThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::SignedLessThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::SignedLessThan { left:_, right:_ } |
        ExpressionDetail::SignedGreaterThan { left:_, right:_ } |
        ExpressionDetail::UnsignedGreaterThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::UnsignedLessThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::UnsignedLessThan { left:_, right:_ } |
        ExpressionDetail::UnsignedGreaterThan { left:_, right:_ } => {
            // all these always yield a BYTE.
            // write to temporary then copy into the array
            if *dest_type != BYTE {
                return Err(SemanticError { loc: node.loc, message: format!("Expected a {}, got byte", dest_type) });
            }
            let t = gen_expression_to_temporary(node, &BYTE, builder, context)?;
            match dest_ptr {
                Some(ptr) => {
                    builder.insert_statement(Stmt {
                        // [ptr] = t
                        loc: node.loc,
                        kind: StmtKind::Binary(Binary::RAMWrite),
                        arg1: Some(StmtArg::Variable(ptr)),
                        arg2: Some(StmtArg::Variable(t)),
                        result: Some(dest)
                    });
                },
                None => {
                    builder.insert_statement(Stmt{
                        // dest = t
                        kind: StmtKind::Unary(Unary::Copy),
                        arg1: Some(StmtArg::Variable(t)),
                        arg2: None,
                        result: Some(dest),
                        loc: node.loc
                    });
                }
            }
            Ok(())
        }
    }
}

fn gen_expression_to_temporary(node: &Box<ExpressionNode>, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context ) -> Result<VarID, SemanticError> {
    match &node.d {
        ExpressionDetail::Number { val } => {
            exact_type_check(node.loc, expected_type, &BYTE)?;
            let out = builder.new_nameless_variable(1);
            builder.insert_statement(Stmt{
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(val.val)),
                arg2: None,
                loc: node.loc,
                result: Some(out),
            });
            Ok(out)
        },
        ExpressionDetail::Array { val:_ } => {
            let r#type = node.get_type(context)?;
            let var_id = builder.new_nameless_variable(r#type.size());
            gen_fill_from_expression(node, &r#type, None, var_id, builder, context)?;
            Ok(var_id)
        }
        ExpressionDetail::FunctionCall { ident, args } => {
            gen_function_call_to_temporary(ident, args, expected_type, node.loc, builder, context)
        },
        ExpressionDetail::MemoryValue { val } => {
            gen_get_memory_value(val, expected_type, builder, context)
        },
        ExpressionDetail::LeftShift { val } => {
            gen_apply_unary(val, node.loc, Unary::LeftShift, expected_type, builder, context)
        },
        ExpressionDetail::RightShift { val } => {
            gen_apply_unary(val, node.loc, Unary::RightShift, expected_type, builder, context)
        },
        ExpressionDetail::BitwiseNOT { val } => {
            gen_apply_unary(val, node.loc, Unary::BitwiseNOT, expected_type, builder, context)
        },
        ExpressionDetail::Log2 { val } => {
            gen_apply_unary(val, node.loc, Unary::Log2, expected_type, builder, context)
        }
        ExpressionDetail::Add { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::Add, expected_type, builder, context)
        }
        ExpressionDetail::Subtract { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::Subtract, expected_type, builder, context)
        }
        ExpressionDetail::Multiply { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::Multiply, expected_type, builder, context)
        }
        ExpressionDetail::Divide { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::Divide, expected_type, builder, context)
        }
        ExpressionDetail::Modulo { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::Modulo, expected_type, builder, context)
        }
        ExpressionDetail::BitwiseXOR { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::BitwiseXOR, expected_type, builder, context)
        }
        ExpressionDetail::BitwiseAND { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::BitwiseAND, expected_type, builder, context)
        }
        ExpressionDetail::BitwiseOR { left, right } => {
            gen_apply_binary(left, right, node.loc, Binary::BitwiseOR, expected_type, builder, context)
        }
        ExpressionDetail::LogicalNOT { val:_ } |
        ExpressionDetail::LogicalOR { left:_, right:_ } |
        ExpressionDetail::LogicalAND { left:_, right:_ } |
        ExpressionDetail::EqualTo { left:_, right:_ } |
        ExpressionDetail::SignedGreaterThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::SignedLessThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::SignedLessThan { left:_, right:_ } |
        ExpressionDetail::SignedGreaterThan { left:_, right:_ } |
        ExpressionDetail::UnsignedGreaterThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::UnsignedLessThanOrEqualTo { left:_, right:_ } |
        ExpressionDetail::UnsignedLessThan { left:_, right:_ } |
        ExpressionDetail::UnsignedGreaterThan { left:_, right:_ } => {
            // convert bool into 1 or 0
            let block_if_true = builder.new_block();
            let block_if_false = builder.new_block();
            let block_after = builder.new_block();
            let result = builder.new_nameless_variable(1);
            gen_expression_as_boolean(node, block_if_true, block_if_false, builder, context)?;
            builder.set_insert_point_at_block_end(block_if_true);
            builder.insert_statement(Stmt{
                // result = 1
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(1)),
                arg2: None,
                result: Some(result),
                loc: node.loc
            });
            builder.set_continue(Some(block_after));
            builder.set_insert_point_at_block_end(block_if_false);
            builder.insert_statement(Stmt{
                // result = 0
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(0)),
                arg2: None,
                result: Some(result),
                loc: node.loc
            });
            builder.set_continue(Some(block_after));
            builder.set_insert_point_at_block_end(block_after);
            Ok(result)
        }
    }
}

fn gen_apply_unary(node: &Box<ExpressionNode>, loc: CodeLocation, op: Unary, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
    exact_type_check(loc, expected_type, &BYTE)?;
    let inner_result = gen_expression_to_temporary(node, &BYTE, builder, context)?;
    let out = builder.new_nameless_variable(1);
    builder.insert_statement(Stmt{
        kind: StmtKind::Unary(op),
        arg1: Some(StmtArg::Variable(inner_result)),
        arg2: None,
        loc: loc,
        result: Some(out),
    });
    Ok(out)
}

fn gen_apply_binary(left: &Box<ExpressionNode>, right: &Box<ExpressionNode>, loc: CodeLocation, op: Binary, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
    exact_type_check(loc, expected_type, &BYTE)?;
    let left_result = gen_expression_to_temporary(left, &BYTE, builder, context)?;
    let right_result = gen_expression_to_temporary(right, &BYTE, builder, context)?;
    let out = builder.new_nameless_variable(1);
    builder.insert_statement(Stmt{
        kind: StmtKind::Binary(op),
        arg1: Some(StmtArg::Variable(left_result)),
        arg2: Some(StmtArg::Variable(right_result)),
        loc: loc,
        result: Some(out),
    });
    Ok(out)
}

fn gen_expression_as_boolean(node: &Box<ExpressionNode>, block_if_true: BlockPointer, block_if_false: BlockPointer, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {
    let r#type = node.get_type(context)?;
    if r#type.nesting.len() != 0 {
        return Err(SemanticError { loc: node.loc, message: format!("Cannot evaluate value of type {} as boolean", r#type) });
    }
    match &node.d {
        ExpressionDetail::Array { val:_ } => {
            Err(SemanticError { loc: node.loc, message: format!("Cannot evaluate value of type {} as boolean", r#type) })
        }

        ExpressionDetail::Number { val:_ } |
        ExpressionDetail::FunctionCall { ident:_, args:_ } |
        ExpressionDetail::MemoryValue { val:_ } |
        ExpressionDetail::LeftShift { val:_ } |
        ExpressionDetail::RightShift { val:_ } |
        ExpressionDetail::BitwiseNOT { val:_ } |
        ExpressionDetail::Log2 { val:_ } |
        ExpressionDetail::Add { left:_, right:_ } |
        ExpressionDetail::Subtract { left:_, right:_ } |
        ExpressionDetail::Multiply { left:_, right:_ } |
        ExpressionDetail::Divide { left:_, right:_ } |
        ExpressionDetail::Modulo { left:_, right:_ } |
        ExpressionDetail::BitwiseXOR { left:_, right:_ } |
        ExpressionDetail::BitwiseAND { left:_, right:_ } |
        ExpressionDetail::BitwiseOR { left:_, right:_ } => {
            // test if result is equal to zero
            let result = gen_expression_to_temporary(node, &BYTE, builder, context)?;
            builder.insert_statement(Stmt{
                // do nothing with result - just set ALU flags.
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Variable(result)),
                arg2: None,
                result: None,
                loc: node.loc
            });
            builder.set_branch(Some(Branch { flag: BranchFlag::BNZ, to: block_if_true }));
            builder.set_continue(Some(block_if_false));
            Ok(())
        }
        
        ExpressionDetail::EqualTo { left, right } => {
            todo!() // needs to compare entire arrays.
        }

        ExpressionDetail::SignedGreaterThanOrEqualTo { left, right } |
        ExpressionDetail::SignedLessThanOrEqualTo { left, right }|
        ExpressionDetail::SignedLessThan { left, right } |
        ExpressionDetail::SignedGreaterThan { left, right } |
        ExpressionDetail::UnsignedGreaterThanOrEqualTo { left, right } |
        ExpressionDetail::UnsignedLessThanOrEqualTo { left, right } |
        ExpressionDetail::UnsignedLessThan { left, right } |
        ExpressionDetail::UnsignedGreaterThan { left, right } => {
            let left_eval = gen_expression_to_temporary(left, &BYTE, builder, context)?;
            let right_eval = gen_expression_to_temporary(right, &BYTE, builder, context)?;
            let (do_l_minus_r, flag_if_true) = match &node.d {
                ExpressionDetail::SignedGreaterThanOrEqualTo { left:_, right:_ } => (true, BranchFlag::BGTE),
                ExpressionDetail::SignedLessThanOrEqualTo { left:_, right:_ } => (false, BranchFlag::BGTE),
                ExpressionDetail::SignedLessThan { left:_, right:_ } => (true, BranchFlag::BNGTE),
                ExpressionDetail::SignedGreaterThan { left:_, right:_ } => (false, BranchFlag::BNGTE),
                ExpressionDetail::UnsignedGreaterThanOrEqualTo { left:_, right:_ } => (true, BranchFlag::BC),
                ExpressionDetail::UnsignedLessThanOrEqualTo { left:_, right:_ } => (false, BranchFlag::BC),
                ExpressionDetail::UnsignedLessThan { left:_, right:_ } => (true, BranchFlag::BNC),
                ExpressionDetail::UnsignedGreaterThan { left:_, right:_ } => (false, BranchFlag::BNC),
                _ => panic!()
            };
            builder.insert_statement(Stmt {
                kind: StmtKind::Binary(Binary::Subtract),
                arg1: Some(StmtArg::Variable(if do_l_minus_r {left_eval} else {right_eval})),
                arg2: Some(StmtArg::Variable(if do_l_minus_r {right_eval} else {left_eval})),
                result: None,
                loc: node.loc
            });
            builder.set_branch(Some(Branch { flag: flag_if_true, to: block_if_true }));
            builder.set_continue(Some(block_if_false));
            Ok(())
        }
        ExpressionDetail::LogicalNOT { val } => {
            gen_expression_as_boolean(val, block_if_false, block_if_true, builder, context)
        }
        ExpressionDetail::LogicalOR { left, right } => {
            let test_right_block = builder.new_block();
            gen_expression_as_boolean(left, block_if_true, test_right_block, builder, context)?;
            builder.set_insert_point_at_block_end(test_right_block);
            gen_expression_as_boolean(right, block_if_true, block_if_false, builder, context)?;
            Ok(())
        }
        ExpressionDetail::LogicalAND { left, right } => {
            let test_right_block = builder.new_block();
            gen_expression_as_boolean(left, test_right_block, block_if_false, builder, context)?;
            builder.set_insert_point_at_block_end(test_right_block);
            gen_expression_as_boolean(right, block_if_true, block_if_false, builder, context)?;
            Ok(())
        }
    }
}

/**
 * pointer_for_array is the offset at which it starts writing values.
 * expected_type refers to the type of the data that is being written (not necessarily that of the entire destination variable, if there is an offset)
 */
fn gen_fill_from_memory_value(node: &Box<MemoryLocationNode>, destination: VarID, src_offset: Option<VarID>, dest_ptr: Option<VarID>, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {
    let actual_type = node.get_type(context)?;
    if actual_type.depth() != expected_type.depth() {
        return Err(SemanticError { loc: node.loc, message: format!("Expected a type of depth {}, got {} (depth {})", expected_type.depth(), actual_type, actual_type.depth()) });
    }
    match &node.d {
        MemoryLocationDetail::Identifier { val } => {
            match context.get_variable_declaration(&val) {
                None => Err(SemanticError{loc: node.loc, message: "Variable used before declaration".to_string()}),
                Some(info) => {
                    let src_ptr = match src_offset {
                        Some(offset) => {
                            builder.insert_statement(Stmt{
                                // offset += start of source array
                                kind: StmtKind::Binary(Binary::Add),
                                arg1: Some(StmtArg::Variable(offset)),
                                arg2: Some(StmtArg::LocationOfVariable(info.id)),
                                result: Some(offset),
                                loc: node.loc
                            });
                            Some(offset)
                        },
                        None => None
                    };
                    gen_copy(info.id, destination, src_ptr, dest_ptr, &actual_type, expected_type, node.loc, builder, context)
                }
            }
        },        
        MemoryLocationDetail::ArraySlice { start:i, arr } |
        MemoryLocationDetail::ArrayIndex { i, arr } => {
            // use same code for both slice and index.
            // because this code block is just calculating the offset of where to start copying items - which is the same for both

            // unwraps won't cause panic because actual_type was already able to be calculated
            // meaning it was already deemed valid to index into arr
            let inner_type = arr.get_type(context)?.inner().unwrap();
            let offset_from_this_layer = gen_expression_to_temporary(i, &BYTE, builder, context)?;
            builder.insert_statement(Stmt{
                // offset_from_this_layer *= inner_type.size()
                kind: StmtKind::Binary(Binary::Multiply),
                arg1: Some(StmtArg::Variable(offset_from_this_layer)),
                arg2: Some(StmtArg::Literal(inner_type.size() as u8)),
                result: Some(offset_from_this_layer),
                loc: node.loc
            });
            let offset = match src_offset {
                Some(offset) => {
                    builder.insert_statement(Stmt{
                        // offset += offset_from_this_layer
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(offset)),
                        arg2: Some(StmtArg::Variable(offset_from_this_layer)),
                        result: Some(offset),
                        loc: node.loc
                    });
                    offset
                },
                None => {
                    offset_from_this_layer
                }
            };

            gen_fill_from_memory_value(arr, destination, Some(offset), dest_ptr, expected_type, builder, context)
        },
    }

    
}

// creates a new temporary variable and copies the value indicated by the MemoryLocationNode into it.
fn gen_get_memory_value(node: &Box<MemoryLocationNode>, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
    let actual_type = node.get_type(context)?;
    if actual_type.depth() != expected_type.depth() {
        return Err(SemanticError { loc: node.loc, message: format!("Expected a type of depth {}, got {} (depth {})", expected_type.depth(), actual_type, actual_type.depth()) });
    }
    let (source, offset, r#type) = gen_get_memory_variable_offset_and_type(node, builder, context)?;
    let dest = builder.new_nameless_variable(expected_type.size());
    gen_copy(source, dest, offset, None, &r#type, expected_type, node.loc, builder, context)?;
    Ok(dest)
}

fn gen_get_memory_variable_offset_and_type(node: &Box<MemoryLocationNode>, builder: &mut IRBuilder, context: &mut Context) -> Result<(VarID, Option<VarID>, Type), SemanticError> {
    match &node.d {
        MemoryLocationDetail::Identifier { val } => {
            match context.get_variable_declaration(val) {
                Some(info) => Ok((info.id, None, info.r#type)),
                None => Err(SemanticError { loc: node.loc, message: "Variable used before declaration".to_string() }),
            }
        },
        MemoryLocationDetail::ArrayIndex { i, arr } => {
            let (var_id, maybe_offset, r#type) = gen_get_memory_variable_offset_and_type(arr, builder, context)?;
            let inner_type = match r#type.inner() {
                Some(inner) => inner,
                None => return Err(SemanticError { loc: node.loc, message: "Cannot index into byte".to_string() })
            };
            let i_eval = gen_expression_to_temporary(i, &BYTE, builder, context)?;
            builder.insert_statement(Stmt{
                // i *= inner_type.size()
                kind: StmtKind::Binary(Binary::Multiply),
                arg1: Some(StmtArg::Variable(i_eval)),
                arg2: Some(StmtArg::Literal(inner_type.size() as u8)),
                result: Some(i_eval),
                loc: node.loc
            });
            let offset = match maybe_offset {
                Some(offset) => {
                    builder.insert_statement(Stmt {
                        // offset += i
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(offset)),
                        arg2: Some(StmtArg::Variable(i_eval)),
                        result: Some(offset),
                        loc: node.loc
                    });
                    offset
                },
                None => {
                    // offset = i
                    i_eval
                },
            };
            Ok((var_id, Some(offset), inner_type))
        }
        MemoryLocationDetail::ArraySlice { start, arr } => {
            // same as array index except return the whole type instead of the inner type
            let (var_id, maybe_offset, r#type) = gen_get_memory_variable_offset_and_type(arr, builder, context)?;
            let inner_type = match r#type.inner() {
                Some(inner) => inner,
                None => return Err(SemanticError { loc: node.loc, message: "Cannot slice byte".to_string() })
            };
            let start_eval = gen_expression_to_temporary(start, &BYTE, builder, context)?;
            builder.insert_statement(Stmt{
                // start *= inner_type.size()
                kind: StmtKind::Binary(Binary::Multiply),
                arg1: Some(StmtArg::Variable(start_eval)),
                arg2: Some(StmtArg::Literal(inner_type.size() as u8)),
                result: Some(start_eval),
                loc: node.loc
            });
            let offset = match maybe_offset {
                Some(offset) => {
                    builder.insert_statement(Stmt {
                        // offset += start
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(offset)),
                        arg2: Some(StmtArg::Variable(start_eval)),
                        result: Some(offset),
                        loc: node.loc
                    });
                    offset
                },
                None => {
                    // offset = start
                    start_eval
                },
            };
            Ok((var_id, Some(offset), r#type))
        }
    }
}

fn gen_copy(source: VarID, dest:VarID, source_ptr: Option<VarID>, dest_ptr: Option<VarID>, source_type: &Type, dest_type: &Type, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

    if *dest_type == BYTE {
        // copy single byte
        if *source_type != BYTE {
            return Err(SemanticError { loc, message: format!("Expected byte, got {}", source_type) })
        }
        match (source_ptr, dest_ptr) {
            (None, None) => {
                builder.insert_statement(Stmt{
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::Variable(source)),
                    arg2: None,
                    result: Some(dest),
                    loc
                });
                Ok(())
            },
            (None, Some(d_ptr)) => {
                builder.insert_statement(Stmt{
                    kind: StmtKind::Binary(Binary::RAMWrite),
                    arg1: Some(StmtArg::Variable(d_ptr)),
                    arg2: Some(StmtArg::Variable(source)),
                    result: Some(dest),
                    loc
                });
                Ok(())
            },
            (Some(s_ptr), None) => {
                builder.insert_statement(Stmt{
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::RAMPointer(s_ptr, source)),
                    arg2: None,
                    result: Some(dest),
                    loc
                });
                Ok(())
            },
            (Some(s_ptr), Some(d_ptr)) => {
                builder.insert_statement(Stmt{
                    kind: StmtKind::Binary(Binary::RAMWrite),
                    arg1: Some(StmtArg::Variable(d_ptr)),
                    arg2: Some(StmtArg::RAMPointer(s_ptr, source)),
                    result: Some(dest),
                    loc
                });
                Ok(())
            }
        }
        
    } else /* one or more of the args is an array */ {
        match (source_ptr, dest_ptr) {
            // this section is probably inefficient if the arrays only have a length of 1 but gonna leave it
            (None, None) => {
                gen_copy_array(source, dest, source_type, dest_type, loc, builder, context)
            },
            (None, Some(d_ptr)) => {
                let s_ptr = builder.new_nameless_variable(1);
                builder.insert_statement(Stmt{
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::LocationOfVariable(source)),
                    arg2: None,
                    result: Some(s_ptr),
                    loc
                });
                gen_copy_array_with_pointer_offsets(source, dest, s_ptr, d_ptr, source_type, dest_type, loc, builder, context)
            }
            (Some(s_ptr), None) => {
                let d_ptr = builder.new_nameless_variable(1);
                builder.insert_statement(Stmt{
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::LocationOfVariable(dest)),
                    arg2: None,
                    result: Some(d_ptr),
                    loc
                });
                gen_copy_array_with_pointer_offsets(source, dest, s_ptr, d_ptr, source_type, dest_type, loc, builder, context)
            },
            (Some(s_ptr), Some(d_ptr)) => {
                gen_copy_array_with_pointer_offsets(source, dest, s_ptr, d_ptr, source_type, dest_type, loc, builder, context)
            }
        }
    }


}

fn gen_copy_array(source: VarID, dest: VarID, source_type: &Type, dest_type: &Type, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {
    if source_type.depth() == 0 && dest_type.depth() == 0 {
        // byte
        builder.insert_statement(Stmt {
            kind: StmtKind::Unary(Unary::Copy),
            arg1: Some(StmtArg::Variable(source)),
            arg2: None,
            result: Some(dest),
            loc
        });
        Ok(())
    } else {
        // array copy
        let source_ptr = builder.new_nameless_variable(1);
        let dest_ptr = builder.new_nameless_variable(1);
        builder.insert_statement(Stmt {
            // src_ptr = start of source array
            kind: StmtKind::Unary(Unary::Copy),
            arg1: Some(StmtArg::LocationOfVariable(source)),
            arg2: None,
            result: Some(source_ptr),
            loc: loc
        });
        builder.insert_statement(Stmt {
            // dest_ptr = start of dest array
            kind: StmtKind::Unary(Unary::Copy),
            arg1: Some(StmtArg::LocationOfVariable(dest)),
            arg2: None,
            result: Some(dest_ptr),
            loc: loc
        });
        gen_copy_array_with_pointer_offsets(source, dest, source_ptr, dest_ptr, source_type, dest_type, loc, builder, context)?;
        Ok(())
    }
}

fn gen_copy_array_with_pointer_offsets(source: VarID, dest: VarID, source_ptr: VarID, dest_ptr: VarID, source_type: &Type, dest_type: &Type, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

    

    if source_type.depth() != dest_type.depth() {
        return Err(SemanticError { loc: loc, message: format!("Expected an array of depth {}, got {} (depth {})", dest_type.depth(), source_type, source_type.depth()) });
    }
    gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, source_type, dest_type, true, loc, builder, context)

}

fn gen_copy_array_with_pointer_offsets_2(source: VarID, dest: VarID, source_ptr: VarID, dest_ptr: VarID, source_type: &Type, dest_type: &Type, is_outer_layer: bool, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

    // pseudocode:
    /*
    copy_array(e_type, a_type, dest_ptr, src_ptr) {

        // note, if the generated loop is only to run once, then we can use one of the pointers in the loop condition rather than creating a new temp variable

        if e_type.depth() === 0 {
            gen copy src to dest at ptrs,
            inc both pointers
            return
        }
        
        chunk_count = e_type[0] * e_type[1] * ... * e_type[i-1]
            where l is the lowest index such that e_type[l] != a_type[l]
            
        if l == 0 {
            let min = min(e_type[0], a_type[0]);
            if min == 1 {
                copy_array(e_type.strip_layers(1), a_type.strip_layers(1), dest_ptr, src_ptr)
            } else {
                gen I = 0
                L1: copy_array(e_type.strip_layers(1), a_type.strip_layers(1), dest_ptr, src_ptr)
                gen I ++
                gen if I < min(e_type[0], a_type[0]) goto L1
            }
            gen dest_ptr += e_type.strip_layers(1).size() * (e_type[0] - min(e_type[0], a_type[0]))
            gen src_ptr += a_type.strip_layers(1).size() * (a_type[0] - min(e_type[0], a_type[0]))

        } else {
            if chunk_count = 1 {
                copy_array(e_type.strip_layers(l), a_type.strip_layers(l), dest_ptr, src_ptr)
            } else {
                gen I = 0
                L1: copy_array(e_type.strip_layers(l), a_type.strip_layers(l), dest_ptr, src_ptr)
                gen if I < chunk_count goto L1
            }

        }
    } */

    if dest_type.depth() == 0 {
        // trivial copy
        let t = builder.new_nameless_variable(1);
        builder.insert_statement(Stmt {
            // copy from source to temporary
            kind: StmtKind::Unary(Unary::Copy),
            arg1: Some(StmtArg::RAMPointer(source_ptr, source)),
            arg2: None,
            result: Some(t),
            loc
        });
        builder.insert_statement(Stmt {
            // copy from temporary to destination
            kind: StmtKind::Binary(Binary::RAMWrite),
            arg1: Some(StmtArg::Variable(dest_ptr)),
            arg2: Some(StmtArg::Variable(t)),
            result: Some(dest),
            loc
        });
        builder.insert_statement(Stmt{
            // increment source_ptr
            kind: StmtKind::Unary(Unary::Increment),
            arg1: Some(StmtArg::Variable(source_ptr)),
            arg2: None,
            result: Some(source_ptr),
            loc
        });
        builder.insert_statement(Stmt{
            // increment dest_ptr
            kind: StmtKind::Unary(Unary::Increment),
            arg1: Some(StmtArg::Variable(dest_ptr)),
            arg2: None,
            result: Some(dest_ptr),
            loc
        });
        return Ok(())
    }

    // find lowest depth such that number of elements is different:
    let mut l = 0;
    let mut chunk_count = 1;
    while l < dest_type.depth() && dest_type.nesting[l] == source_type.nesting[l]{
        l += 1;
        chunk_count *= dest_type.nesting[l];
    }

    if l == 0 {
        // outermost layer of array has differing number of elements
        let els_in_outermost = min(dest_type.nesting[0], source_type.nesting[0]);
        // copy this number of elements.
        if els_in_outermost == 1 {
            // only copying a single element.
            gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;

            if !is_outer_layer {
                // mop up
                let increase_source_ptr_by = source_type.inner().unwrap().size() * (source_type.nesting[0] - els_in_outermost);
                let increase_dest_ptr_by = dest_type.inner().unwrap().size() * (dest_type.nesting[0] - els_in_outermost);
                if increase_source_ptr_by > 0 {
                    builder.insert_statement(Stmt {
                        // source_ptr += increase_source_ptr_by
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(source_ptr)),
                        arg2: Some(StmtArg::Literal(increase_source_ptr_by as u8)),
                        result: Some(source_ptr),
                        loc
                    });
                }
                if increase_dest_ptr_by > 0 {
                    builder.insert_statement(Stmt {
                        // dest_ptr += increase_dest_ptr_by
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(dest_ptr)),
                        arg2: Some(StmtArg::Literal(increase_dest_ptr_by as u8)),
                        result: Some(dest_ptr),
                        loc
                    });
                }
            }
        } else {
            // copying more than 1 element
            if is_outer_layer {
                // use dest_ptr instead of creating a temporary
                let loop_start = builder.new_block();
                let loop_after = builder.new_block();
                builder.set_insert_point_at_block_end(loop_start);
                gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;
                let stop_when_dest_ptr_reaches = dest_ptr + els_in_outermost * dest_type.inner().unwrap().size();
                builder.insert_statement(Stmt {
                    // if dest_ptr < stop_when_dest_ptr_reaches goto loop_start
                    kind: StmtKind::Binary(Binary::Subtract),
                    arg1: Some(StmtArg::Variable(dest_ptr)),
                    arg2: Some(StmtArg::Literal(stop_when_dest_ptr_reaches as u8)),
                    result: None,
                    loc
                });
                builder.set_branch(Some(Branch { flag: BranchFlag::BNC, to: loop_start }));
                builder.set_continue(Some(loop_after));
                builder.set_insert_point_at_block_end(loop_after);
            } else {
                let i = builder.new_nameless_variable(1);
                builder.insert_statement(Stmt {
                    // i = 0
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::Literal(0)),
                    arg2: None,
                    result: Some(i),
                    loc
                });
                let loop_start = builder.new_block();
                let loop_after = builder.new_block();
                builder.set_insert_point_at_block_end(loop_start);
                gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;
                builder.insert_statement(Stmt {
                    // i++
                    kind: StmtKind::Unary(Unary::Increment),
                    arg1: Some(StmtArg::Variable(i)),
                    arg2: None,
                    result: Some(i),
                    loc
                });
                builder.insert_statement(Stmt {
                    // if i < min(e_type[0], a_type[0]) goto loop_start
                    kind: StmtKind::Binary(Binary::Subtract),
                    arg1: Some(StmtArg::Variable(i)),
                    arg2: Some(StmtArg::Literal(els_in_outermost as u8)),
                    result: None,
                    loc
                });
                builder.set_branch(Some(Branch { flag: BranchFlag::BNC, to: loop_start }));
                builder.set_continue(Some(loop_after));
                builder.set_insert_point_at_block_end(loop_after);
                // mop up
                let increase_source_ptr_by = source_type.inner().unwrap().size() * (source_type.nesting[0] - els_in_outermost);
                let increase_dest_ptr_by = dest_type.inner().unwrap().size() * (dest_type.nesting[0] - els_in_outermost);
                if increase_source_ptr_by > 0 {
                    builder.insert_statement(Stmt {
                        // source_ptr += increase_source_ptr_by
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(source_ptr)),
                        arg2: Some(StmtArg::Literal(increase_source_ptr_by as u8)),
                        result: Some(source_ptr),
                        loc
                    });
                }
                if increase_dest_ptr_by > 0 {
                    builder.insert_statement(Stmt {
                        // dest_ptr += increase_dest_ptr_by
                        kind: StmtKind::Binary(Binary::Add),
                        arg1: Some(StmtArg::Variable(dest_ptr)),
                        arg2: Some(StmtArg::Literal(increase_dest_ptr_by as u8)),
                        result: Some(dest_ptr),
                        loc
                    });
                }


            }

        }
    } else /*l >= 1, meaning that the next l layers have the same nesting and can be stepped through with a single pointer, and no mop up needed */ {
        if chunk_count == 1 /*only 1 element to copy */ {
            gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?; 
        } else {
            if is_outer_layer {
                // use a test on dest_ptr instead of creating a new temporary
                let loop_start = builder.new_block();
                let loop_after = builder.new_block();
                builder.set_insert_point_at_block_end(loop_start);
                gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?; 
                let stop_when_dest_ptr_reaches = dest_ptr + dest_type.size();
                builder.insert_statement(Stmt {
                    // if dest_ptr < stop_when_dest_ptr_reaches goto loop_start
                    kind: StmtKind::Binary(Binary::Subtract),
                    arg1: Some(StmtArg::Variable(dest_ptr)),
                    arg2: Some(StmtArg::Literal(stop_when_dest_ptr_reaches as u8)),
                    result: None,
                    loc
                });
                builder.set_branch(Some(Branch { flag: BranchFlag::BNC, to: loop_start }));
                builder.set_continue(Some(loop_after));
                builder.set_insert_point_at_block_end(loop_after);
            } else {
                let i = builder.new_nameless_variable(1);
                builder.insert_statement(Stmt {
                    // i = 0
                    kind: StmtKind::Unary(Unary::Copy),
                    arg1: Some(StmtArg::Literal(0)),
                    arg2: None,
                    result: Some(i),
                    loc
                });
                let loop_start = builder.new_block();
                let loop_after = builder.new_block();
                builder.set_insert_point_at_block_end(loop_start);
                gen_copy_array_with_pointer_offsets_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?;
                builder.insert_statement(Stmt {
                    // i++
                    kind: StmtKind::Unary(Unary::Increment),
                    arg1: Some(StmtArg::Variable(i)),
                    arg2: None,
                    result: Some(i),
                    loc
                });
                builder.insert_statement(Stmt {
                    // if i < chunk_count goto loop_start
                    kind: StmtKind::Binary(Binary::Subtract),
                    arg1: Some(StmtArg::Variable(i)),
                    arg2: Some(StmtArg::Literal(chunk_count as u8)),
                    result: None,
                    loc
                });
                builder.set_branch(Some(Branch { flag: BranchFlag::BNC, to: loop_start }));
                builder.set_continue(Some(loop_after));
                builder.set_insert_point_at_block_end(loop_after);
            }


        }
    }


    Ok(())
}

fn gen_fill_from_function_call(function_ident: &String, args: &Vec<Box<ExpressionNode>>, dest_type: &Type, dest_ptr: Option<VarID>, dest: VarID, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

    // eval to temporary then copy the result.
    // this is fine because all functions currently return a single BYTE
    // - not wasting time since copy propagation can deal with it

    let result = gen_function_call_to_temporary(function_ident, args, dest_type, loc, builder, context)?;
    // ident_to_function(..).unwrap() is fine because gen_function_call_to_temporary has already verified that the function exists
    gen_copy(result, dest, None, dest_ptr, &function_to_signature(&ident_to_function(function_ident).unwrap()).return_type, dest_type, loc, builder, context)?;
    Ok(())
    
}

fn gen_function_call_to_temporary(function_ident: &String, args: &Vec<Box<ExpressionNode>>, expected_type: &Type, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
    let function = match ident_to_function(function_ident) {
        None => return Err(SemanticError { loc: loc, message: "Function does not exist".to_string() }),
        Some(function) => function
    };

    let sig = function_to_signature(&function);
    // check correct number of args
    if sig.num_args != args.len() {
        return Err(SemanticError { loc, message: format!("Incorrect number of args for function {}: expected {} got {}", function_ident, sig.num_args, args.len()) })
    }
    // return type check
    if sig.return_type.depth() != expected_type.depth() {
        return Err(SemanticError { loc, message: format!("Expected a type of depth {}, got {} (depth {})", expected_type.depth(), sig.return_type, sig.return_type.depth()) });
    }

    // currently all functions return a single byte
    let result = builder.new_nameless_variable(1);
    match function {
        Function::Len => {
            // don't evaluate the arg. Compute its type at compile time
            let arg_type = args.get(0).unwrap().get_type(context)?;
            let len = match arg_type.nesting.first() {
                Some(l) => *l,
                None => {
                    return Err(SemanticError{loc: args.get(0).unwrap().loc, message: "Cannot get len of byte".to_string()})
                }
            };
            builder.insert_statement(Stmt {
                // result = len
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(len as u8)),
                arg2: None,
                result: Some(result),
                loc
            });
        },
        Function::Wait => {
            builder.insert_statement(Stmt {
                // wait
                kind: StmtKind::Nullary(Nullary::Wait),
                arg1: None,
                arg2: None,
                result: None,
                loc
            });
            builder.insert_statement(Stmt {
                // result = 0
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(0)),
                arg2: None,
                result: Some(result),
                loc
            });
        },
        Function::Input => {
            builder.insert_statement(Stmt{
                // result = input
                kind: StmtKind::Nullary(Nullary::GetInput),
                arg1: None,
                arg2: None,
                result: Some(result),
                loc
            });
        },
        Function::UpdateScreen => {
            builder.insert_statement(Stmt {
                // update screen
                kind: StmtKind::Nullary(Nullary::UpdateScreen),
                arg1: None,
                arg2: None,
                result: None,
                loc
            });
            builder.insert_statement(Stmt {
                // result = 0
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(0)),
                arg2: None,
                result: Some(result),
                loc
            });
        },
        Function::RAMWrite => {
            let ptr = gen_expression_to_temporary(args.get(0).unwrap(), &BYTE, builder, context)?;
            let val = gen_expression_to_temporary(args.get(1).unwrap(), &BYTE, builder, context)?;
            builder.insert_statement(Stmt {
                // [ptr] = val
                kind: StmtKind::Binary(Binary::RAMWrite),
                arg1: Some(StmtArg::Variable(ptr)),
                arg2: Some(StmtArg::Variable(val)),
                result: None,
                loc
            });
            builder.insert_statement(Stmt {
                // result = 0
                kind: StmtKind::Unary(Unary::Copy),
                arg1: Some(StmtArg::Literal(0)),
                arg2: None,
                result: Some(result),
                loc
            });
        }
    }
    Ok(result)
    
}

fn exact_type_check(loc: CodeLocation, expected: &Type, actual: &Type) -> Result<(), SemanticError> {
    if *expected != *actual {
        return Err(SemanticError { loc, message: format!("Expected a {}, got {}", expected, actual) })
    }
    return Ok(())
}

trait Typeable {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError>;
}

impl Typeable for &Box<ExpressionNode> {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError> {
        match (*self.type_annotation.borrow()).clone() {
            // use memoized type
            Some(r#type) => Ok(r#type),
            None => {
                let r#type = match &self.d {
                    ExpressionDetail::Number { val } => {
                        val.get_type(context)?
                    },
                    ExpressionDetail::Array { val } => {
                        if val.len() == 0 {
                            return Err(SemanticError { loc: self.loc, message: "0 length array".to_string() })
                        }
                        let mut child_types = vec![];
                        for child in val {
                            child_types.push(child.get_type(context)?);
                        }

                        // check consistent nesting depth
                        let nesting_depth = child_types[0].depth();
                        for (i, child) in (&child_types[1..]).iter().enumerate() {
                            if child.depth() != nesting_depth {
                                return Err(SemanticError { loc: val[i+1].loc, message: format!("Expected a type of depth {}, got {} (depth {})", nesting_depth, child, child.depth()) })
                            }
                        }

                        // find type that can contain all children
                        let mut r#type = Type {
                            nesting: vec![0;val.len()],
                        };
                        for child_type in child_types {
                            r#type = r#type.enlarged_to_fit(&child_type);
                        }

                        r#type

                    }
                    ExpressionDetail::FunctionCall { ident, args:_ } => {
                        let function = match ident_to_function(ident) {
                            None => return Err(SemanticError { loc: self.loc, message: "Function does not exist".to_string() }),
                            Some(function) => function
                        };
                        function_to_signature(&function).return_type
                    },
                    ExpressionDetail::MemoryValue { val } => {
                        val.get_type(context)?
                    },

                    ExpressionDetail::LeftShift { val:_ } |
                    ExpressionDetail::RightShift { val:_ } |
                    ExpressionDetail::BitwiseNOT { val:_ } |
                    ExpressionDetail::Log2 { val:_ } |
                    ExpressionDetail::LogicalNOT { val:_ } |
                    ExpressionDetail::Add { left:_, right:_ } |
                    ExpressionDetail::Subtract { left:_, right:_ } |
                    ExpressionDetail::Multiply { left:_, right:_ } |
                    ExpressionDetail::Divide { left:_, right:_ } |
                    ExpressionDetail::Modulo { left:_, right:_ } |
                    ExpressionDetail::BitwiseXOR { left:_, right:_ } |
                    ExpressionDetail::BitwiseAND { left:_, right:_ } |
                    ExpressionDetail::BitwiseOR { left:_, right:_ } |
                    ExpressionDetail::EqualTo { left:_, right:_ } |
                    ExpressionDetail::LogicalOR { left:_, right:_ } |
                    ExpressionDetail::LogicalAND { left:_, right:_ } |
                    ExpressionDetail::SignedGreaterThanOrEqualTo { left:_, right:_ } |
                    ExpressionDetail::SignedLessThanOrEqualTo { left:_, right:_ } |
                    ExpressionDetail::SignedLessThan { left:_, right:_ } |
                    ExpressionDetail::SignedGreaterThan { left:_, right:_ } |
                    ExpressionDetail::UnsignedGreaterThanOrEqualTo { left:_, right:_ } |
                    ExpressionDetail::UnsignedLessThanOrEqualTo { left:_, right:_ } |
                    ExpressionDetail::UnsignedLessThan { left:_, right:_ } |
                    ExpressionDetail::UnsignedGreaterThan { left:_, right:_ } => {
                        BYTE
                    }
                };

                // memoize
                *(self.type_annotation.borrow_mut()) = Some(r#type.clone());
                Ok(r#type)
            }
        }
    }
}

impl Typeable for &Box<MemoryLocationNode> {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError> {
        match &self.d {
            MemoryLocationDetail::Identifier { val } => {
                let info = match context.get_variable_declaration(val) {
                    Some(info) => info,
                    None => return Err(SemanticError { loc: self.loc, message: "Variable used before declaration".to_string() }),
                };
                Ok(info.r#type)
            },
            MemoryLocationDetail::ArrayIndex { i:_, arr } => {
                let array_type = arr.get_type(context)?;
                match array_type.inner() {
                    None => Err(SemanticError { loc: self.loc, message: "Cannot index into byte".to_string() }),
                    Some(t) => Ok(t)
                }
            },
            MemoryLocationDetail::ArraySlice { start:_, arr } => {
                arr.get_type(context)
            }
        }
    }
}

impl Typeable for &Box<ConstantNode> {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError> {
        match &self.d {
        ConstantDetail::Number { val:_ } => Ok(Type{
            nesting: vec![]
        }),
        ConstantDetail::Array { val } => {
            if val.len() == 0 {
                return Err(SemanticError { loc: self.loc, message: "Size 0 array".to_string() })
            }
            let mut node_iter = val.iter();
            let first_child = node_iter.next().unwrap();
            let first_child_type = first_child.get_type(context)?;
            // expect all other children to have the same type
            for child in node_iter {
                let child_type = child.get_type(context)?;
                if first_child_type != child_type {
                    return Err(SemanticError { loc: self.loc, message: format!("Inconsistent type within array: {} and {}", first_child_type, child_type) });
                }
            }
            let mut nesting = vec![val.len()];
            nesting.extend(first_child_type.nesting);
            return Ok(Type { nesting })
            
        }
    }
    }
}

impl Typeable for &Box<NumberNode> {
    fn get_type(&self, _: &Context) -> Result<Type, SemanticError> {
        Ok(Type {
            nesting: vec![]
        })
    }
}

impl Typeable for &Box<TypeNode> {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError> {
        let (type_body, is_const) = match &self.d {
            TypeDetail::ConstType { t } => (t, true),
            TypeDetail::NonConstType { t } => (t, false),
        };
        let r#type  = type_body.get_type(context)?;
        Ok(r#type)
    }
}

impl Typeable for &Box<TypeBodyNode> {
    fn get_type(&self, context: &Context) -> Result<Type, SemanticError> {
        match &self.d {
            TypeBodyDetail::Byte {  } => Ok(Type { nesting: vec![] }),
            TypeBodyDetail::Array { size, t } => {
                if size.val == 0 {
                    Err(SemanticError{loc: size.loc, message: "0 size in array length".to_string()})
                } else {
                    let mut r#type = t.get_type(context)?;
                    r#type.nesting.insert(0, size.val as usize);
                    Ok(r#type)
                }
            }
        }
    }
}




fn flatten_constant(node: &Box<ConstantNode>) -> Vec<u8> {
    match &node.d {
        ConstantDetail::Number { val } => vec![val.val],
        ConstantDetail::Array { val } => {
            let mut out = vec![];
            for constant in val {
                out.extend(flatten_constant(constant));
            }
            out
        },
    }
}















impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut nesting_part = String::new();
        for size in &self.nesting {
            nesting_part = format!("{}[{}]", nesting_part, size)

        }

        write!(f, "{}byte", nesting_part)
    }
}