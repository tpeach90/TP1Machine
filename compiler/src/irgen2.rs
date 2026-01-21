
use std::{cmp::min, collections::HashMap, iter::zip, string::ParseError, thread::Builder, vec};

use crate::{ast::*, common::{BranchFlag, CodeLocation}, ir::{BasicBlock, Binary, BlockPointer, Branch, IntermediateRepresentation, Stmt, StmtArg, StmtKind, Unary, VarID}};

pub struct SemanticError {
    pub loc: CodeLocation,
    pub message: String,
}

struct Context {
    stack: ContextStack,
}

impl Context {
    fn new() -> Context {
        Context { stack: vec![] }
    }

    fn new_frame(&mut self) {
        self.stack.push(ContextFrame { declarations: HashMap::new() });
    }

    fn pop_frame(&mut self) -> Option<ContextFrame> {
        self.stack.pop()
    }

    fn get_variable_declaration(&self, ident: &str) -> Option<SymbolInfo> {
        for frame in self.stack.iter().rev() {
            match frame.declarations.get(ident) {
                Some(info) => return Some(info.clone()),
                None => ()
            }
        }
        return None;
    }

    fn get_variable_declaration_in_outermost_frame(&self, ident: &str) -> Option<SymbolInfo> {
        match self.stack.last() {
            None => None,
            Some(frame) => frame.declarations.get(ident).cloned()
        }
    }

    fn add_declaration(&mut self, ident: &str, info: SymbolInfo) {
        self.stack.last_mut().unwrap().declarations.insert(ident.to_owned(), info);
    }

}

type ContextStack = Vec<ContextFrame>;
struct ContextFrame {
    declarations: HashMap<String/*ident*/, SymbolInfo>,
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
    // is_const: bool,
}

#[derive(Clone)]
pub enum TypeNestingConfig {
    // if Type refers to a slice we do not know how many elements are in the outermost layer.
    OutermostSizeUnknown{not_including_outermost: Vec<usize>},
    OutermostSizeKnown{including_outermost: Vec<usize>},
}

// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum ArraySize {
//     Unknown,
//     Known(usize)
// }

impl Type {
    // fn possible_to_coerce_to(&self, other: &Type) -> bool {
        
    //     if ! self.is_const && other.is_const {
    //         return false;
    //     }

    //     if self.nesting.len() != other.nesting.len() {
    //         return false;
    //     }

    //     // if either has outer size unknown, then the outer layer is compatible
    //     // the generated code will just start filling from wherever is determined at runtime (unsafe but whatever)
    //     for pair in zip(&self.nesting, &other.nesting) {
    //         match pair {
    //             (ArraySize::Known(size1), ArraySize::Known(size2)) => {
    //                 if *size1 != *size2 {
    //                     return false
    //                 }
    //             }
    //             _ => ()
    //         }
    //     }

    //     return true;

    // }

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

    // fn combine_and_resolve(&self, other: &Type) -> Option<Type> {
    //     let mut combined = self.clone();
    //     combined.is_const = self.is_const && other.is_const;
    //     if self.nesting.len() != other.nesting.len() {
    //         return None
    //     }
    //     for (i, nesting_pair) in zip(&self.nesting, &other.nesting).enumerate() {
    //         let size = match nesting_pair {
    //             (ArraySize::Known(x), ArraySize::Known(y)) => 
    //             if x == y {
    //                 ArraySize::Known(*x)
    //             } else {
    //                 return None
    //             },
    //             (ArraySize::Unknown, ArraySize::Known(x)) | (ArraySize::Known(x), ArraySize::Unknown) => ArraySize::Known(*x),
    //             (ArraySize::Unknown, ArraySize::Unknown) => ArraySize::Unknown
    //         };
    //         combined.nesting[i] = size;
    //     }
    //     return Some(combined);

    // }
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

// trait Function {
//     fn gen_function(&self, args: Vec<VarID>, builder: &mut Builder, context: &mut Context) -> Result<VarID, SemanticError>;
//     fn arg_types(&self) -> Vec<Type>;
//     fn return_type(&self) -> Type;
// }

pub enum Function {
    Len
}

pub fn ident_to_function(ident: &str) -> Option<Function> {
    match ident {
        "len" => Some(Function::Len),
        _ => None
    }
}

pub struct FunctionSignature {
    arg_types: Vec<Type>,
    return_type: Type,
}

pub fn function_to_signature(function: &Function) -> FunctionSignature {
    match function {
        Function::Len => FunctionSignature {
            arg_types: vec![],
            return_type: Type { nesting: vec![]}
        }
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
            builder.set_continue(Some(loop_body));
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            builder.set_continue(Some(loop_body));
            return Ok(())
        },
        InnerStatementDetail::WhileLoop { condition, body } => {
            let loop_condition = builder.new_block();
            let loop_body = builder.new_block();
            let loop_after = builder.new_block();
            builder.set_continue(Some(loop_condition));
            builder.set_insert_point_at_block_end(loop_condition);
            gen_condition(condition, builder, context, loop_body, loop_after)?;
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            builder.set_continue(Some(loop_condition));
            builder.set_insert_point_at_block_end(loop_after);
            Ok(())

        },
        InnerStatementDetail::DoWhileLoop { body, condition } => {
            let loop_body = builder.new_block();
            let loop_after = builder.new_block();
            builder.set_continue(Some(loop_body));
            builder.set_insert_point_at_block_end(loop_body);
            gen_block(body, builder, context)?;
            gen_condition(condition, builder, context, loop_body, loop_after)?;
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
        InnerStatementDetail::DeclarationStatement { r#type, identifier, val } => todo!(),
        InnerStatementDetail::AssignmentStatement { lvalue, rvalue } => {
            // let expected_type = lvalue
            todo!()
        },
        InnerStatementDetail::ExpressionStatement { val } => todo!(),
        InnerStatementDetail::BreakStatement {  } => todo!(),
        InnerStatementDetail::ContinueStatement {  } => todo!(),
        InnerStatementDetail::Block { body } => todo!(),
    }
}

fn gen_condition(condition: &Box<ConditionNode>, builder: &mut IRBuilder, context: &mut Context, block_if_true: BlockPointer, block_if_false: BlockPointer) -> Result<(), SemanticError> {
    match &condition.d {
        ConditionDetail::Branch { flag } => {
            builder.set_branch(Some(Branch { flag: *flag, to: block_if_true }));
            builder.set_continue(Some(block_if_false));
            Ok(())
        },
        ConditionDetail::Expression { val } => todo!(),
    }
}

fn gen_fill_from_expression(node: &Box<ExpressionNode>, expected_type: &Type, ram_pointer: VarID, fill_var: VarID, builder: &mut IRBuilder, context: &mut Context ) -> Result<(), SemanticError> {
    // if expected_type.is_const {
    //     return Err(SemanticError { loc: node.loc, message: "Writing to const".to_string() })
    // }
    match &node.d {
        ExpressionDetail::Number { val } => {
            // type check
            if expected_type.nesting.len() != 0 {
                return Err(SemanticError { loc: node.loc, message: format!("Expected an array of type {}", expected_type) })
            }
            // write to arr
            builder.insert_statement(Stmt {
                loc: node.loc,
                kind: StmtKind::Binary(Binary::RAMWrite),
                arg1: Some(StmtArg::Variable(ram_pointer)),
                arg2: Some(StmtArg::Literal(val.val)),
                result: Some(fill_var)
            });
            // inc pointer
            builder.insert_statement(Stmt {
                loc: node.loc,
                kind: StmtKind::Unary(Unary::Increment),
                arg1: Some(StmtArg::Variable(ram_pointer)),
                arg2: None,
                result: Some(ram_pointer)
            });
            Ok(())
        },
        ExpressionDetail::Array { val } => {

            // filling an array
            // source and destination arrays may be of different types
            // but as long as the depth is the same it's fine
            // we just skip indeces in the source or destination arrays when necessary

            let actual_type = node.get_type(context)?;

            if actual_type.depth() != expected_type.depth() {
                return Err(SemanticError { loc: node.loc, message: format!("Expected type {}, got {}", expected_type, actual_type) });
            }
            let expected_inner_type = match expected_type.inner() {
                None => return Err(SemanticError { loc: node.loc, message: "Expected an array".to_string() }),
                Some(t) => t
            };
            if actual_type.nesting.first().unwrap() == expected_type.nesting.first().unwrap()  {
                // Don't need to mess about with the pointer to get it to align correctly.
                for child in val {
                    gen_fill_from_expression(child, &expected_inner_type, ram_pointer, fill_var, builder, context)?;
                }
            } else if actual_type.nesting.first().unwrap() > expected_type.nesting.first().unwrap() {
                // too many values to fit in the destination. ignore the out of bounds ones.
                for child in &val[0..*expected_type.nesting.first().unwrap()] {
                    gen_fill_from_expression(child, &expected_inner_type, ram_pointer, fill_var, builder, context)?;
                }
            } else {
                // too few values to fit in the destination.
                // we may be inside a nested array definition, so increase the pointer at the end so we stay in line with the flattened array.
                for child in val {
                    gen_fill_from_expression(child, &expected_inner_type, ram_pointer, fill_var, builder, context)?;
                }
                let pointer_increase_amount = (expected_type.nesting.first().unwrap() - val.len()) * expected_inner_type.size();
                builder.insert_statement(Stmt{
                    kind: StmtKind::Binary(Binary::Add),
                    loc: node.loc,
                    arg1: Some(StmtArg::Variable(ram_pointer)),
                    arg2: Some(StmtArg::Literal(pointer_increase_amount as u8)),
                    result: Some(ram_pointer)
                });
            }

            todo!();
            
        },
        ExpressionDetail::FunctionCall { ident, args } => todo!(),
        ExpressionDetail::MemoryValue { val } => todo!(),
        // ExpressionDetail::MemoryReference { val } => todo!(),
        ExpressionDetail::LeftShift { val } => todo!(),
        ExpressionDetail::RightShift { val } => todo!(),
        ExpressionDetail::BitwiseNOT { val } => todo!(),
        ExpressionDetail::LogicalNOT { val } => todo!(),
        ExpressionDetail::Log2 { val } => todo!(),
        ExpressionDetail::Add { left, right } => todo!(),
        ExpressionDetail::Subtract { left, right } => todo!(),
        ExpressionDetail::Multiply { left, right } => todo!(),
        ExpressionDetail::Divide { left, right } => todo!(),
        ExpressionDetail::Modulo { left, right } => todo!(),
        ExpressionDetail::BitwiseXOR { left, right } => todo!(),
        ExpressionDetail::BitwiseAND { left, right } => todo!(),
        ExpressionDetail::BitwiseOR { left, right } => todo!(),
        ExpressionDetail::EqualTo { left, right } => todo!(),
        ExpressionDetail::LogicalOR { left, right } => todo!(),
        ExpressionDetail::LogicalAND { left, right } => todo!(),
        ExpressionDetail::SignedGreaterThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::SignedLessThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::SignedLessThan { left, right } => todo!(),
        ExpressionDetail::SignedGreaterThan { left, right } => todo!(),
        ExpressionDetail::UnsignedGreaterThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::UnsignedLessThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::UnsignedLessThan { left, right } => todo!(),
        ExpressionDetail::UnsignedGreaterThan { left, right } => todo!(),
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
        ExpressionDetail::Array { val } => todo!(),
        ExpressionDetail::FunctionCall { ident, args } => todo!(),
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
        ExpressionDetail::LogicalNOT { val } => {
            todo!()
        },
        ExpressionDetail::LogicalOR { left, right } => todo!(),
        ExpressionDetail::LogicalAND { left, right } => todo!(),
        ExpressionDetail::EqualTo { left, right } => todo!(),
        ExpressionDetail::SignedGreaterThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::SignedLessThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::SignedLessThan { left, right } => todo!(),
        ExpressionDetail::SignedGreaterThan { left, right } => todo!(),
        ExpressionDetail::UnsignedGreaterThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::UnsignedLessThanOrEqualTo { left, right } => todo!(),
        ExpressionDetail::UnsignedLessThan { left, right } => todo!(),
        ExpressionDetail::UnsignedGreaterThan { left, right } => todo!(),
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

fn gen_get_memory_value(node: &Box<MemoryLocationNode>, expected_type: &Type, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
    let actual_type = node.get_type(context)?;
    if actual_type.depth() != expected_type.depth() {
        return Err(SemanticError { loc: node.loc, message: format!("Expected a type of depth {}, got {} (depth {})", expected_type.depth(), actual_type, actual_type.depth()) });
    }
    match &node.d {
        MemoryLocationDetail::Identifier { val } => {
            match context.get_variable_declaration(&val) {
                None => Err(SemanticError{loc: node.loc, message: "Variable used before declaration".to_string()}),
                Some(info) => {
                    if actual_type == *expected_type {
                        // just use a pointer to the array
                        Ok(info.id)
                    } else {
                        // copy array into correct type
                        let dest_array = builder.new_nameless_variable(expected_type.size());
                        let source_ptr = builder.new_nameless_variable(1);
                        let dest_ptr = builder.new_nameless_variable(1);
                        builder.insert_statement(Stmt {
                            // src_ptr = start of source array
                            kind: StmtKind::Unary(Unary::Copy),
                            arg1: Some(StmtArg::LocationOfVariable(info.id)),
                            arg2: None,
                            result: Some(source_ptr),
                            loc: node.loc
                        });
                        builder.insert_statement(Stmt {
                            // dest_ptr = start of dest array
                            kind: StmtKind::Unary(Unary::Copy),
                            arg1: Some(StmtArg::LocationOfVariable(dest_array)),
                            arg2: None,
                            result: Some(dest_ptr),
                            loc: node.loc
                        });
                        gen_copy_array(info.id, dest_array, source_ptr, dest_ptr, &info.r#type, expected_type, node.loc, builder, context)?;
                        Ok(dest_array)
                    }
                }
            }
        },
        MemoryLocationDetail::ArrayIndex { i, arr } => todo!(),
        MemoryLocationDetail::ArraySlice { start, arr } => todo!(),
    }

}

fn gen_copy_array(source: VarID, dest: VarID, source_ptr: VarID, dest_ptr: VarID, source_type: &Type, dest_type: &Type, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

    

    if source_type.depth() != dest_type.depth() {
        return Err(SemanticError { loc: loc, message: format!("Expected an array of depth {}, got {} (depth {})", dest_type.depth(), source_type, source_type.depth()) });
    }
    gen_copy_array_2(source, dest, source_ptr, dest_ptr, source_type, dest_type, true, loc, builder, context)

}

fn gen_copy_array_2(source: VarID, dest: VarID, source_ptr: VarID, dest_ptr: VarID, source_type: &Type, dest_type: &Type, is_outer_layer: bool, loc: CodeLocation, builder: &mut IRBuilder, context: &mut Context) -> Result<(), SemanticError> {

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
            gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;

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
                gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;
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
                gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(1).unwrap(), &dest_type.strip_layers(1).unwrap(), false, loc, builder, context)?;
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
            gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?; 
        } else {
            if is_outer_layer {
                // use a test on dest_ptr instead of creating a new temporary
                let loop_start = builder.new_block();
                let loop_after = builder.new_block();
                builder.set_insert_point_at_block_end(loop_start);
                gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?; 
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
                gen_copy_array_2(source, dest, source_ptr, dest_ptr, &source_type.strip_layers(l).unwrap(), &dest_type.strip_layers(l).unwrap(), false, loc, builder, context)?;
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

fn exact_type_check(loc: CodeLocation, expected: &Type, actual: &Type) -> Result<(), SemanticError> {
    if *expected != *actual {
        return Err(SemanticError { loc, message: format!("Expected a {}, got {}", expected, actual) })
    }
    return Ok(())
}

// fn gen_expression_coerce_to_type(node: &Box<ExpressionNode>, expected_type: Type, pointer_for_array: Option<VarID>, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {
//     match &node.d {
//         ExpressionDetail::Number { val } => {
//             let var_id = builder.new_nameless_variable(1);
//             builder.insert_statement(Stmt {
//                 loc: node.loc, 
//                 kind: StmtKind::Unary(Unary::Copy), 
//                 arg1: Some(StmtArg::Literal(val.val)), 
//                 arg2: None, 
//                 result: Some(var_id)
//             });
//             let actual_type = Type{is_const: true, nesting: vec![]};
//             let coerced_var_id = gen_type_coerce(expected_type, actual_type, var_id, builder, context)?;
//             Ok(coerced_var_id)
//         },
//         ExpressionDetail::Array { val } => {

//             let outer_size = match expected_type.nesting.first() {
//                 Some(ArraySize::Known(size)) => {
//                     if *size != val.len() {
//                         return Err(SemanticError { loc: node.loc, message: "Fill to array with incompatible length".to_string() })
//                     }
//                     *size
//                 },
//                 Some(ArraySize::Unknown) => {
//                     val.len()
//                 },
//                 None => return Err(SemanticError { loc: node.loc, message: "Cannot fill byte with array".to_string() }),
//             };

//             let inner_type= expected_type.inner();
//             for expr in val {
//                 // screw this
//             }

//         },
//         ExpressionDetail::FunctionCall { ident, args } => todo!(),
//         ExpressionDetail::MemoryValue { val } => todo!(),
//         ExpressionDetail::MemoryReference { val } => todo!(),
//         ExpressionDetail::LeftShift { val } => todo!(),
//         ExpressionDetail::RightShift { val } => todo!(),
//         ExpressionDetail::BitwiseNOT { val } => todo!(),
//         ExpressionDetail::LogicalNOT { val } => todo!(),
//         ExpressionDetail::Log2 { val } => todo!(),
//         ExpressionDetail::Add { left, right } => todo!(),
//         ExpressionDetail::Subtract { left, right } => todo!(),
//         ExpressionDetail::Multiply { left, right } => todo!(),
//         ExpressionDetail::Divide { left, right } => todo!(),
//         ExpressionDetail::Modulo { left, right } => todo!(),
//         ExpressionDetail::BitwiseXOR { left, right } => todo!(),
//         ExpressionDetail::BitwiseAND { left, right } => todo!(),
//         ExpressionDetail::BitwiseOR { left, right } => todo!(),
//         ExpressionDetail::EqualTo { left, right } => todo!(),
//         ExpressionDetail::LogicalOR { left, right } => todo!(),
//         ExpressionDetail::LogicalAND { left, right } => todo!(),
//         ExpressionDetail::SignedGreaterThanOrEqualTo { left, right } => todo!(),
//         ExpressionDetail::SignedLessThanOrEqualTo { left, right } => todo!(),
//         ExpressionDetail::SignedLessThan { left, right } => todo!(),
//         ExpressionDetail::SignedGreaterThan { left, right } => todo!(),
//         ExpressionDetail::UnsignedGreaterThanOrEqualTo { left, right } => todo!(),
//         ExpressionDetail::UnsignedLessThanOrEqualTo { left, right } => todo!(),
//         ExpressionDetail::UnsignedLessThan { left, right } => todo!(),
//         ExpressionDetail::UnsignedGreaterThan { left, right } => todo!(),
//     }

//     // type coersion - might do nothing if types already match

// }

// fn gen_type_coerce(expected_type: Type, actual_type: Type, var_id: VarID, builder: &mut IRBuilder, context: &mut Context) -> Result<VarID, SemanticError> {

//     if expected_type == actual_type {
//         return Ok(var_id)
//     }

//     todo!()
// }

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
                                return Err(SemanticError { loc: val[i+1].loc, message: format!("Inconsistent array nesting depth. Expected {} got {}", nesting_depth, child.depth()) })
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
                    // ExpressionDetail::MemoryReference { val } => todo!(),

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
                        Type {
                            nesting: vec![]
                        }
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
            TypeBodyDetail::Void {  } => todo!(),
            // TypeBodyDetail::ROMPointer { t } => todo!(),
            // TypeBodyDetail::RAMPointer { t } => todo!(),
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