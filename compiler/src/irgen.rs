use std::{collections::HashMap, fmt::Display};

use itertools::izip;

use crate::{ast::*, common::CodeLocation, ir::{BasicBlock, Constant, IntermediateRepresentation, Stmt, StmtArg, Unary, VarID}};

pub struct SemanticError {
    pub loc: CodeLocation,
    pub message: String,
}

struct Context {
    stack: ContextStack,
    id_counter: VarID
}

type ContextStack = Vec<ContextFrame>;
struct ContextFrame {
    declarations: HashMap<String/*ident*/, SymbolInfo>,
}


impl Context {
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

    fn next_id(&mut self) -> VarID {
        let out = self.id_counter;
        self.id_counter += 1;
        return out;
    }
}

#[derive(Clone)]
struct SymbolInfo {
    id: VarID,
    r#type: Type,
}

#[derive(Clone, PartialEq, Eq)]
struct Type {
    // eg:
    //  "let const [4][8]byte myArr = ...", myArr has array_nesting = [4, 8]
    //  "myArr[2]": array_nesting=[8]
    //  "myArr[1:]": array_nesting=[0,8], and outer_size_unknown=true
    //  "8": array_nesting=[1]
    //  "let const void myVoid", myVoid: array_nesting=[0]
    nested_arrays: Vec<usize>,
    outer_size_unknown: bool,
    is_const: bool,
}

// A >= B if a can be coerced to B
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.nested_arrays.len() != other.nested_arrays.len() {
            return None;
        }

        // check length of each nested array is compatible
        // outermost array might have unknown size if we are assigning to a slice -> assume compatible
        for (i, (a, b)) in izip!(&self.nested_arrays, &other.nested_arrays).enumerate() {
            if a != b && !(i == 0 && (self.outer_size_unknown || other.outer_size_unknown)) {
                return None;
            }
        }

        // cannot assign to const - check
        match 2*(self.is_const as u8) + (other.is_const as u8) {
            0 => Some(std::cmp::Ordering::Equal),
            1 => Some(std::cmp::Ordering::Less),
            2 => Some(std::cmp::Ordering::Greater),
            _ => None
        }

    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

trait GenIR {
    fn gen_ir(&self, context: & mut Context, ir: & mut IntermediateRepresentation, block: BasicBlock) -> Result<BasicBlock, SemanticError>;
}

impl GenIR for ProgramNode {
    fn gen_ir(&self, context: & mut Context, ir: & mut IntermediateRepresentation, block: BasicBlock) -> Result<BasicBlock, SemanticError>{

        context.stack.push(ContextFrame { declarations: HashMap::new() });
        
        let mut block = BasicBlock::new();

        for node in &self.outer_statements {
            block = node.gen_ir(context, ir, block)?;
        }

        context.stack.pop();
        return Ok(block)
    }
}

impl GenIR for OuterStatementNode {
    fn gen_ir(&self, context: & mut Context, ir: & mut IntermediateRepresentation, block: BasicBlock) -> Result<BasicBlock, SemanticError> {
        match &self.d {
            OuterStatementDetail::DeclarationStatement { r#type, identifier, val } => {
                if context.get_variable_declaration_in_outermost_frame(identifier).is_some() {
                    return SemanticError{loc: self.loc, message: "Re-declaration of variable".to_string()}
                }

                let specified_type: Type = todo!();
                let actual_type: Type = todo!();

                if !(actual_type >= specified_type) {
                    return SemanticError{loc: self.loc, message: format!("Cannot convert {} to {}", actual_type, specified_type)};
                }

                // todo type conversion maybe

                let info = SymbolInfo {
                    id: context.next_id(),
                    r#type: todo!()
                };
                let literal = match &val.d {
                    ConstantDetail::Number { val } => todo!(),
                    ConstantDetail::Array { val } => todo!(),
                };

                context.add_declaration(identifier, info);
                block.statements.push(Stmt{
                    loc: self.loc,
                    kind: Unary,
                    arg1: Some(StmtArg::Literal(val.d))
                });
                Ok(block);
            },
        }
    }
}

impl GenIR for BlockNode {
    fn gen_ir(&self, context: & mut Context, ir: & mut IntermediateRepresentation, block: BasicBlock) -> Result<BasicBlock, SemanticError> {

        // declarations inside block
        context.stack.push(ContextFrame { declarations: HashMap::new() });

        for stmt in &self.statements {
            stmt.gen_ir(context, ir)?;
        }

        context.stack.pop();
        return Ok(())
    }
}

impl GenIR for InnerStatementNode {
    fn gen_ir(&self, context: & mut Context, ir: & mut IntermediateRepresentation, block: BasicBlock) -> Result<BasicBlock, SemanticError> {
        match &self.d {
            InnerStatementDetail::ForeverLoop { body } => todo!(),
            InnerStatementDetail::WhileLoop { condition, body } => todo!(),
            InnerStatementDetail::DoWhileLoop { body, condition } => todo!(),
            InnerStatementDetail::IfStatement { condition, r#true } => {
                
            },
            InnerStatementDetail::IfElseStatement { condition, r#true, r#false } => todo!(),
            InnerStatementDetail::DeclarationStatement { r#type, identifier, val } => todo!(),
            InnerStatementDetail::AssignmentStatement { lvalue, rvalue } => todo!(),
            InnerStatementDetail::ExpressionStatement { val } => todo!(),
            InnerStatementDetail::BreakStatement {  } => todo!(),
            InnerStatementDetail::ContinueStatement {  } => todo!(),
            InnerStatementDetail::Block { body } => todo!(),
        }
    }
}