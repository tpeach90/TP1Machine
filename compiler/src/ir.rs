use std::collections::HashMap;

use crate::common::{BranchFlag, CodeLocation};

pub type VarID = usize;
// pub type VarVersion = usize;
pub type BlockPointer = usize;

pub struct IntermediateRepresentation {
    pub global_constants: HashMap<VarID, Vec<u8>>,
    pub basic_blocks: Vec<BasicBlock>,
    pub id_to_ident: HashMap<VarID, String>
}

impl IntermediateRepresentation {
    fn get_block(&self, ptr: BlockPointer) -> Option<&BasicBlock>{
        return self.basic_blocks.get(ptr);
    }
}

pub struct BasicBlock {
    pub statements: Vec<Stmt>,
    
    pub branch: Option<Branch>,
    pub continue_to: Option<BlockPointer>,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            branch: None,
            continue_to: None,
            statements: vec![]
        }
    }
}

pub struct Branch {
    pub flag: BranchFlag,
    pub to: BlockPointer // BasicBlock
}

pub struct Stmt {
    pub loc: CodeLocation, // origin from input source code
    pub kind: StmtKind,
    pub arg1: Option<StmtArg>,
    pub arg2: Option<StmtArg>,
    pub result: Option<StmtResult>,
}

pub enum StmtKind {
    Nullary(Nullary),
    Unary(Unary),
    Binary(Binary),
    Phi(BlockPointer, BlockPointer)
}

pub enum StmtArg {
    Literal(u8),
    Variable(Variable),
    Constant(Constant), // reference to rom variable
}

pub struct StmtResult {
    pub var: Variable,
    pub offset: u8, // used if writing to an array.
}

pub struct Variable {
    pub id: VarID,
    // pub version: VarVersion
}

pub struct Constant {
    pub id: VarID,
}

pub enum Nullary {
    UpdateScreen,
    Wait,
    GetInput,
    Stop,
}

pub enum Unary {
    Copy,
    Increment,
    Decrement,
    LeftShift,
    RightShift,
    TwosComplement,
    Compare,
    BitwiseNOT,
    Log2,
}

pub enum Binary {
    RAMWrite,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseXOR,
    BitwiseAND,
    BitwiseOR,
}






