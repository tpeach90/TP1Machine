use crate::common::CodeLocation;

pub enum Node<'a> {
    ProgramNode(ProgramNode<'a>),
    OuterStatementNode(OuterStatementNode<'a>),
    FunctionArgumentNode(FunctionArgumentNode<'a>),
    TypeNode(TypeNode<'a>),
    TypeBodyNode(TypeBodyNode<'a>),
    BlockNode(BlockNode<'a>),
    InnerStatementNode(InnerStatementNode<'a>),
    ConditionNode(ConditionNode<'a>),
    ConditionDetail(ConditionDetail<'a>),
    ExpressionNode(ExpressionNode<'a>),
    MemoryLocationNode(MemoryLocationNode<'a>),
    ConstantNode(ConstantNode<'a>)
}

pub struct ProgramNode<'a> {
    pub loc: CodeLocation,
    pub statements: Vec<OuterStatementNode<'a>>,
}


pub struct OuterStatementNode<'a> {
    pub loc: CodeLocation,
    pub d: OuterStatementDetail<'a>
}

pub enum OuterStatementDetail<'a> {
    DeclarationStatement {r#type: &'a TypeNode<'a>, identifier: String, val: &'a ConstantNode<'a>},
    Function {r#type: &'a TypeNode<'a>, identifier: String, args: Vec<&'a FunctionArgumentNode<'a>>, body: &'a BlockNode<'a>}
}

pub struct FunctionArgumentNode<'a> {
    pub loc: CodeLocation,
    pub r#type: TypeNode<'a>,
    pub identifier: String,
}

pub struct TypeNode<'a> {
    pub loc: CodeLocation,
    pub d: TypeDetail<'a>,
}

pub enum TypeDetail<'a> {
    ConstType {t: &'a TypeBodyNode<'a>},
    NonConstType {t: &'a TypeBodyNode<'a>}
}

pub struct TypeBodyNode<'a> {
    pub loc: CodeLocation,
    pub d: TypeBodyDetail<'a>,
}

pub enum TypeBodyDetail<'a> {
    Byte {},
    Void {},
    ROMPointer {t: &'a TypeBodyNode<'a>},
    RAMPointer {t: &'a TypeBodyNode<'a>},
    Array {size: u8, t: &'a TypeBodyNode<'a>},
}

pub struct BlockNode<'a> {
    pub loc: CodeLocation,
    pub statements: Vec<InnerStatementNode<'a>>
}

pub struct InnerStatementNode<'a> {
    pub loc: CodeLocation,
    pub d: InnerStatementDetail<'a>,
}

pub enum InnerStatementDetail<'a> {
    ForeverLoop {body: &'a BlockNode<'a>},
    WhileLoop {condition: &'a ConditionNode<'a>, body: &'a BlockNode<'a>},
    DoWhileLoop {body: &'a BlockNode<'a>, condition: &'a ConditionNode<'a>},
    IfStatement {condition: &'a ConditionNode<'a>, r#true: &'a BlockNode<'a>},
    IfElseStatement {condition: &'a ConditionNode<'a>, r#true: &'a BlockNode<'a>, r#false: &'a BlockNode<'a>},
    DeclarationStatement {r#type: &'a TypeNode<'a>, identifier: String, val: &'a ExpressionNode<'a>},
    AssignmentStatement{loc: &'a MemoryLocationNode<'a>, val: &'a ExpressionNode<'a>},
    ExpressionStatement {val: &'a ExpressionNode<'a>},
    Block{body: &'a BlockNode<'a>}
}

pub struct ConditionNode<'a> {
    pub loc: CodeLocation,
    pub d: ConditionDetail<'a>,
}

pub enum ConditionDetail<'a> {
    Branch {flag: BranchFlag},
    Expression {val: &'a ExpressionNode<'a>}
}

pub enum BranchFlag {
    BZ = 0,
    BNZ = 1,
    BC = 2,
    BNC = 3,
    BN = 4,
    BNN = 5,
    BO = 6,
    BNO = 7,
    BGTE = 8,
    BNGTE = 9,
}



pub struct ExpressionNode<'a> {
    pub loc: CodeLocation,
    pub d: ExpressionDetail<'a>,
}

pub enum ExpressionDetail<'a>{
    Number {val: u8},
    Array {val: Vec<&'a MemoryLocationNode<'a>>},
    MemoryValue {val: &'a MemoryLocationNode<'a>},
    MemoryAddress {val: &'a MemoryLocationNode<'a>},
    LeftShift {val: &'a ExpressionNode<'a>},
    RightShift {val: &'a ExpressionNode<'a>},
    BitwiseNot {val: &'a ExpressionNode<'a>},
    LogicalNot {val: &'a ExpressionNode<'a>},
    Log2 {val: &'a ExpressionNode<'a>},
    Add {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    Subtract {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    Multiply {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    Divide {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    Modulo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    BitwiseXOR {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    BitwiseAND {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    BitwiseOR {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    EqualTo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    SignedGreaterThanOrEqualTo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    SignedLessThanOrEqualTo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    SignedLessThan {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    SignedGreaterThan {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    UnsignedGreaterThanOrEqualTo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    UnsignedLessThanOrEqualTo {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    UnsignedLessThan {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
    UnsignedGreaterThan {left: &'a ExpressionNode<'a>, right: &'a ExpressionNode<'a>},
}


pub struct MemoryLocationNode<'a> {
    pub loc: CodeLocation,
    pub d: MemoryLocationDetail<'a>,
}

pub enum MemoryLocationDetail<'a> {
    Identifier {val: String},
    ROMDereference {val: &'a MemoryLocationNode<'a>},
    RAMDereference {val: &'a MemoryLocationNode<'a>},
    ArrayIndex {i: &'a ExpressionNode<'a>, arr: &'a MemoryLocationNode<'a>},
    ArraySlice {start: &'a ExpressionNode<'a>, end: &'a ExpressionNode<'a>, arr: &'a MemoryLocationNode<'a>},
    ArraySliceNoStart {end: &'a ExpressionNode<'a>, arr: &'a MemoryLocationNode<'a>},
    ArraySliceNoEnd {start: &'a ExpressionNode<'a>, arr: &'a MemoryLocationNode<'a>},
}

pub struct ConstantNode<'a> {
    pub loc: CodeLocation,
    pub d: ConstantDetail<'a>,
}

pub enum ConstantDetail<'a> {
    Number {val: u8},
    Array {val: Vec<&'a ConstantNode<'a>>}
}