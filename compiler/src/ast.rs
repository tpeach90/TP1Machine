use std::fmt::Debug;

use crate::common::CodeLocation;

pub enum Node {
    ProgramNode(ProgramNode),
    OuterStatementNode(OuterStatementNode),
    FunctionArgumentNode(FunctionArgumentNode),
    TypeNode(TypeNode),
    TypeBodyNode(TypeBodyNode),
    BlockNode(BlockNode),
    InnerStatementNode(InnerStatementNode),
    ConditionNode(ConditionNode),
    ConditionDetail(ConditionDetail),
    ExpressionNode(ExpressionNode),
    MemoryLocationNode(MemoryLocationNode),
    ConstantNode(ConstantNode)
}

#[derive(Debug)]
pub struct ProgramNode {
    pub loc: CodeLocation,
    pub statements: Vec<OuterStatementNode>,
}

#[derive(Debug)]
pub struct OuterStatementNode {
    pub loc: CodeLocation,
    pub d: OuterStatementDetail
}

#[derive(Debug)]
pub enum OuterStatementDetail {
    DeclarationStatement {r#type: Box<TypeNode>, identifier: String, val: Box<ConstantNode>},
    Function {r#type: Box<TypeNode>, identifier: String, args: Vec<Box <FunctionArgumentNode>>, body: Box <BlockNode>}
}

#[derive(Debug)]
pub struct FunctionArgumentNode {
    pub loc: CodeLocation,
    pub r#type: TypeNode,
    pub identifier: String,
}

#[derive(Debug)]
pub struct TypeNode {
    pub loc: CodeLocation,
    pub d: TypeDetail,
}

#[derive(Debug)]
pub enum TypeDetail {
    ConstType {t: Box <TypeBodyNode>},
    NonConstType {t: Box <TypeBodyNode>}
}

#[derive(Debug)]
pub struct TypeBodyNode {
    pub loc: CodeLocation,
    pub d: TypeBodyDetail,
}

#[derive(Debug)]
pub enum TypeBodyDetail {
    Byte {},
    Void {},
    ROMPointer {t: Box <TypeBodyNode>},
    RAMPointer {t: Box <TypeBodyNode>},
    Array {size: Box<NumberNode>, t: Box <TypeBodyNode>},
}

#[derive(Debug)]
pub struct BlockNode {
    pub loc: CodeLocation,
    pub statements: Vec<InnerStatementNode>
}

#[derive(Debug)]
pub struct InnerStatementNode {
    pub loc: CodeLocation,
    pub d: InnerStatementDetail,
}

#[derive(Debug)]
pub enum InnerStatementDetail {
    ForeverLoop {body: Box <BlockNode>},
    WhileLoop {condition: Box <ConditionNode>, body: Box <BlockNode>},
    DoWhileLoop {body: Box <BlockNode>, condition: Box <ConditionNode>},
    IfStatement {condition: Box <ConditionNode>, r#true: Box <BlockNode>},
    IfElseStatement {condition: Box <ConditionNode>, r#true: Box <BlockNode>, r#false: Box <BlockNode>},
    DeclarationStatement {r#type: Box <TypeNode>, identifier: String, val: Box <ExpressionNode>},
    AssignmentStatement{loc: Box <MemoryLocationNode>, val: Box <ExpressionNode>},
    ExpressionStatement {val: Box <ExpressionNode>},
    Block{body: Box <BlockNode>}
}

#[derive(Debug)]
pub struct ConditionNode {
    pub loc: CodeLocation,
    pub d: ConditionDetail,
}

#[derive(Debug)]
pub enum ConditionDetail {
    Branch {flag: BranchFlag},
    Expression {val: Box <ExpressionNode>}
}

#[derive(Debug)]
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



#[derive(Debug)]
pub struct ExpressionNode {
    pub loc: CodeLocation,
    pub d: ExpressionDetail,
}

#[derive(Debug)]
pub enum ExpressionDetail{
    Number {val: Box<NumberNode>},
    Array {val: Vec<Box <ExpressionNode>>}, // array in braces.
    MemoryValue {val: Box <MemoryLocationNode>},
    MemoryReference {val: Box <MemoryLocationNode>}, // "&" followed by mem location
    LeftShift {val: Box <ExpressionNode>},
    RightShift {val: Box <ExpressionNode>},
    BitwiseNOT {val: Box <ExpressionNode>},
    LogicalNOT {val: Box <ExpressionNode>},
    Log2 {val: Box <ExpressionNode>},
    Add {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    Subtract {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    Multiply {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    Divide {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    Modulo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    BitwiseXOR {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    BitwiseAND {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    BitwiseOR {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    EqualTo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    LogicalOR {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    LogicalAND {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    SignedGreaterThanOrEqualTo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    SignedLessThanOrEqualTo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    SignedLessThan {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    SignedGreaterThan {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    UnsignedGreaterThanOrEqualTo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    UnsignedLessThanOrEqualTo {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    UnsignedLessThan {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
    UnsignedGreaterThan {left: Box <ExpressionNode>, right: Box <ExpressionNode>},
}


#[derive(Debug)]
pub struct MemoryLocationNode {
    pub loc: CodeLocation,
    pub d: MemoryLocationDetail,
}

#[derive(Debug)]
pub enum MemoryLocationDetail {
    Identifier {val: String},
    ROMDereference {val: Box <MemoryLocationNode>},
    RAMDereference {val: Box <MemoryLocationNode>},
    ArrayIndex {i: Box <ExpressionNode>, arr: Box <MemoryLocationNode>},
    ArraySlice {start: Box <ExpressionNode>, end: Box <ExpressionNode>, arr: Box <MemoryLocationNode>},
    ArraySliceNoStart {end: Box <ExpressionNode>, arr: Box <MemoryLocationNode>},
    ArraySliceNoEnd {start: Box <ExpressionNode>, arr: Box <MemoryLocationNode>},
}

#[derive(Debug)]
pub struct ConstantNode {
    pub loc: CodeLocation,
    pub d: ConstantDetail,
}

#[derive(Debug)]
pub enum ConstantDetail {
    Number {val: Box<NumberNode>},
    Array {val: Vec<Box <ConstantNode>>}
}

#[derive(Debug)]
pub struct NumberNode {
    pub loc: CodeLocation,
    pub val: u8
}


















impl std::fmt::Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        
        let children = match &self.d {
            ExpressionDetail::Number { val } => vec![("val".to_string(), (*val).to_string())],

            ExpressionDetail::Array { val } => val.iter().enumerate().map(|(i, n)| (i.to_string(), n.to_string())).collect(),

            ExpressionDetail::MemoryValue { val } |
            ExpressionDetail::MemoryReference { val } => todo!(),

            ExpressionDetail::LeftShift { val } |
            ExpressionDetail::RightShift { val } |
            ExpressionDetail::BitwiseNOT { val } |
            ExpressionDetail::LogicalNOT { val } |
            ExpressionDetail::Log2 { val } => vec![(String::from("val"), val.to_string())],

            ExpressionDetail::Add { left, right } |
            ExpressionDetail::Subtract { left, right } |
            ExpressionDetail::Multiply { left, right } |
            ExpressionDetail::Divide { left, right } |
            ExpressionDetail::Modulo { left, right } |
            ExpressionDetail::BitwiseXOR { left, right } |
            ExpressionDetail::BitwiseAND { left, right } |
            ExpressionDetail::BitwiseOR { left, right } |
            ExpressionDetail::EqualTo { left, right } |
            ExpressionDetail::LogicalOR { left, right } |
            ExpressionDetail::LogicalAND { left, right } |
            ExpressionDetail::SignedGreaterThanOrEqualTo { left, right } |
            ExpressionDetail::SignedLessThanOrEqualTo { left, right } |
            ExpressionDetail::SignedLessThan { left, right } |
            ExpressionDetail::SignedGreaterThan { left, right } |
            ExpressionDetail::UnsignedGreaterThanOrEqualTo { left, right } |
            ExpressionDetail::UnsignedLessThanOrEqualTo { left, right } |
            ExpressionDetail::UnsignedLessThan { left, right } |
            ExpressionDetail::UnsignedGreaterThan { left, right } => vec![(String::from("left"), left.to_string()), (String::from("right"), right.to_string())],
        };
        
        let kind = match &self.d {
            ExpressionDetail::Number { val: _ } => "Number".to_string(),
            ExpressionDetail::Array { val: _ } => "Array".to_string(),
            ExpressionDetail::MemoryValue { val: _ } => "MemoryValue".to_string(),
            ExpressionDetail::MemoryReference { val: _ } => "MemoryReference".to_string(),
            ExpressionDetail::LeftShift { val: _ } => "LeftShift".to_string(),
            ExpressionDetail::RightShift { val: _ } => "RightShift".to_string(),
            ExpressionDetail::BitwiseNOT { val: _ } => "BitwiseNOT".to_string(),
            ExpressionDetail::LogicalNOT { val: _ } => "LogicalNOT".to_string(),
            ExpressionDetail::Log2 { val: _ } => "Log2".to_string(),
            ExpressionDetail::Add { left: _, right: _ } => "Add".to_string(),
            ExpressionDetail::Subtract { left: _, right: _ } => "Subtract".to_string(),
            ExpressionDetail::Multiply { left: _, right: _} => "Multiply".to_string(),
            ExpressionDetail::Divide {left: _, right: _ } => "Divide".to_string(),
            ExpressionDetail::Modulo { left: _, right: _ } => "Modulo".to_string(),
            ExpressionDetail::BitwiseXOR { left: _, right: _ } => "BitwiseXOR".to_string(),
            ExpressionDetail::BitwiseAND { left: _, right: _ } => "BitwiseAND".to_string(),
            ExpressionDetail::BitwiseOR { left: _, right: _ } => "BitwiseOR".to_string(),
            ExpressionDetail::EqualTo { left: _, right: _ } => "EqualTo".to_string(),
            ExpressionDetail::LogicalOR { left: _, right: _ } => "LogicalOR".to_string(),
            ExpressionDetail::LogicalAND { left: _, right: _ } => "LogicalAND".to_string(),
            ExpressionDetail::SignedGreaterThanOrEqualTo { left: _, right: _ } => "SignedGreaterThanOrEqualTo".to_string(),
            ExpressionDetail::SignedLessThanOrEqualTo { left: _, right: _ } => "SignedLessThanOrEqualTo".to_string(),
            ExpressionDetail::SignedLessThan { left: _, right: _ } => "SignedLessThan".to_string(),
            ExpressionDetail::SignedGreaterThan { left: _, right: _ } => "SignedGreaterThan".to_string(),
            ExpressionDetail::UnsignedGreaterThanOrEqualTo { left: _, right: _ } => "UnsignedGreaterThanOrEqualTo".to_string(),
            ExpressionDetail::UnsignedLessThanOrEqualTo { left: _, right: _ } => "UnsignedLessThanOrEqualTo".to_string(),
            ExpressionDetail::UnsignedLessThan { left: _, right: _ } => "UnsignedLessThan".to_string(),
            ExpressionDetail::UnsignedGreaterThan { left: _, right: _ } => "UnsignedGreaterThan".to_string(),
        };

        let mut out = kind ;
        for (i, (role, tree_str)) in children.iter().enumerate() {
            out += "\n";
            let lines: Vec<String> = (role.clone().to_string() + ": " + &tree_str).split("\n").map(|s| s.to_string()).collect();
            for (j, line) in lines.iter().enumerate() {
                if i != children.len() - 1  {
                    if j == 0 {
                        out += "├╴";
                    } else {
                        out += "│ ";
                    }
                } else {
                    if j == 0 {
                        out += "└╴";
                    } else {
                        out += "  ";
                    }
                }
                out += line;
                if  j != lines.len() - 1 {
                    out += "\n";
                }
            }

        };

        write!(f, "{}", out)

    }
}

impl std::fmt::Display for NumberNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}