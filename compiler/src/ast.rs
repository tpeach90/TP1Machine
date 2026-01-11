use std::{fmt::Debug, vec};

use crate::common::CodeLocation;

#[derive(Debug)]
pub struct ProgramNode {
    pub loc: CodeLocation,
    pub outer_statements: Vec<Box<OuterStatementNode>>,
    pub main: Box<BlockNode>,
}

#[derive(Debug)]
pub struct OuterStatementNode {
    pub loc: CodeLocation,
    pub d: OuterStatementDetail
}

#[derive(Debug)]
pub enum OuterStatementDetail {
    DeclarationStatement {r#type: Box<TypeNode>, identifier: String, val: Box<ConstantNode>},
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
    pub statements: Vec<Box<InnerStatementNode>>
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
    AssignmentStatement{lvalue: Box <MemoryLocationNode>, rvalue: Box <ExpressionNode>},
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
    FunctionCall {ident: String, args: Vec<Box<ExpressionNode>>},
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





struct NodeFormatData {
    children: Vec<(String, String)>, // role, inner Display
    kind: String,
}


impl std::fmt::Display for ProgramNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut children: Vec<(String, String)> = vec![];
        for (i, outer_statement) in self.outer_statements.iter().enumerate() {
            children.push((i.to_string(), (*outer_statement).to_string()));
        }
        children.push(("main".to_string(), self.main.to_string()));

        write!(f, "{}", format_node(NodeFormatData { children, kind: "Program".to_string() }))
    }
}


impl std::fmt::Display for OuterStatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            OuterStatementDetail::DeclarationStatement { r#type, identifier, val } => NodeFormatData {
                children: vec![
                    ("type".to_string(), r#type.to_string()),
                    ("identifier".to_string(), identifier.to_owned()),
                    ("val".to_string(), val.to_string())
                ],
                kind: "DeclarationStatement".to_string()
            }
        };
        write!(f, "{}", format_node(nfd))
    }
}

impl std::fmt::Display for TypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            TypeDetail::ConstType { t } => NodeFormatData{
                children:vec![("body".to_string(), t.to_string())],
                kind: "ConstType".to_string()
            },
            TypeDetail::NonConstType { t } => NodeFormatData{
                children:vec![("body".to_string(), t.to_string())],
                kind: "NonConstType".to_string()
            },
        };
        write!(f, "{}", format_node(nfd))
    }
}

impl std::fmt::Display for TypeBodyNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            TypeBodyDetail::Byte {  } => NodeFormatData{children: vec![], kind:"Byte".to_string()},
            TypeBodyDetail::Void {  } => NodeFormatData{children: vec![], kind:"Void".to_string()},
            TypeBodyDetail::ROMPointer { t } => NodeFormatData{children: vec![("inner".to_string(), t.to_string())], kind:"ROMPointer".to_string()},
            TypeBodyDetail::RAMPointer { t } => NodeFormatData{children: vec![("inner".to_string(), t.to_string())], kind:"RAMPointer".to_string()},
            TypeBodyDetail::Array { size, t } => NodeFormatData {
                children: vec![
                    ("size".to_string(), size.to_string()),
                    ("inner".to_string(), t.to_string())
                ],
                kind: "Array".to_string()
            }
        };
        write!(f, "{}", format_node(nfd))
    }
}

impl std::fmt::Display for ConstantNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            ConstantDetail::Number { val } => NodeFormatData{children: vec![("val".to_string(), val.to_string())], kind: "Number".to_string()},
            ConstantDetail::Array { val } => {
                let mut children = vec![];
                for (i, node) in val.iter().enumerate() {
                    children.push((i.to_string(), node.to_string()));
                }
                NodeFormatData { children, kind: "Array".to_string() }
            },
        };
        write!(f, "{}", format_node(nfd))
    }
}

impl std::fmt::Display for BlockNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut children = vec![];
        for (i, node) in self.statements.iter().enumerate() {
            children.push((i.to_string(), node.to_string()));
        }
        write!(f, "{}", format_node(NodeFormatData { children, kind: "Block".to_string() }))
    }
}

impl std::fmt::Display for InnerStatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            InnerStatementDetail::ForeverLoop { body } => NodeFormatData {
                children: vec![("body".to_string(), body.to_string())],
                kind: "ForeverLoop".to_string()
            },
            InnerStatementDetail::WhileLoop { condition, body } => NodeFormatData {
                children: vec![
                    ("condition".to_string(), condition.to_string()),
                    ("body".to_string(), body.to_string())
                ],
                kind: "WhileLoop".to_string()
            },
            InnerStatementDetail::DoWhileLoop { body, condition } => NodeFormatData {
                children: vec![
                    ("body".to_string(), body.to_string()),
                    ("condition".to_string(), condition.to_string()),
                ],
                kind: "DoWhileLoop".to_string() 
            },
            InnerStatementDetail::IfStatement { condition, r#true } => NodeFormatData {
                children: vec![
                    ("condition".to_string(), condition.to_string()),
                    ("true".to_string(), r#true.to_string()),
                ],
                kind: "IfStatement".to_string()
            },
            InnerStatementDetail::IfElseStatement { condition, r#true, r#false } => NodeFormatData {
                children: vec![
                    ("condition".to_string(), condition.to_string()),
                    ("true".to_string(), r#true.to_string()),
                    ("false".to_string(), r#false.to_string()),
                ],
                kind: "IfElseStatement".to_string()
            },
            InnerStatementDetail::DeclarationStatement { r#type, identifier, val } => NodeFormatData {
                children: vec![
                    ("type".to_string(), r#type.to_string()),
                    ("identifier".to_string(), identifier.to_string()),
                    ("val".to_string(), val.to_string()),
                ],
                kind: "DeclarationStatement".to_string()
            },
            InnerStatementDetail::AssignmentStatement { lvalue, rvalue } => NodeFormatData {
                children: vec![
                    ("lvalue".to_string(), lvalue.to_string()),
                    ("rvalue".to_string(), rvalue.to_string()),
                ],
                kind: "AssignmentStatement".to_string()
            },
            InnerStatementDetail::ExpressionStatement { val } => NodeFormatData {
                children: vec![
                    ("val".to_string(), val.to_string()),
                ],
                kind: "ExpressionStatement".to_string()
            },
            InnerStatementDetail::Block { body } => NodeFormatData {
                children: vec![
                    ("body".to_string(), body.to_string()),
                ],
                kind: "Block".to_string()
            },
        };
        write!(f, "{}", format_node(nfd))
    }
}

impl std::fmt::Display for MemoryLocationNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            MemoryLocationDetail::Identifier { val } => NodeFormatData {
                children: vec![("val".to_string(), val.to_owned())],
                kind: "Identifier".to_string()
            },
            MemoryLocationDetail::ROMDereference { val } => NodeFormatData {
                children: vec![("val".to_string(), val.to_string())],
                kind: "ROMDereference".to_string()
            },
            MemoryLocationDetail::RAMDereference { val } => NodeFormatData {
                children: vec![("val".to_string(), val.to_string())],
                kind: "RAMDereference".to_string()
            },
            MemoryLocationDetail::ArrayIndex { i, arr } => NodeFormatData {
                children: vec![
                    ("i".to_string(), i.to_string()),
                    ("arr".to_string(), arr.to_string()),
                ],
                kind: "ArrayIndex".to_string()
            },
            MemoryLocationDetail::ArraySlice { start, end, arr } => NodeFormatData {
                children: vec![
                    ("start".to_string(), start.to_string()),
                    ("end".to_string(), end.to_string()),
                    ("arr".to_string(), arr.to_string()),
                ],
                kind: "ArraySlice".to_string()
            },
            MemoryLocationDetail::ArraySliceNoStart { end, arr } => NodeFormatData {
                children: vec![
                    ("end".to_string(), end.to_string()),
                    ("arr".to_string(), arr.to_string()),
                ],
                kind: "ArraySliceNoStart".to_string()
            },
            MemoryLocationDetail::ArraySliceNoEnd { start, arr } => NodeFormatData {
                children: vec![
                    ("start".to_string(), start.to_string()),
                    ("arr".to_string(), arr.to_string()),
                ],
                kind: "ArraySliceNoEnd".to_string()
            },
        };
        write!(f, "{}", format_node(nfd))

    }
}


impl std::fmt::Display for ConditionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nfd = match &self.d {
            ConditionDetail::Branch { flag } => NodeFormatData {
                children: vec![
                    ("flag".to_string(), flag.to_string()),
                ],
                kind: "Branch".to_string()
            },
            ConditionDetail::Expression { val } => NodeFormatData {
                children: vec![
                    ("val".to_string(), val.to_string()),
                ],
                kind: "Expression".to_string()
            }
        };
        write!(f, "{}", format_node(nfd))

    }
}

impl std::fmt::Display for BranchFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match &self {
            BranchFlag::BZ => "BZ".to_string(),
            BranchFlag::BNZ => "BNZ".to_string(),
            BranchFlag::BC => "BC".to_string(),
            BranchFlag::BNC => "BNC".to_string(),
            BranchFlag::BN => "BN".to_string(),
            BranchFlag::BNN => "BNN".to_string(),
            BranchFlag::BO => "BO".to_string(),
            BranchFlag::BNO => "BNO".to_string(),
            BranchFlag::BGTE => "BGTE".to_string(),
            BranchFlag::BNGTE => "BNGTE".to_string(),
        })
    }
}


impl std::fmt::Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        
        let children = match &self.d {
            ExpressionDetail::Number { val } => vec![("val".to_string(), (*val).to_string())],

            ExpressionDetail::Array { val } => val.iter().enumerate().map(|(i, n)| (i.to_string(), n.to_string())).collect(),
            ExpressionDetail::FunctionCall { ident, args } => {
                let mut children = vec![("ident".to_string(), ident.to_owned())];
                for (i, arg) in args.iter().enumerate() {
                    children.push((i.to_string(), arg.to_string()))
                }
                children
            },       

            ExpressionDetail::MemoryValue { val } |
            ExpressionDetail::MemoryReference { val } => vec![("val".to_string(), val.to_string())],

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
            ExpressionDetail::FunctionCall { ident: _, args: _ } => "FunctionCall".to_string(),
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

        let nfd = NodeFormatData{children, kind};

        write!(f, "{}", format_node(nfd))

    }
}

impl std::fmt::Display for NumberNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

fn format_node(nfd: NodeFormatData) -> String {
    let mut out = nfd.kind ;

    for (i, (role, tree_str)) in nfd.children.iter().enumerate() {
        out += "\n";
        let lines: Vec<String> = (role.clone().to_string() + ": " + &tree_str).split("\n").map(|s| s.to_string()).collect();
        for (j, line) in lines.iter().enumerate() {
            if i != nfd.children.len() - 1  {
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

    return out;
}