use std::{collections::{HashMap, HashSet}, format, vec};


use itertools::enumerate;

use crate::common::{BranchFlag, CodeLocation};

pub type VarID = usize;
pub type DefinitionID = usize;
// pub type VarVersion = usize;
pub type BlockPointer = usize;

pub struct IntermediateRepresentation {
    pub global_constants: HashMap<VarID, Vec<u8>>,
    pub basic_blocks: Vec<BasicBlock>,
    pub id_to_ident: HashMap<VarID, String>,
    pub id_to_size: HashMap<VarID, usize>,
    definition_index: HashMap<DefinitionID, DefinitionLocation>
}

#[derive(Clone, Copy)]
pub struct DefinitionLocation {
    pub block_id: usize,
    pub stmt_idx_in_block: usize
}

impl IntermediateRepresentation {
    pub fn new() -> IntermediateRepresentation {
        IntermediateRepresentation {
            basic_blocks: vec![],
            global_constants:HashMap::new(),
            id_to_ident: HashMap::new(),
            id_to_size: HashMap::new(),
            definition_index: HashMap::new()
        }
    }

    pub fn get_block(&self, ptr: BlockPointer) -> Option<&BasicBlock>{
        return self.basic_blocks.get(ptr);
    }

    pub fn reindex_definitions(&mut self) {
        let mut definition_index = HashMap::<DefinitionID, DefinitionLocation>::new();
        for (block_id, block) in self.basic_blocks.iter().enumerate() {
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                definition_index.insert(stmt.definition_id, DefinitionLocation{ block_id, stmt_idx_in_block: stmt_idx });
            }
        }
        self.definition_index = definition_index;
    }

    pub fn get_definition(&self, id: DefinitionID) -> Option<(DefinitionLocation, &Stmt)> {
        match self.definition_index.get(&id) {
            Some(loc) =>
                match self.basic_blocks.get(loc.block_id) {
                    Some(block) => match block.statements.get(loc.stmt_idx_in_block) {
                        Some(stmt) if stmt.definition_id == id => Some((*loc, stmt)),
                        _ => None
                    },
                    None => None,
                },
            None => None,
        }
    }

    pub fn to_dot_code(&self) -> String {
        // https://stackoverflow.com/questions/53468814/how-can-i-influence-graphviz-dot-to-make-nicer-control-flow-graphs-by-removing-s
        let mut out = "digraph G {
  mclimit=1.5;
  rankdir=TD; ordering=out;
  graph[fontsize=10 fontname=\"Verdana\"];
  color=\"#efefef\";
  node[shape=box style=filled fontsize=8 fontname=\"Verdana\" fillcolor=\"#efefef\"];
  edge[fontsize=8 fontname=\"Verdana\"];
".to_string();

        // nodes
        for (ptr, _) in enumerate(&self.basic_blocks) {
            let mut label = block_to_string(ptr, &self);
            label = label.replace("\\", "\\\\");
            label = label.replace("\"", "\\\"");
            label = label.replace("\n", "\\l");
            out += &format!("node_{} [label=\"{}\"];\n", ptr, label);
        }

        // edges
        for (ptr, block) in enumerate(&self.basic_blocks) {
            match (&block.branch, block.continue_to) {
                (None, None) => (),
                (None, Some(cont)) => {
                    out += &format!("node_{}:s -> node_{}:n [weight=\"1\"]\n", ptr, cont);
                },
                (Some(Branch{flag:_, to: block_if_true}), None) => {
                    out += &format!("node_{}:s -> node_{}:n [weight=\"1\"][color=\"green\"]\n", ptr, block_if_true);
                }
                (Some(Branch{flag:_, to:block_if_true}), Some(block_if_false)) => {
                    out += &format!("node_{}:sw -> node_{}:n [weight=\"1\"][color=\"red\"]\n", ptr, block_if_false);
                    out += &format!("node_{}:se -> node_{}:n [weight=\"1\"][color=\"green\"]\n", ptr, block_if_true);
                }
            }
        }

        out += "}";

        out
    }
}

pub struct BasicBlock {
    pub statements: Vec<Stmt>,
    
    pub branch: Option<Branch>,
    pub continue_to: Option<BlockPointer>,

    // variables set during optimization phase
    pub resets_alu_flags: bool
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            branch: None,
            continue_to: None,
            statements: vec![],
            resets_alu_flags: false
        }
    }
}

#[derive(Clone)]
pub struct Branch {
    pub flag: BranchFlag,
    pub to: BlockPointer // BasicBlock
}

#[derive(Clone)]
pub struct Stmt {
    pub loc: CodeLocation, // origin from input source code
    pub kind: StmtKind,
    pub arg1: Option<StmtArg>,
    pub arg2: Option<StmtArg>,
    pub result: Option<VarID>,
    pub definition_id: DefinitionID
}

impl Stmt {
    pub fn new(kind:StmtKind, arg1: Option<StmtArg>, arg2: Option<StmtArg>, result: Option<VarID>, loc: CodeLocation) -> Stmt{
        Stmt { loc, kind, arg1, arg2, result, definition_id: 0}
    }

    pub fn vars_used(&self) -> HashSet<VarID> {
        let mut vars_used = HashSet::<VarID>::new();
        match &self.arg1 {
            Some(arg) => vars_used.extend(arg.vars_used()),
            None => (),
        }
        match &self.arg2 {
            Some(arg) => vars_used.extend(arg.vars_used()),
            None => (),
        }
        vars_used
    }
}

#[derive(Clone, Hash)]
pub enum StmtKind {
    Nullary(Nullary),
    Unary(Unary),
    Binary(Binary),
    // Phi(BlockPointer, BlockPointer)
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum StmtArg {
    Literal(u8),
    Variable(VarID),
    RAMPointer(Box<StmtArg>/*pointer */, VarID/*array within which pointed.*/),
    LocationOfVariable(VarID),
    Constant(Constant), // reference to rom variable
    FoldedLiteral(Box<FoldedLiteral>)
}
impl StmtArg {
    pub fn uses_value_of_var(&self, var: VarID) -> bool {
        match self {
            StmtArg::Variable(var1) => *var1 == var,
            StmtArg::RAMPointer(stmt_arg, arr) => *arr == var || stmt_arg.uses_value_of_var(var),
            StmtArg::Literal(_) |
            StmtArg::LocationOfVariable(_) |
            StmtArg::Constant(_) |
            StmtArg::FoldedLiteral(_) => false,
        }
    }

    pub fn vars_used(&self) -> HashSet<VarID> {
        match self {
            StmtArg::Literal(_) |
            StmtArg::LocationOfVariable(_) |
            StmtArg::Constant(_) |
            StmtArg::FoldedLiteral(_) => HashSet::new(),
            StmtArg::Variable(var_id) => HashSet::from([*var_id]),
            StmtArg::RAMPointer(stmt_arg, var_id) => {
                let mut vars = stmt_arg.vars_used();
                vars.insert(*var_id);
                vars
            }
        }
    }
}

// Created from constant folding optimization.
// Don't evaluate until code generation due to LocationOfVariable not yet known
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum FoldedLiteral {
    Literal(u8),
    LocationOfVariable(VarID),
    BitwiseNOT(Box<FoldedLiteral>),
    Log2(Box<FoldedLiteral>),
    Increment(Box<FoldedLiteral>),
    Decrement(Box<FoldedLiteral>),
    TwosComplement(Box<FoldedLiteral>),
    Add(Box<FoldedLiteral>, Box<FoldedLiteral>),
    Subtract(Box<FoldedLiteral>, Box<FoldedLiteral>),
    Multiply(Box<FoldedLiteral>, Box<FoldedLiteral>),
    Divide(Box<FoldedLiteral>, Box<FoldedLiteral>),
    Modulo(Box<FoldedLiteral>, Box<FoldedLiteral>),
    BitwiseXOR(Box<FoldedLiteral>, Box<FoldedLiteral>),
    BitwiseAND(Box<FoldedLiteral>, Box<FoldedLiteral>),
    BitwiseOR(Box<FoldedLiteral>, Box<FoldedLiteral>),
}

// pub struct StmtResult {
//     pub var_id: VarID,
//     pub offset: u8, // used if writing to an array.
// }

// pub struct Variable {
//     pub id: VarID,
//     // pub version: VarVersion
// }

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Constant {
    pub id: VarID,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub enum Nullary {
    UpdateScreen,
    Wait,
    GetInput,
    Stop,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub enum Unary {
    Copy,
    Increment,
    Decrement,
    // LeftShift,
    // RightShift,
    TwosComplement,
    Compare,
    BitwiseNOT,
    Log2,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
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
impl Binary {
    pub fn is_commutative(&self) -> bool {
        match self {
            Binary::RAMWrite |
            Binary::Subtract |
            Binary::Divide |
            Binary::Modulo => false,
            Binary::Add |
            Binary::Multiply |
            Binary::BitwiseXOR |
            Binary::BitwiseAND |
            Binary::BitwiseOR => true,
        }
    }
}

fn block_to_string(block_ptr: BlockPointer, ir: &IntermediateRepresentation) -> String {
    let block = match ir.get_block(block_ptr){
        Some(block) => block,
        None => return "".to_string(),
    };

    let mut out = format!("Block {}\n", block_ptr);
    for stmt in &block.statements {
        out = format!("{}{}\n", out, stmt_to_string(stmt, ir));
    }
    match block.branch {
        Some(Branch{flag, to:_}) => {
            out = format!("{}{}\n", out, flag);
        },
        None => (),
    }

    out

}


fn stmt_to_string(stmt: &Stmt, ir: &IntermediateRepresentation) -> String {
    let instruction_identifier = match &stmt.kind {
        StmtKind::Nullary(nullary) => match nullary {
            Nullary::UpdateScreen => "UpdateScreen",
            Nullary::Wait => "Wait",
            Nullary::GetInput => "GetInput",
            Nullary::Stop => "Stop",
        },
        StmtKind::Unary(unary) => match unary {
            Unary::Copy => "Copy",
            Unary::Increment => "Increment",
            Unary::Decrement => "Decrement",
            // Unary::LeftShift => "LeftShift",
            // Unary::RightShift => "RightShift",
            Unary::TwosComplement => "TwosComplement",
            Unary::Compare => "Compare",
            Unary::BitwiseNOT => "BitwiseNOT",
            Unary::Log2 => "Log2",
        },
        StmtKind::Binary(binary) => match binary {
            Binary::RAMWrite => "RAMWrite",
            Binary::Add => "Add",
            Binary::Subtract => "Subtract",
            Binary::Multiply => "Multiply",
            Binary::Divide => "Divide",
            Binary::Modulo => "Modulo",
            Binary::BitwiseXOR => "BitwiseXOR",
            Binary::BitwiseAND => "BitwiseAND",
            Binary::BitwiseOR => "BitwiseOR",
        },
    };
    
    let mut out = String::new();
    match stmt.result {
        Some(var_id) => {
            out = format!("{}{} = ", out, variable_to_string(&var_id, ir));
        }
        None => (),
    }
    out = format!("{}{}", out, instruction_identifier);

    let arg_1_str = match &stmt.arg1 {
        Some(stmt) => Some(arg_to_string(&stmt, ir)),
        None => None,
    };
    let arg_2_str = match &stmt.arg2 {
        Some(stmt) => Some(arg_to_string(&stmt, ir)),
        None => None,
    };

    match arg_1_str {
        Some(s) => {
            out = format!("{} {}", out, s);
            if arg_2_str.is_some() {
                out = format!("{},", out);
            }
        },
        None => (),
    }
    match arg_2_str {
        Some(s) => {
            out = format!("{} {}", out, s);
        },
        None => (),
    }

    out
    
}


fn arg_to_string(stmt_arg: &StmtArg, ir: &IntermediateRepresentation) -> String {
    match stmt_arg {
        StmtArg::Literal(val) => {
            val.to_string()
        }
        StmtArg::Variable(var_id) => {
            variable_to_string(var_id, ir)
        },
        StmtArg::RAMPointer(pointer, array) => {
            format!("{}[{}]", variable_to_string(array, ir), arg_to_string(pointer, ir))
        },
        StmtArg::LocationOfVariable(var_id) => {
            format!("@{}", variable_to_string(var_id, ir))
        }
        StmtArg::Constant(constant) => {
            format!("`{}`", variable_to_string(&constant.id, ir))
        },
        StmtArg::FoldedLiteral(folded_literal) => {
            folded_literal_to_string(folded_literal, ir)
        },
    }
}

fn folded_literal_to_string(fl: &Box<FoldedLiteral>, ir: &IntermediateRepresentation) -> String {
    let kind = match **fl {
        FoldedLiteral::Literal(_) => "Literal",
        FoldedLiteral::LocationOfVariable(_) => "LocationOfVariable",
        FoldedLiteral::BitwiseNOT(_) => "BitwiseNOT",
        FoldedLiteral::Log2(_) => "Log2",
        FoldedLiteral::Add(_, _) => "Add",
        FoldedLiteral::Subtract(_, _) => "Subtract",
        FoldedLiteral::Multiply(_, _) => "Multiply",
        FoldedLiteral::Divide(_, _) => "Divide",
        FoldedLiteral::Modulo(_, _) => "Modulo",
        FoldedLiteral::BitwiseXOR(_, _) => "BitwiseXOR",
        FoldedLiteral::BitwiseAND(_, _) => "BitwiseAND",
        FoldedLiteral::BitwiseOR(_, _) => "BitwiseOR",
        FoldedLiteral::Decrement(_) => "Decrement",
        FoldedLiteral::Increment(_) => "Increment",
        FoldedLiteral::TwosComplement(_) => "TwosComplement"
    };
    match **fl {
        FoldedLiteral::Literal(l) => 
            format!("`{}`", l),
        FoldedLiteral::LocationOfVariable(var_id) => 
            format!("@{}", variable_to_string(&var_id, ir)),
        FoldedLiteral::Increment(ref folded_literal) |
        FoldedLiteral::Decrement(ref folded_literal) |
        FoldedLiteral::TwosComplement(ref folded_literal) |
        FoldedLiteral::BitwiseNOT(ref folded_literal) |
        FoldedLiteral::Log2(ref folded_literal) =>
            format!("{}({})", kind, folded_literal_to_string(&folded_literal, ir)),
        FoldedLiteral::Add(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::Subtract(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::Multiply(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::Divide(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::Modulo(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::BitwiseXOR(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::BitwiseAND(ref folded_literal, ref folded_literal1) |
        FoldedLiteral::BitwiseOR(ref folded_literal, ref folded_literal1) =>
            format!("{}({},{})", kind, folded_literal_to_string(&folded_literal, ir), folded_literal_to_string(&folded_literal1, ir)),
    }
}

fn variable_to_string(var_id: &VarID, ir: &IntermediateRepresentation) -> String {
    match ir.id_to_ident.get(&var_id) {
        Some(ident) => format!("{}:{}", ident, var_id),
        None => format!(":{}", var_id),
    }
}




