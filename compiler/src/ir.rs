use std::{collections::HashMap};


use itertools::enumerate;

use crate::common::{BranchFlag, CodeLocation};

pub type VarID = usize;
// pub type VarVersion = usize;
pub type BlockPointer = usize;

pub struct IntermediateRepresentation {
    pub global_constants: HashMap<VarID, Vec<u8>>,
    pub basic_blocks: Vec<BasicBlock>,
    pub id_to_ident: HashMap<VarID, String>,
    pub id_to_size: HashMap<VarID, usize>
}

impl IntermediateRepresentation {
    pub fn new() -> IntermediateRepresentation {
        IntermediateRepresentation {
            basic_blocks: vec![],
            global_constants:HashMap::new(),
            id_to_ident: HashMap::new(),
            id_to_size: HashMap::new()
        }
    }

    pub fn get_block(&self, ptr: BlockPointer) -> Option<&BasicBlock>{
        return self.basic_blocks.get(ptr);
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
    pub result: Option<VarID>,
}

pub enum StmtKind {
    Nullary(Nullary),
    Unary(Unary),
    Binary(Binary),
    // Phi(BlockPointer, BlockPointer)
}

pub enum StmtArg {
    Literal(u8),
    Variable(VarID),
    RAMPointer(VarID/*pointer */, VarID/*array within which pointed.*/),
    LocationOfVariable(VarID),
    Constant(Constant), // reference to rom variable
}

// pub struct StmtResult {
//     pub var_id: VarID,
//     pub offset: u8, // used if writing to an array.
// }

// pub struct Variable {
//     pub id: VarID,
//     // pub version: VarVersion
// }

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
            Unary::LeftShift => "LeftShift",
            Unary::RightShift => "RightShift",
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
            format!("{}[{}]", variable_to_string(array, ir), variable_to_string(pointer, ir))
        },
        StmtArg::LocationOfVariable(var_id) => {
            format!("@{}", variable_to_string(var_id, ir))
        }
        StmtArg::Constant(constant) => {
            format!("`{}`", variable_to_string(&constant.id, ir))
        },
    }
}

fn variable_to_string(var_id: &VarID, ir: &IntermediateRepresentation) -> String {
    match ir.id_to_ident.get(&var_id) {
        Some(ident) => format!("{}:{}", ident, var_id),
        None => format!(":{}", var_id),
    }
}




