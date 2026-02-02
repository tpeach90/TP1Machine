
// basic block analysis
//  generated definitions at end of block, and assosiated varids
//  killed varids


// basic block returns list of killed ids

use std::{collections::{HashMap, HashSet}, iter::zip, vec};

use argparse::parser::Var;

use crate::ir::{BasicBlock, BlockPointer, Branch, DefinitionID, IntermediateRepresentation, StmtKind, VarID};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Definition {
    id: DefinitionID,
    var_id: VarID,
    
}

fn get_surviving_definitions_from_within_block(block: &BasicBlock) -> Vec<Definition> {
    let mut def_map: HashMap<VarID, DefinitionID> = HashMap::new();
    for stmt in &block.statements {
        match &stmt.result {
            Some(var_id) => {
                // may overwrite definition from earlier in this block
                // this is intended - only interested in surviving definitions at block exit
                def_map.insert(*var_id, stmt.definition_id);
            },
            None => ()
        }
    }
    let mut definitions = vec![];
    for (var_id, definition_id) in def_map {
        definitions.push(Definition{
            id: definition_id,
            var_id: var_id
        });
    }
    return definitions;
}

// BlockPointer -> Vec<BlockPointer>
fn get_all_block_parents(ir: &IntermediateRepresentation) -> Vec<Vec<BlockPointer>> {

    let mut parents = vec![vec![];ir.basic_blocks.len()];
    for (i, block) in ir.basic_blocks.iter().enumerate() {
        match &block.branch {
            Some(Branch { flag:_, to:next_block_id }) => {
                parents[*next_block_id].push(i);
            }
            None => ()
        }
        match &block.continue_to {
            Some(next_block_id) => {
                parents[*next_block_id].push(i);
            }
            None => ()
        }
    }
    return parents;
}



pub struct ReachingDefinitions {
    in_set: HashSet<Definition>,
    out_set: HashSet<Definition>
}
fn get_reaching_definitions(ir: &IntermediateRepresentation) -> Vec<ReachingDefinitions> {

    // varkill set: all instances of these variables are killed, except for those with a def in define set
    let mut block_to_varkill_set: Vec<HashSet<DefinitionID>> = vec![];
    let mut block_to_define_set: Vec<Vec<Definition>> = vec![];
    for block in &ir.basic_blocks {
        let define_set = get_surviving_definitions_from_within_block(block);
        let mut varkill_set = HashSet::new();
        for defn in &define_set {
            varkill_set.insert(defn.var_id);
        }
        block_to_define_set.push(define_set);
        block_to_varkill_set.push(varkill_set);
    }

    let parents = get_all_block_parents(ir);

    // run iterative alg build these
    let mut block_to_out_set: Vec<HashSet<Definition>> = vec![HashSet::new();ir.basic_blocks.len()];
    let mut block_to_in_set: Vec<HashSet<Definition>> = vec![HashSet::new();ir.basic_blocks.len()];
    loop {
        let mut changes = false;
        for (i, _) in ir.basic_blocks.iter().enumerate() {
            // union of OUT defns from predecessors
            let mut in_set: HashSet<Definition> = HashSet::new();
            for predecessor_block_ptr in &parents[i] {
                in_set.extend(block_to_out_set.get(*predecessor_block_ptr).unwrap());
            }
            
            // remove definitions killed in this block
            let mut out_set = in_set.iter().filter(|defn|
                block_to_varkill_set[i].contains(&defn.var_id)
            ).cloned().collect::<HashSet<Definition>>();

            // add definitions defined in this block
            out_set.extend(block_to_define_set.get(i).unwrap());

            // update and check for changes
            if out_set != block_to_out_set[i] {
                block_to_out_set[i] = out_set;
                changes = true;
            }
            block_to_in_set[i] = in_set;
        }
        if !changes {
            break;
        }
    }

    // zip in and out set
    let mut defns: Vec<ReachingDefinitions> = vec![];
    for (in_set, out_set) in zip(block_to_in_set, block_to_out_set) {
        defns.push(ReachingDefinitions { in_set, out_set });
    }
    return defns;
}