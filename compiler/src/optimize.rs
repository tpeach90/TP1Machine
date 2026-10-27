
// basic block analysis
//  generated definitions at end of block, and assosiated varids
//  killed varids


// basic block returns list of killed ids

use std::{collections::{HashMap, HashSet}, format, iter::zip, matches, vec};
use std::hash::{DefaultHasher, Hash, Hasher};

use enumerable::Enumerable;
use itertools::Itertools;

use crate::{common::{BranchFlag, CodeLocation}, ir::{BasicBlock, Binary, BlockPointer, Branch, DefinitionID, FoldedLiteral, IntermediateRepresentation, Nullary, Stmt, StmtArg, StmtKind, Unary, VarID}};

pub struct OptimizationError {
    pub loc: CodeLocation,
    pub message: String,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Definition {
    id: DefinitionID,
    var_id: VarID,
    
}

pub fn optimize(ir: &mut IntermediateRepresentation) -> Result<(), OptimizationError> {

    ir.reindex_definitions();

    loop {
        let mut changes = false;

        if branch_cleanup(ir) {
            changes = true;
        }

        if eliminiate_no_effect_code(ir)? {
            changes = true;
        }

        for i in 0..ir.basic_blocks.len() {
            // optimize single block
            if local_value_numbering_optimization_algorithm_with_extensions(ir, i)? {
                changes = true
            }
        }

        ir.reindex_definitions();
        if global_copy_propagation(ir) {
            changes = true;
        }

        if !changes {
            break;
        }
    }
    
    Ok(())
}

fn get_downwards_exposed_definitions_vec(block: &BasicBlock) -> Vec<Definition> {
    let mut definitions = vec![];
    for (var_id, definition_id) in get_downwards_exposed_definitions(block) {
        definitions.push(Definition{
            id: definition_id,
            var_id: var_id
        });
    }
    return definitions;
}

// definitions created in this block, and not later killed in this block
fn get_downwards_exposed_definitions(block: &BasicBlock) -> HashMap<VarID, DefinitionID> {
    get_downwards_exposed_definitions_after_n_stmts(block, block.statements.len())
}

fn get_downwards_exposed_definitions_after_n_stmts(block: &BasicBlock, n: usize) -> HashMap<VarID, DefinitionID> {
    let mut def_map: HashMap<VarID, DefinitionID> = HashMap::new();
    for (_, stmt) in (0..n).zip(&block.statements) {
        match &stmt.result {
            Some(var_id) => {
                // may overwrite definition from earlier in this block
                // this is intended - only interested in downwards exposed definitions at block exit
                def_map.insert(*var_id, stmt.definition_id);
            },
            None => ()
        }
    }
    return def_map;
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



pub struct DefinitionsAtBlockEnds {
    in_set: HashSet<Definition>,
    out_set: HashSet<Definition>
}
// reaching definitions for every block. Returns a vector indexed by block id
fn get_reaching_definitions(ir: &IntermediateRepresentation) -> Vec<DefinitionsAtBlockEnds> {

    // varkill set: all instances of these variables are killed, except for those with a def in define set
    let mut block_to_varkill_set: Vec<HashSet<DefinitionID>> = vec![];
    let mut block_to_define_set: Vec<Vec<Definition>> = vec![];
    for block in &ir.basic_blocks {
        let define_set = get_downwards_exposed_definitions_vec(block);
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
                !block_to_varkill_set[i].contains(&defn.var_id)
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
    let mut defns: Vec<DefinitionsAtBlockEnds> = vec![];
    for (in_set, out_set) in zip(block_to_in_set, block_to_out_set) {
        defns.push(DefinitionsAtBlockEnds { in_set, out_set });
    }
    return defns;
}

// returns (live_out for every block, live_in for block 0)
fn get_live_variables(ir: &IntermediateRepresentation) -> (Vec<HashSet<VarID>>, HashSet<VarID>) {

    let mut block_to_upwards_exposed_set: Vec<HashSet<VarID>> = vec![HashSet::new();ir.basic_blocks.len()];
    let mut block_to_define_set: Vec<HashSet<VarID>> = vec![HashSet::new();ir.basic_blocks.len()];
    for (i, block) in ir.basic_blocks.iter().enumerate() {
        for stmt in &block.statements {
            for var_id in &stmt.vars_used() {
                if !block_to_define_set[i].contains(var_id) {
                    block_to_upwards_exposed_set[i].insert(*var_id);
                }
            }
            match stmt.result {
                Some(var_id) => {
                    block_to_define_set[i].insert(var_id);
                },
                None => (),
            }
        }
    }

    // run iterative alg build these
    let mut block_to_live_out_set: Vec<HashSet<VarID>> = vec![HashSet::new();ir.basic_blocks.len()];
    
    loop {
        let mut changes = false;
        for (i, block) in ir.basic_blocks.iter().enumerate() {

            let mut live_out = HashSet::<VarID>::new();

            match &block.continue_to {
                Some(block_ptr) => {
                    live_out.extend(compute_live_in(*block_ptr, &block_to_upwards_exposed_set, &block_to_live_out_set, &block_to_define_set));
                },
                None => (),
            }
            match &block.branch {
                Some(Branch { to: block_ptr, .. }) => {
                    live_out.extend(compute_live_in(*block_ptr, &block_to_upwards_exposed_set, &block_to_live_out_set, &block_to_define_set));
                },
                None => (),
            }

            // update and check for changes
            if live_out != block_to_live_out_set[i] {
                block_to_live_out_set[i] = live_out;
                changes = true;
            }

        }
        if !changes {
            break;
        }
    }

    let live_in_block_0 = if ir.basic_blocks.len() > 0 {
        compute_live_in(0, &block_to_upwards_exposed_set, &block_to_live_out_set, &block_to_define_set)
    } else {
        HashSet::<VarID>::new()
    };

    (block_to_live_out_set, live_in_block_0)

    
}

fn compute_live_in(block_ptr: BlockPointer, block_to_upwards_exposed_set: &Vec<HashSet<VarID>>, block_to_live_out_set: &Vec<HashSet<VarID>>, block_to_define_set: &Vec<HashSet<VarID>>) -> HashSet<usize> {
    let mut live_in = block_to_upwards_exposed_set[block_ptr].clone();
    for var_id in &block_to_live_out_set[block_ptr] {
        if !block_to_define_set[block_ptr].contains(var_id) {
            live_in.insert(*var_id);
        }
    };
    live_in
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct CopyOp {
    source: StmtArg,
    destination: VarID,
}

fn stmt_to_copy_op(stmt: &Stmt) -> Option<CopyOp> {
    match stmt {
        Stmt { kind: StmtKind::Unary(Unary::Copy), arg1: Some(arg), arg2: None, result: Some(result_var_id), .. } => {
            Some(CopyOp { source: arg.clone(), destination: *result_var_id })
        }
        _ => None
    }
}

// Simialar to computing available expressions (page 490 in Engineering a Compiler 2nd ed, Keith Cooper et al), except we're dealing with whole copy statements (including the result var_id), and copy statements can be killed if either the source or destination is redefined.
fn get_available_copy_statements(ir: &IntermediateRepresentation) -> Vec<Vec<CopyOp>> {

    if ir.basic_blocks.len() == 0 {
        return vec![];
    }

    let mut block_to_downwards_exposed: Vec<HashSet<CopyOp>> = vec![];
    for block in &ir.basic_blocks {
        let mut downwards_exposed = HashSet::<CopyOp>::new();
        for stmt in &block.statements {
            // remove killed copy ops (either source or destination redefined)
            match stmt.result {
                Some(result_var_id) => {
                    downwards_exposed = downwards_exposed.into_iter().filter(|CopyOp { source: copy_op_source, destination: copy_op_destination }|
                        *copy_op_destination != result_var_id
                        && !copy_op_source.vars_used().contains(&result_var_id)
                    ).collect();
                }
                None => ()
            }
            // add this op if it is a copy
            match stmt_to_copy_op(stmt) {
                Some(copy_op) => {
                    downwards_exposed.insert(copy_op);
                },
                None => (),
            }
        }
        block_to_downwards_exposed.push(downwards_exposed);
    }

    let mut block_to_vars_killed: Vec<HashSet<VarID>> = vec![];
    for block in &ir.basic_blocks {
        let mut vars_killed = HashSet::<VarID>::new();
        for stmt in &block.statements {
            match stmt.result {
                Some(var_id) => {
                    vars_killed.insert(var_id);
                }
                None => ()
            }
        }
        block_to_vars_killed.push(vars_killed);
    }

    let mut all_copy_statements_hs = HashSet::<CopyOp>::new();
    for block in &ir.basic_blocks {
        for stmt in &block.statements {
            match stmt_to_copy_op(stmt) {
                Some(copy_op) => {
                    all_copy_statements_hs.insert(copy_op);
                }
                None => ()
            }
        }
    }
    let all_copy_statements = all_copy_statements_hs.into_iter().collect_vec();

    let parents = get_all_block_parents(ir);

    // avail_in will be generated iteratively
    let mut block_to_avail_in: Vec<Vec<CopyOp>> = vec![all_copy_statements.clone();ir.basic_blocks.len()];
    block_to_avail_in[0].clear();

    
    loop {
        let mut changes = false;
        // dont recalculate avail_in for block 0 because this must always be empty
        for block_ptr in 1..ir.basic_blocks.len() {

            let mut avail_in = block_to_avail_in[block_ptr].clone();

            for parent_block_ptr in &parents[block_ptr] {
                // downwards_exposed ∪ (avail_in - vars_killed) 
                let mut parent_avail_out = block_to_downwards_exposed[*parent_block_ptr].clone();
                parent_avail_out.extend(block_to_avail_in[*parent_block_ptr].iter().filter(|CopyOp { source, destination }|
                    // for each copy op, neither source or destination is killed in the parent.
                    block_to_vars_killed[*parent_block_ptr].intersection(&source.vars_used()).count() == 0
                    && !block_to_vars_killed[*parent_block_ptr].contains(destination)
                ).cloned());

                // intersect avail_in with parent_avail_out
                avail_in = avail_in.into_iter().filter(|copy_op| 
                    if parent_avail_out.contains(copy_op) {
                        true
                    } else {
                        changes = true;
                        false
                    }
                ).collect();
            }

            block_to_avail_in[block_ptr] = avail_in;
            
        }

        if !changes {
            break;
        }
    }

    return block_to_avail_in;

}

pub fn global_copy_propagation(ir: &mut IntermediateRepresentation) -> bool {

    let mut avail_copy_statements_all = get_available_copy_statements(ir);
    let mut changes = false;
    
    
    for block_id in (0..ir.basic_blocks.len()).rev() {
        let mut avail_copy_statements = avail_copy_statements_all[block_id].clone();
        let mut block_changes = false;

        let mut new_stmts: Vec<Stmt> = vec![];

        for stmt in &ir.basic_blocks[block_id].statements {
            
            // to be called on both args of the stmt
            let get_arg_replacement = |var_id_x: VarID| {

                let replacement = avail_copy_statements.iter().find(|CopyOp { destination, .. }| *destination == var_id_x).cloned();
                return match &replacement {
                    Some(CopyOp { source, .. }) => match source {
                        StmtArg::RAMPointer(_, _) => None, // propagating a pointer dereference might be slower & increase file size
                        StmtArg::Literal(_) |
                        StmtArg::Variable(_) |
                        StmtArg::LocationOfVariable(_) |
                        StmtArg::Constant(_) |
                        StmtArg::FoldedLiteral(_) => replacement,
                    },
                    None => None
                };
            };

            let mut new_stmt = stmt.clone();

            // replace args if copy propagation is found to be valid
            match stmt.arg1 {
                Some(StmtArg::Variable(var_id)) => match get_arg_replacement(var_id) {
                    Some(op) => {
                        new_stmt.arg1 = Some(op.source);
                        block_changes = true;
                    },
                    _ => ()
                },
                _ => ()
            };
            match stmt.arg2 {
                Some(StmtArg::Variable(var_id)) => match get_arg_replacement(var_id) {
                    Some(op) => {
                        new_stmt.arg2 = Some(op.source);
                        block_changes = true;
                    },
                    _ => ()
                },
                _ => ()
            };

            // remove avails killed by this statement
            match &new_stmt.result {
                Some(result_var_id) => {
                    avail_copy_statements = avail_copy_statements.into_iter().filter(|CopyOp { source, destination }|
                        destination != result_var_id 
                        && !source.uses_value_of_var(*result_var_id)
                    ).collect();
                },
                None => (),
            }

            // add avail if this was a copy statement
            match stmt_to_copy_op(&new_stmt) {
                Some(copy_op) => {
                    avail_copy_statements.push(copy_op);
                },
                None => (),
            }

            new_stmts.push(new_stmt);

            if block_changes {
                changes = true;
            }

        }

        ir.basic_blocks[block_id].statements = new_stmts;

        // copy ops might not be available now - need to recalculate everything
        if block_changes {
            avail_copy_statements_all = get_available_copy_statements(ir);
        }
    }

    // todo, does replacing instructions invalidate the avail statements?

    changes
}

// replace Stmt (S1) args if:
// 1. arg is a variable (x)
// 2. S1 has exactly 1 reaching definition of x, which is a copy (S0: x<-y)
// 3. WRONG!!!!! S0 and S1 both have a single identical reaching definition of y
// 3. CORRECT: S0 must not be killed on any path to S1 (y must not be redefined)
// in this case, in S1 x can be replaced with y
// assumptions this function uses:
// 1. there are no uninitialized variable errors in the program
// 2. ir definitnion index is up-to-date
pub fn global_copy_propagation_old(ir: &mut IntermediateRepresentation) -> Result<bool, OptimizationError> {
    let reaching_defns_all = get_reaching_definitions(ir);

    let mut changes = false;

    for block_id in 0..ir.basic_blocks.len() {
        let mut defns_at_s1 = reaching_defns_all[block_id].in_set.clone().into_iter().collect_vec();
        let mut new_stmts: Vec<Stmt> = vec![];

        for stmt_s1 in &ir.basic_blocks[block_id].statements {

            // fn to be called separately on arg1 and arg2
            let get_arg_replacement = |var_id_x: VarID| {
                // check arg has 1 reching definition
                // assumption: this is reaching over all paths - otherwise an uninitialized variable error would have been raised earlier in compilation
                let defns_of_x_at_s1 = defns_at_s1.iter().filter(|defn| defn.var_id == var_id_x).collect_vec();
                if defns_of_x_at_s1.len() == 0 {
                    return Err(OptimizationError{ loc: stmt_s1.loc, message: format!("Variable is not initialized")});
                } else if defns_of_x_at_s1.len() > 1 {
                    return Ok(None)
                }
                // find definition S0 and check if it is a copy x<-y, and that y is usable
                match ir.get_definition(defns_of_x_at_s1[0].id) {
                    Some((
                        def_loc, 
                        Stmt {
                            kind:StmtKind::Unary(Unary::Copy), 
                            arg1: Some(def_arg), 
                            ..
                        }
                    )) => {
                        match def_arg {
                            StmtArg::RAMPointer(_, _) => Ok(None), // propagating a pointer dereference might be slower & increase file size
                            StmtArg::Literal(_) |
                            StmtArg::LocationOfVariable(_) |
                            StmtArg::Constant(_) |
                            StmtArg::FoldedLiteral(_) => Ok(Some(def_arg.clone())),
                            StmtArg::Variable(var_id_y) => {
                                // check what definition of var_id_y this statement is using
                                // first check if var_id_y was defined earlier in the block
                                let defn_id_of_y_at_s0 = match get_downwards_exposed_definitions_after_n_stmts(
                                    &ir.basic_blocks[def_loc.block_id],
                                    def_loc.stmt_idx_in_block
                                ).get(var_id_y) {
                                    Some(defn_id) => *defn_id,
                                    None => {
                                        // var_id_y was not defined in the block before the copy statement
                                        // check if there is exactly 1 definition reaching the start of the block
                                        let defns_of_y_at_start_of_block_containing_s0 = reaching_defns_all[def_loc.block_id].in_set.iter().filter(|defn| defn.var_id == *var_id_y ).collect_vec();
                                        if defns_of_y_at_start_of_block_containing_s0.len() != 1 {
                                            return Ok(None)
                                        }
                                        defns_of_y_at_start_of_block_containing_s0[0].id
                                    },
                                };
                                // check that this definition is the only definition of var_id_y that reaches us
                                let defns_of_y_at_s1 = defns_at_s1.iter().filter(|defn| defn.var_id == *var_id_y).collect_vec();
                                if defns_of_y_at_s1.len() != 1 {
                                    return Ok(None);
                                }
                                if defns_of_y_at_s1[0].id != defn_id_of_y_at_s0 {
                                    return Ok(None);
                                }

                                // safe to propogate copy
                                Ok(Some(def_arg.clone()))
                            }
                        }

                    },
                    _ => Ok(None),
                }

            };

            let mut new_stmt_s1 = stmt_s1.clone();

            // replace args if copy propagation is found to be valid
            match stmt_s1.arg1 {
                Some(StmtArg::Variable(var_id)) => match get_arg_replacement(var_id)? {
                    Some(arg) => {
                        new_stmt_s1.arg1 = Some(arg);
                        changes = true;
                    },
                    _ => ()
                },
                _ => ()
            };
            match stmt_s1.arg2 {
                Some(StmtArg::Variable(var_id)) => match get_arg_replacement(var_id)? {
                    Some(arg) => {
                        new_stmt_s1.arg2 = Some(arg);
                        changes = true;
                    },
                    _ => ()
                },
                _ => ()
            };
            
            // remove definitions killed by this statement
            match new_stmt_s1.result {
                Some(result_var_id) => {
                    // todo slow
                    defns_at_s1 = defns_at_s1.into_iter().filter(|defn| defn.var_id != result_var_id).collect_vec();
                    defns_at_s1.push(Definition { id: new_stmt_s1.definition_id, var_id: result_var_id });
                },
                None => (),
            }

            new_stmts.push(new_stmt_s1);

            
        }

        ir.basic_blocks[block_id].statements = new_stmts;
        
    }

    Ok(changes)
}

enum StmtEffect {
    Effect,
    ALUFlagsOnly,
    NoEffect,
}

fn eliminiate_no_effect_code(ir: &mut IntermediateRepresentation) -> Result<bool, OptimizationError> {
    let (live_out_all, live_in_block_0) = get_live_variables(ir);

    if live_in_block_0.len() != 0 {
        let mut var_names = live_in_block_0.iter().map(|var_id| 
            ir.id_to_ident.get(var_id).unwrap_or(&"nameless var".to_string()).to_owned()
        );
        return Err(OptimizationError{ loc: CodeLocation { start_index: 0, end_index: 0 }, message: format!("The following variable(s) are potentially used before initialization: {}", var_names.join(", ")) });
    }

    let mut changes = false;

    for block_id in 0..ir.basic_blocks.len() {

        let must_preserve_flags_from_final_alu_instruction = ir.basic_blocks[block_id].branch.is_some() || !all_paths_out_of_block_reset_alu_flags_before_branching(ir, block_id)?;
        
        let mut has_processed_final_alu_instruction = false;

        // push statements in reverse order, then later reverse in-place
        let mut new_statements: Vec<Stmt> = vec![];

        let mut live = live_out_all[block_id].clone();

        // iterate through in reverse order
        for stmt in ir.basic_blocks[block_id].statements.iter().rev() {

            let effect = match stmt.kind {
                StmtKind::Nullary(Nullary::UpdateScreen) | 
                StmtKind::Nullary(Nullary::Wait) | 
                StmtKind::Nullary(Nullary::Stop) => StmtEffect::Effect,
                StmtKind::Nullary(Nullary::GetInput) | 
                StmtKind::Unary(Unary::Copy) => {
                    if stmt.result.is_some_and(|var_id| live.contains(&var_id)) {
                        StmtEffect::Effect
                    } else {
                        StmtEffect::NoEffect
                    }
                },
                StmtKind::Binary(Binary::RAMWrite) => {
                    // don't eliminate ramwrites with no Result, as this may be a screen update.
                    if stmt.result.is_none_or(|var_id| live.contains(&var_id)) {
                        StmtEffect::Effect
                    } else {
                        StmtEffect::NoEffect
                    }
                },
                StmtKind::Unary(Unary::Increment) |
                StmtKind::Unary(Unary::Decrement) |
                StmtKind::Unary(Unary::TwosComplement) |
                StmtKind::Unary(Unary::Compare) |
                StmtKind::Unary(Unary::BitwiseNOT) |
                StmtKind::Unary(Unary::Log2) |
                StmtKind::Binary(Binary::Add) |
                StmtKind::Binary(Binary::Subtract) |
                StmtKind::Binary(Binary::Multiply) |
                StmtKind::Binary(Binary::Divide) |
                StmtKind::Binary(Binary::Modulo) |
                StmtKind::Binary(Binary::BitwiseXOR) |
                StmtKind::Binary(Binary::BitwiseAND) |
                StmtKind::Binary(Binary::BitwiseOR) => {
                    let effect = if stmt.result.is_some_and(|var_id| live.contains(&var_id)) {
                        StmtEffect::Effect
                    } else if !has_processed_final_alu_instruction && must_preserve_flags_from_final_alu_instruction {
                        StmtEffect::ALUFlagsOnly
                    } else {
                        StmtEffect::NoEffect
                    };
                    has_processed_final_alu_instruction = true;
                    effect
                }
            };

            match effect {
                StmtEffect::Effect => {
                    // keep stmt
                    new_statements.push(stmt.clone());
                    match stmt.result {
                        Some(var_id) => {
                            live.remove(&var_id);
                        }
                        None => ()
                    };
                    live.extend(stmt.vars_used());
                },
                StmtEffect::ALUFlagsOnly => {
                    let mut new_statement = stmt.clone();
                    if new_statement.result.is_some() {
                        new_statement.result = None; // discard result
                        changes = true;
                    }
                    new_statements.push(new_statement);
                    live.extend(stmt.vars_used());
                },
                StmtEffect::NoEffect => {
                    // don't add to new_statements - eliminate
                    changes = true;
                },
            }
        }

        new_statements.reverse();
        ir.basic_blocks[block_id].statements = new_statements;
        
        
    }
    return Ok(changes);
}

pub fn all_paths_out_of_block_reset_alu_flags_before_branching(ir: &IntermediateRepresentation, block_ptr: BlockPointer) -> Result<bool, OptimizationError> {
    let mut visited = vec![false;ir.basic_blocks.len()];
    all_paths_out_of_block_reset_alu_flags_before_branching_1(ir, block_ptr, &mut visited)
}

pub fn all_paths_out_of_block_reset_alu_flags_before_branching_1(ir: &IntermediateRepresentation, block_ptr: BlockPointer, visited: &mut Vec<bool>) -> Result<bool, OptimizationError> {

    let block = &ir.basic_blocks[block_ptr];
    visited[block_ptr] = true;

    let branch_resets = match block.branch {
        Some(Branch { flag:_, to }) => {
            if ir.basic_blocks[to].resets_alu_flags {
                // succ resets flags
                true
            } else if ir.basic_blocks[to].branch.is_some() {
                // succ does not reset alu flags and branches
                false
            } else if !visited[to] {
                // succ does not reset alu flags and does not branch
                all_paths_out_of_block_reset_alu_flags_before_branching_1(ir, to, visited)?
            } else {
                // already visited succ,
                true // stop here otherwise we'll end in an infinite loop.
                // this path does not matter because it takes us to somewhere we're already investigating/ed
                // only issue is if we have looped back to the block we were investigating at the start
                // but this procedure is only useful if there is an alu instruction in the block that we're investigating
                // so assume true. (todo)
            }
        },
        None => true
    };
    let branch_and_continue_reset = branch_resets && match block.continue_to {
        Some(to) => {
            if ir.basic_blocks[to].resets_alu_flags {
                true
            } else if ir.basic_blocks[to].branch.is_some() {
                false
            } else if !visited[to] {
                all_paths_out_of_block_reset_alu_flags_before_branching_1(ir, to, visited)?
            } else {
                true 
            }
        }
        None => true
    };

    Ok(branch_and_continue_reset)

}

pub fn local_value_numbering_optimization_algorithm_with_extensions(ir: &mut IntermediateRepresentation, block_ptr: BlockPointer) -> Result<bool, OptimizationError> {

    let all_paths_out_of_block_reset_alu_flags_before_branching = all_paths_out_of_block_reset_alu_flags_before_branching(&ir, block_ptr)?;
    let block = ir.basic_blocks.get_mut(block_ptr).unwrap();
    let mut changes = false;

    loop {
        let mut changes_this_iteration = false;

        if simplify_independent_statements_in_block(block, all_paths_out_of_block_reset_alu_flags_before_branching) {
            changes_this_iteration = true;
            changes = true;
        }

        if subexpression_elimination_in_block(block, all_paths_out_of_block_reset_alu_flags_before_branching) {
            changes_this_iteration = true;
            changes = true;
        }

        if !changes_this_iteration {
            break;
        }
    }

    Ok(changes)

}

fn get_idx_of_final_alu_instruction(block: &BasicBlock) -> Option<usize> {
    let mut final_alu_stmt_i = None;
    for i in 0..block.statements.len() {
        if is_alu_instruction(&block.statements[i]) {
            final_alu_stmt_i = Some(i);
        }
    }
    final_alu_stmt_i
}

// returns true if changes
fn simplify_independent_statements_in_block(block: &mut BasicBlock, may_replace_final_alu_instruction: bool) -> bool {
    
    let mut changes = false;
    
    // find idx of last ALU instruction (which would set the flags before a potential branch)
    let final_alu_stmt_i = get_idx_of_final_alu_instruction(block);

    let mut new_statements: Vec<Stmt> = vec![];


    // simplify statements (algebraic identities, constant folding)
    // also simplify branches if possible
    for i in 0..block.statements.len() {

        let (maybe_new_stmt, maybe_flagset) = simplify_single_statement(&block.statements[i]);

        // some consideration is needed with replacing instructions that can alter the ALU flag status at the end of this block
        // specifically the *last* alu instruction.
        // it is safe to replace the last alu instruction in the following cases
        // 1. The ALU flags are guaranteed to be the same with the replacement as with the original
        // 2. The behaviour of the branch is the same with the replacement statement, AND all paths leaving the block reset the ALU before branching again
        // The branch can be eliminated entirely if the relevant flags are known, and all paths leaving the block reset the ALU before branching again.
        // Ignore the first case for now (todo) and only implement the second case
        
        let is_final_alu_stmt = final_alu_stmt_i.is_some_and(|fas_i| fas_i == i);
        if !is_final_alu_stmt {
            // replacement always valid
            match maybe_new_stmt {
                Some(new_stmt) => {
                    changes = true;
                    new_statements.push(new_stmt);
                }
                None => {
                    new_statements.push(block.statements[i].clone());
                }
            }
        } else if may_replace_final_alu_instruction {
            // replacement instruction might affect the ALU flags in this block
            // Check to see if enough information is available to ensure that the program will continue correctly.
            let replaced = match (maybe_new_stmt, &block.branch, &maybe_flagset) {
                (Some(new_stmt), None, _) => {
                    // branch flags are not used
                    new_statements.push(new_stmt);
                    changes = true;
                    true
                }
                (Some(new_stmt), Some(Branch{flag, to}), Some(pfs)) => match (flag, pfs) {
                    (flag, pfs) if branch_is_certain_to_be_taken(flag, pfs) => {
                        block.continue_to = Some(to.to_owned());
                        block.branch = None;
                        new_statements.push(new_stmt);
                        changes = true;
                        true
                    },
                    (flag, pfs) if branch_is_certain_not_to_be_taken(flag, pfs) => {
                        block.branch = None;
                        new_statements.push(new_stmt);
                        changes = true;
                        true
                    },
                    (BranchFlag::BZ, PartialFlagSet{zn_from_cmp: Some(stmt_arg), ..}) |
                    (BranchFlag::BNZ, PartialFlagSet{zn_from_cmp: Some(stmt_arg), ..}) |
                    (BranchFlag::BN, PartialFlagSet{zn_from_cmp: Some(stmt_arg), ..}) |
                    (BranchFlag::BNN, PartialFlagSet{zn_from_cmp: Some(stmt_arg), ..}) => {
                        // we can add a cmp instruction to get the Z and N flags
                        new_statements.push(new_stmt.clone());
                        new_statements.push(Stmt::new(StmtKind::Unary(Unary::Compare), Some(stmt_arg.clone()), None, None, new_stmt.loc));
                        changes = true;
                        true
                    },
                    (BranchFlag::BGTE, PartialFlagSet{zn_from_cmp: Some(stmt_arg), o:Some(true), ..}) |
                    (BranchFlag::BNGTE, PartialFlagSet{zn_from_cmp: Some(stmt_arg), o:Some(false), ..}) => {
                        new_statements.push(new_stmt.clone());
                        new_statements.push(Stmt::new(StmtKind::Unary(Unary::Compare), Some(stmt_arg.clone()), None, None, new_stmt.loc));
                        // already know about o flag, replace branch flag to solely depend on the new instruction
                        block.branch.as_mut().unwrap().flag = BranchFlag::BN;
                        changes = true;
                        true
                    },
                    (BranchFlag::BGTE, PartialFlagSet{zn_from_cmp: Some(stmt_arg), o:Some(false), ..}) |
                    (BranchFlag::BNGTE, PartialFlagSet{zn_from_cmp: Some(stmt_arg), o:Some(true), ..}) => {
                        new_statements.push(new_stmt.clone());
                        new_statements.push(Stmt::new(StmtKind::Unary(Unary::Compare), Some(stmt_arg.clone()), None, None, new_stmt.loc));
                        block.branch.as_mut().unwrap().flag = BranchFlag::BNN;
                        changes = true;
                        true
                    },
                    (BranchFlag::BZ, PartialFlagSet{z:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BNZ, PartialFlagSet{z:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BC, PartialFlagSet{c:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BNC, PartialFlagSet{c:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BN, PartialFlagSet{n:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BNN, PartialFlagSet{n:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BO, PartialFlagSet{o:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BNO, PartialFlagSet{o:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BGTE, PartialFlagSet{n: None, o:None, zcno_from_replacement_instruction: true, ..}) |
                    (BranchFlag::BNGTE, PartialFlagSet{n: None, o:None, zcno_from_replacement_instruction: true, ..}) => {
                        // replacement statement preserves relevant flags
                        new_statements.push(new_stmt);
                        changes = true;
                        true
                    },
                    (flag, PartialFlagSet{z, c, n, o, zcno_from_replacement_instruction: true, ..})  => {
                        // for BranchFlags that use more than one CCR flag, i.e. BGTE and BNGTE which depend on both N and O, we may be able to narrow the BranchFlag for the new instruction if there is a zcno_from_replacement_instruction
                        // convert flag to bits
                        //               mask       possible values
                        // e.g. BGTE => (0b0011, vec![0b0000, 0b0011]),
                        // for the bits in the mask that we already know, check if any of the possible value patterns match
                        // e.g. if we know mask=0010, value=0000
                        // value patterns that apply are [0b0000]
                        // the remaining unknown bits are 0011 & ¬0010 = 0001
                        // check to see if any of the BranchFlags match 0001, [0000]
                        // in this case yes, this is BNO
                        // we can narrow the flag to BNO with the replacement instruction

                        let mut known_mask: u8 = 0b0000; // ZCNO
                        let mut known_value: u8 = 0b0000;
                        for (bit, flag) in vec![z, c, n, o].iter().enumerate() {
                            let current_mask = (2_u8).pow(u32::try_from(3 - bit).unwrap());
                            match flag {
                                Some(true) => {
                                    known_mask |= current_mask;
                                    known_value |= current_mask;
                                },
                                Some(false) => {
                                    known_mask |= current_mask;
                                },
                                None => ()
                            }
                        }
                        let (req_mask, req_values) = branch_flag_to_required_mask_and_values(flag);
                        let common_mask = req_mask & known_mask;
                        let remaining_mask = req_mask & !known_mask;

                        let mut remaining_values: Vec<u8> = vec![];
                        for req_value in &req_values {
                            // check that the known bits match the required bits (if overlap)
                            if known_value & common_mask == req_value & common_mask {
                                let remaining_value = req_value & remaining_mask;
                                if !remaining_values.contains(&remaining_value) {
                                    remaining_values.push(remaining_value);
                                }
                            }
                        }

                        // see if there is an appropriate BranchFlag 
                        let replacement_branch_flag = mask_and_values_to_branch_flag(&remaining_mask, &remaining_values);

                        match replacement_branch_flag {
                            Some(new_flag) => {
                                new_statements.push(new_stmt);
                                block.branch.as_mut().unwrap().flag = new_flag;
                                changes = true;
                                true
                            },
                            None => {
                                // replacement not valid
                                false
                            },
                        }
                    },
                    _ => false
                },
                (None, Some(Branch{flag, to}), Some(pfs)) => {
                    // no replacement instruction possible, but some information is known about the flagset.
                    // e.g. bitwise operations will never set C or O flags
                    // could potentially do branch elimination
                    if branch_is_certain_to_be_taken(flag, pfs) {
                        block.continue_to = Some(to.to_owned());
                        block.branch = None;
                        changes = true;
                    } else if branch_is_certain_not_to_be_taken(flag, pfs) {
                        block.branch = None;
                        changes = true;
                    }

                    false // statement not replaced

                }
                _ => false
            };
            if !replaced {
                // replacement not valid
                new_statements.push(block.statements[i].clone());
            }
        } else {
            // replacement not valid
            new_statements.push(block.statements[i].clone());
        }


    }

    block.statements = new_statements;

    changes
}

// returns a new statement if input matches an identity, or both operands are constants
// also returns some info about which flags will be definitely set/unset after the old operation which may be useful if a branch statement follows.
// returns:
// Option<Stmt> a replacement Stmt, if the input could be reduced
// Option<PartialFlagSet> info about the flags set by the original statement, if it was an ALU instruction
pub fn simplify_single_statement(stmt: &Stmt) -> (Option<Stmt>, Option<PartialFlagSet>) {

    match stmt.kind {
        StmtKind::Nullary(_) => (None, None),
        StmtKind::Unary(unary) => {
            let a1 = stmt.arg1.clone().unwrap();
            // flag analysis
            let mut partial_flagset = match unary {
                Unary::Copy => None,
                Unary::Increment => Some(PartialFlagSet::new()),
                Unary::Decrement => Some(PartialFlagSet::new()),
                Unary::TwosComplement => Some(PartialFlagSet::new()),
                Unary::Compare => Some(PartialFlagSet::new().nc().no()),
                Unary::BitwiseNOT => Some(PartialFlagSet::new().nc().no()),
                Unary::Log2 => Some(PartialFlagSet::new().nc()),
            };

            // constant folding
            // is a1 constant? if so, convert to folded literal
            let new_fl = match arg_to_fl(&a1) {
                Some(fl) => match unary {
                    Unary::Copy => None,
                    Unary::Compare => None,
                    Unary::Increment => 
                        Some(FoldedLiteral::Increment(fl)),
                    Unary::Decrement => 
                        Some(FoldedLiteral::Decrement(fl)),
                    Unary::TwosComplement =>
                        Some(FoldedLiteral::TwosComplement(fl)),
                    Unary::BitwiseNOT => 
                        Some(FoldedLiteral::BitwiseNOT(fl)),
                    Unary::Log2 => 
                        Some(FoldedLiteral::Log2(fl)),
                }
                None => None,
            };
            // reduce folded literal if possible
            let (new_arg, more_detailed_partial_flagset) = match new_fl {
                Some(fl) => match reduce_fl(&fl) {
                    Some((lit, flag_set)) => {
                        (Some(StmtArg::Literal(lit)), Some(flag_set.to_partial()))
                    },
                    None => {
                        (Some(StmtArg::FoldedLiteral(Box::from(fl))), None)
                    }
                },
                None => (None, None),
            };
            // if the folded literal can be reduced with constant folding then more info is known about the flagset
            // replace initial flagset in this case
            match more_detailed_partial_flagset {
                Some(fs) => {
                    partial_flagset = Some(fs);
                }
                None => ()
            }
            // create replacement Stmt
            let new_stmt = match new_arg {
                Some(arg) => {
                    let mut new_stmt = stmt.clone();
                    new_stmt.kind = StmtKind::Unary(Unary::Copy);
                    new_stmt.arg1 = Some(arg);
                    new_stmt.arg2 = None;
                    Some(new_stmt)
                }
                None => None
            };

           
            (new_stmt, partial_flagset)
        },
        StmtKind::Binary(binary) => {
            let a1 = stmt.arg1.clone().unwrap();
            let a2 = stmt.arg2.clone().unwrap();

            // identites that can be replaced with a copy
            // eg adding 0 or multiplying by 1
            let replacement_and_flagset = match (binary, &a1, &a2) {
                // a+0=a
                (Binary::Add, a, StmtArg::Literal(0)) |
                (Binary::Add, StmtArg::Literal(0), a) => Some((a.clone(), PartialFlagSet::nz_from_cmp(&a).nc().no())),
                // a-0=a
                (Binary::Subtract, a1, StmtArg::Literal(0)) => Some((a1.clone(), PartialFlagSet::nz_from_cmp(&a1).c().no())),
                // a-a=0
                (Binary::Subtract, a1, a2) if a1 == a2 => Some((StmtArg::Literal(0), PartialFlagSet::nz_from_lit(0).c())),
                // a*1=a
                (Binary::Multiply, a, StmtArg::Literal(1)) |
                (Binary::Multiply, StmtArg::Literal(1), a) => Some((a.clone(), PartialFlagSet::nz_from_cmp(&a).nc().no())),
                // a*0=0
                (Binary::Multiply, _, StmtArg::Literal(0)) |
                (Binary::Multiply, StmtArg::Literal(0), _) => Some((StmtArg::Literal(0), PartialFlagSet::nz_from_lit(0).nc().no())),
                // a/1=a
                (Binary::Divide, a1, StmtArg::Literal(1)) => Some((a1.clone(), PartialFlagSet::nz_from_cmp(&a1).nc().no())),
                // a/0=255
                (Binary::Divide, _, StmtArg::Literal(0)) => Some((StmtArg::Literal(255), PartialFlagSet::nz_from_lit(255).nc().o())),
                // 0/a=0 (if a!=0)
                (Binary::Divide, StmtArg::Literal(0), StmtArg::Literal(x)) if *x != 0 => Some((StmtArg::Literal(0), PartialFlagSet::nz_from_lit(0).nc().no())),
                // a/a=1 (if a!=0)
                (Binary::Divide, a1, a2) if *a1 == *a2 && matches!(a1, StmtArg::Literal(x) if *x != 0) => Some((StmtArg::Literal(1), PartialFlagSet::nz_from_lit(1).nc().no())),
                // a&a=a
                (Binary::BitwiseAND, a1, a2) if *a1 == *a2 => Some((a1.clone(), PartialFlagSet::nz_from_cmp(&a1).nc().no())),
                // a&0=0
                (Binary::BitwiseAND, StmtArg::Literal(0), _) |
                (Binary::BitwiseAND, _, StmtArg::Literal(0))  => Some((StmtArg::Literal(0), PartialFlagSet::nz_from_lit(0).nc().no())),
                // a&255=a
                (Binary::BitwiseAND, StmtArg::Literal(255), a) |
                (Binary::BitwiseAND, a, StmtArg::Literal(255)) => Some((a.clone(), PartialFlagSet::nz_from_cmp(&a).nc().no())),
                // a|a=a
                (Binary::BitwiseOR, a1, a2) if *a1 == *a2 => Some((a1.clone(), PartialFlagSet::nz_from_cmp(&a1).nc().no())),
                // a|0=a
                (Binary::BitwiseOR, StmtArg::Literal(0), a) |
                (Binary::BitwiseOR, a, StmtArg::Literal(0))  => Some((a.clone(), PartialFlagSet::nz_from_cmp(&a).nc().no())),
                // a|255=255
                (Binary::BitwiseOR, StmtArg::Literal(255), _) |
                (Binary::BitwiseOR, _, StmtArg::Literal(255)) => Some((StmtArg::Literal(255), PartialFlagSet::nz_from_lit(255).nc().no())),
                // a^a=0
                (Binary::BitwiseXOR, a1, a2) if a1 == a2 => Some((StmtArg::Literal(0), PartialFlagSet::nz_from_lit(0).nc().no())),
                // a^0=a
                (Binary::BitwiseXOR, StmtArg::Literal(0), a) |
                (Binary::BitwiseXOR, a, StmtArg::Literal(0))  => Some((a.clone(), PartialFlagSet::nz_from_cmp(&a).nc().no())),
                
                _ => None
            };

            // return if we have found a replacement
            match replacement_and_flagset {
                Some((replacement_arg, flagset)) => {
                    let mut new_stmt = stmt.clone();
                    new_stmt.kind = StmtKind::Unary(Unary::Copy);
                    new_stmt.arg1 = Some(replacement_arg);
                    new_stmt.arg2 = None;
                    return (Some(new_stmt), Some(flagset))
                }
                None => ()
            }
            
            // check for binary operations that can be converted to another unary op (not copy)
            let kind_replacement_and_flagset = match (binary, &a1, &a2) {
                // a^255=~a
                (Binary::BitwiseXOR, a, StmtArg::Literal(255)) | 
                (Binary::BitwiseXOR, StmtArg::Literal(255), a) => Some((Unary::BitwiseNOT, a.clone(), PartialFlagSet::nz_from_cmp(a).nc().no())),
                // 0-a=-a
                (Binary::Subtract, StmtArg::Literal(0), a) => Some((Unary::TwosComplement, a.clone(), PartialFlagSet::zcno_from_replacement_instruction())),
                // a*255=-a
                (Binary::Multiply, StmtArg::Literal(255), a) | 
                (Binary::Multiply, a, StmtArg::Literal(255)) => Some((Unary::TwosComplement, a.clone(), PartialFlagSet::zcno_from_replacement_instruction().nc().no())),
                // a+1=a++, a-255=a++
                (Binary::Add, a, StmtArg::Literal(1)) |
                (Binary::Add, StmtArg::Literal(1), a) |
                (Binary::Subtract, a, StmtArg::Literal(255)) => Some((Unary::Increment, a.clone(), PartialFlagSet::zcno_from_replacement_instruction())),
                // a-1=a--, a+255=a--
                (Binary::Subtract, a, StmtArg::Literal(1)) |
                (Binary::Add, a, StmtArg::Literal(255)) |
                (Binary::Add, StmtArg::Literal(255), a)=> Some((Unary::Decrement, a.clone(), PartialFlagSet::zcno_from_replacement_instruction())),

                _ => None
            };

            // return if we have found a replacement
            match kind_replacement_and_flagset {
                Some((kind, replacement_arg, flagset)) => {
                    let mut new_stmt = stmt.clone();
                    new_stmt.kind = StmtKind::Unary(kind);
                    new_stmt.arg1 = Some(replacement_arg);
                    new_stmt.arg2 = None;
                    return (Some(new_stmt), Some(flagset))
                }
                None => ()
            }
            
            // generic flag analysis
            let mut partial_flagset = match binary {
                Binary::RAMWrite => None,
                Binary::Add => Some(PartialFlagSet::new()),
                Binary::Subtract => Some(PartialFlagSet::new()),
                Binary::Multiply => Some(PartialFlagSet::new().nc().no()),
                Binary::Divide => Some(PartialFlagSet::new().nc()),
                Binary::Modulo => Some(PartialFlagSet::new().nc()),
                Binary::BitwiseXOR => Some(PartialFlagSet::new().nc().no()),
                Binary::BitwiseAND => Some(PartialFlagSet::new().nc().no()),
                Binary::BitwiseOR => Some(PartialFlagSet::new().nc().no()),
            };

            // constant folding
            let new_fl = match (arg_to_fl(&a1), arg_to_fl(&a2)) {
                (Some(fl1), Some(fl2)) => match binary {
                    Binary::RAMWrite => None,
                    Binary::Add => Some(FoldedLiteral::Add(fl1, fl2)),
                    Binary::Subtract => Some(FoldedLiteral::Subtract(fl1, fl2)),
                    Binary::Multiply => Some(FoldedLiteral::Multiply(fl1, fl2)),
                    Binary::Divide => Some(FoldedLiteral::Divide(fl1, fl2)),
                    Binary::Modulo => Some(FoldedLiteral::Modulo(fl1, fl2)),
                    Binary::BitwiseXOR => Some(FoldedLiteral::BitwiseXOR(fl1, fl2)),
                    Binary::BitwiseAND => Some(FoldedLiteral::BitwiseAND(fl1, fl2)),
                    Binary::BitwiseOR => Some(FoldedLiteral::BitwiseOR(fl1, fl2)),
                }
                _ => None,
            };

            // reduce FoldedLiteral to Literal if possible
            let (arg, more_detailed_partial_flagset) = match new_fl {
                Some(fl) => match reduce_fl(&fl) {
                    Some((lit, flagset)) => (Some(StmtArg::Literal(lit)), Some(flagset.to_partial())),
                    None => (Some(StmtArg::FoldedLiteral(Box::new(fl))), None),
                },
                None => (None, None),
            };

            // if the folded literal can be reduced with constant folding then more info is known about the flagset
            // replace initial flagset in this case
            match more_detailed_partial_flagset {
                Some(fs) => {
                    partial_flagset = Some(fs);
                }
                None => ()
            }

            // return if we reduced
            match arg {
                Some(replacement_arg) => {
                    let mut new_stmt = stmt.clone();
                    new_stmt.kind = StmtKind::Unary(Unary::Copy);
                    new_stmt.arg1 = Some(replacement_arg);
                    new_stmt.arg2 = None;
                    return (Some(new_stmt), partial_flagset);
                }
                None => ()
            }
            
            // no binary reduction possible
            (None, partial_flagset)
        }
        
    }

}

#[derive(Hash, Eq, PartialEq)]
enum Subexpression {
    Nullary{kind: Nullary},
    Unary{kind: Unary, arg: StmtArg},
    Binary{kind:Binary, arg_a: StmtArg, arg_b:StmtArg}
}
struct LiveDefn {
    subexpression: Subexpression,
    definition_id: DefinitionID,
    result: VarID,
    loc: CodeLocation
}
impl LiveDefn {
    fn uses_value_of_var(&self, var: VarID) -> bool {
        match &self.subexpression {
            Subexpression::Unary { arg, ..} => arg.uses_value_of_var(var),
            Subexpression::Binary {arg_a, arg_b, .. } => arg_a.uses_value_of_var(var) || arg_b.uses_value_of_var(var),
            _ => false
        }
    }
}

fn subexpression_elimination_in_block(block: &mut BasicBlock, may_replace_final_alu_instruction: bool) -> bool {
    let mut changes = false;
    let mut live_defns: Vec<LiveDefn> = vec![];
    let mut new_statements: Vec<Stmt> = vec![];

    // find idx of last ALU instruction (which would set the flags before a potential branch)
    let mut final_alu_stmt_i = None;
    for i in 0..block.statements.len() {
        if is_alu_instruction(&block.statements[i]) {
            final_alu_stmt_i = Some(i);
        }
    }

    for i in 0..block.statements.len() {

        let mut has_replaced_statement = false;
        let mut new_live_defn: Option<LiveDefn> = None; 
        
        // try to replace this statement
        let is_final_alu_stmt = final_alu_stmt_i.is_some_and(|fas_i| fas_i == i);
        if !(is_final_alu_stmt && !may_replace_final_alu_instruction) {
            (has_replaced_statement, new_live_defn) = match &block.statements[i] {
                Stmt { kind: StmtKind::Nullary(nullary), loc, result, definition_id, .. } => {
                    match nullary {
                        Nullary::UpdateScreen |
                        Nullary::Wait |
                        Nullary::GetInput |
                        Nullary::Stop => {
                            (false, None)
                        }
                    }
                }
                Stmt { kind: StmtKind::Unary(unary), arg1: Some(arg), loc, result, definition_id, .. } => {
                    match unary {
                        Unary::Copy |
                        Unary::Compare => (false, None),
                        Unary::Increment |
                        Unary::Decrement |
                        Unary::TwosComplement |
                        Unary::BitwiseNOT |
                        Unary::Log2 => {
                            let subexpression = Subexpression::Unary { kind: unary.clone(), arg: arg.clone() };
                            let maybe_live_defn = live_defns.iter().find(|&defn| defn.subexpression == subexpression);
                            match maybe_live_defn {
                                Some(live_defn) => {
                                    // identical, non-killed definition
                                    // replace this instruction with a copy
                                    new_statements.push(Stmt { 
                                        loc: *loc, 
                                        kind: StmtKind::Unary(Unary::Copy), 
                                        arg1: Some(StmtArg::Variable(live_defn.result)), 
                                        arg2: None, 
                                        result: *result, 
                                        definition_id: *definition_id 
                                    });
                                    changes = true;
                                    (true, None)

                                },
                                None => {
                                    // create new live_defns record (add later)
                                    match result {
                                        Some(result_var_id) => {
                                            let new_live_defn = LiveDefn{definition_id: *definition_id, loc: *loc, result: *result_var_id, subexpression};
                                            (false, Some(new_live_defn))

                                        },
                                        None => (false, None)
                                    }
                                },
                            }
                            
                            
                        }
                    }
                }
                Stmt { kind: StmtKind::Binary(binary), arg1: Some(arg1), arg2: Some(arg2), loc, result, definition_id, .. } => {

                    // if op is commutative, re-order args according to hash value
                    let (arg_a, arg_b) = if binary.is_commutative() {    
                        if calculate_hash(arg1) < calculate_hash(arg2) {
                            (arg1, arg2)
                        } else {
                            (arg2, arg1)
                        }
                    } else {
                        (arg1, arg2)
                    };
                    
                    let subexpression = Subexpression::Binary { kind: *binary, arg_a:arg_a.clone(), arg_b:arg_b.clone() };
                    let maybe_live_defn = live_defns.iter().find(|&defn| defn.subexpression == subexpression);
                    match binary {
                        Binary::RAMWrite => {
                            // There could be a RAMWrite with the exact same args as a previous one.
                            // result var_id may be different, but it will always refer to the same location in RAM.
                            // todo - delete this instruction in this case.
                            (false, None)
                        },
                        Binary::Add |
                        Binary::Subtract |
                        Binary::Multiply |
                        Binary::Divide |
                        Binary::Modulo |
                        Binary::BitwiseXOR |
                        Binary::BitwiseAND |
                        Binary::BitwiseOR => {
                            match maybe_live_defn {
                                Some(live_defn) => {
                                    // identical, non-killed definition
                                    // replace with a copy of that instruction
                                    new_statements.push(Stmt { 
                                        loc: *loc, 
                                        kind: StmtKind::Unary(Unary::Copy), 
                                        arg1: Some(StmtArg::Variable(live_defn.result)), 
                                        arg2: None, 
                                        result: *result, 
                                        definition_id: *definition_id 
                                    });
                                    changes = true;
                                    (true, None)
                                }
                                None => {
                                    match result {
                                        Some(result_var_id) => {
                                            let new_live_defn = LiveDefn{definition_id: *definition_id, loc: *loc, result: *result_var_id, subexpression};
                                            (false, Some(new_live_defn))

                                        },
                                        None => (false, None)
                                    }
                                },
                            }
                        }
                    }
                },
                _ => (false, None)
            };

            
            
        }
        

        
        if !has_replaced_statement {
            // copy the original
            new_statements.push(block.statements[i].clone());
        }

        // remove defns killed by this statement
        match &block.statements[i].result {
            Some(var_id) => {
                live_defns = live_defns.into_iter().filter(|defn|
                    // defn uses a now overwritten arg, or result of defn has been overwritten
                    !defn.uses_value_of_var(*var_id) && defn.result != *var_id
                ).collect()
            },
            _ => (),
        }

        // add new defn if statement created one
        match new_live_defn {
            Some(defn) => live_defns.push(defn),
            None => (),
        }
    }
    block.statements = new_statements;
    return changes

}

fn branch_cleanup(ir: &mut IntermediateRepresentation) -> bool {

    if ir.basic_blocks.len() <= 1 {
        return false;
    }

    let mut changes = false;

    let mut all_parents = get_all_block_parents(ir);

    // if a block has no parents then it can be eliminated.
    // delete all statements and jump/branches.
    // NOT VALID for the entry block 0.
    for block_ptr in 1..ir.basic_blocks.len() {
        if all_parents[block_ptr].len() == 0 {
            let block = &mut ir.basic_blocks[block_ptr];
            if block.statements.len() > 0 {
                // del statements
                changes = true;
                block.statements.clear();
            }
            if block.branch.is_some() || block.continue_to.is_some() {
                // del branches
                changes = true;
                block.branch = None;
                block.continue_to = None;
                // recalculate parents
                all_parents = get_all_block_parents(ir);
            }
        }
    }

    // if a block contains no instructions and no branch (only a jump), then the jump can be propagated.
    for block_ptr in 0..ir.basic_blocks.len() {
        match &ir.basic_blocks[block_ptr] {
            BasicBlock { statements, branch: None, continue_to: Some(continue_ptr), .. } if statements.len() == 0 => {
                let continue_ptr = *continue_ptr;
                for parent_ptr in &all_parents[block_ptr] {
                    if *parent_ptr == block_ptr {
                        // ignore case of block pointing to itself
                        continue;
                    }
                    changes = true;
                    match ir.basic_blocks[*parent_ptr].continue_to {
                        Some(ptr) if ptr == block_ptr => {
                            ir.basic_blocks[*parent_ptr].continue_to = Some(continue_ptr);
                        },
                        _ => (),
                    }
                    match ir.basic_blocks[*parent_ptr].branch {
                        Some(Branch { flag, to }) if to == block_ptr => {
                            ir.basic_blocks[*parent_ptr].branch = Some(Branch { flag, to: continue_ptr });
                        }
                        _ => ()
                    }
                }
                // recalculate parents
                all_parents = get_all_block_parents(ir);
            }
            _ => ()
        }
    }

    // if a block has only 1 parent, and that parent points to the block using a jump (and has no branch), then all the block's statements and any jump/branch can be moved to the end of the parent.
    for block_ptr in 1..ir.basic_blocks.len() {
        if all_parents[block_ptr].len() == 1 {
            let parent_ptr = all_parents[block_ptr][0];
            if matches!(ir.basic_blocks[parent_ptr], BasicBlock { branch: None, continue_to: Some(ptr), .. } if ptr == block_ptr) {
                changes = true;
                let mut new_stmts = ir.basic_blocks[parent_ptr].statements.clone();
                new_stmts.extend(ir.basic_blocks[block_ptr].statements.iter().cloned());
                ir.basic_blocks[parent_ptr].statements = new_stmts;
                ir.basic_blocks[parent_ptr].continue_to = ir.basic_blocks[block_ptr].continue_to;
                ir.basic_blocks[parent_ptr].branch = ir.basic_blocks[block_ptr].branch.clone();

                // delete everything from this block - everything has been moved into the parent
                ir.basic_blocks[block_ptr].statements.clear();
                ir.basic_blocks[block_ptr].branch = None;
                ir.basic_blocks[block_ptr].continue_to = None;

                all_parents = get_all_block_parents(ir);
            }
        }
    }

    changes

}

pub fn arg_to_fl(arg: &StmtArg) -> Option<Box<FoldedLiteral>> {
    match arg {
        StmtArg::Literal(lit) => Some(Box::from(FoldedLiteral::Literal(*lit))),
        StmtArg::Variable(_) => None,
        StmtArg::RAMPointer(_, _) => None,
        StmtArg::LocationOfVariable(var_id) => Some(Box::from(FoldedLiteral::LocationOfVariable(*var_id))),
        StmtArg::Constant(_) => None, // todo
        StmtArg::FoldedLiteral(folded_literal) => Some(folded_literal.clone()),
    }
}

pub enum ALUStatus {
    FlagSet(FlagSet),
    CompareThisVar(VarID)
}
pub struct FlagSet {
    z: bool,
    c: bool,
    n: bool,
    o: bool
}
impl FlagSet {
    pub fn new() -> FlagSet {
        FlagSet { z: false, c: false, n: false, o: false }
    }
    pub fn z(mut self) -> Self {
        self.z = true; self
    }
    pub fn c(mut self) -> Self {
        self.c = true; self
    }
    pub fn n(mut self) -> Self {
        self.n = true; self
    }
    pub fn o(mut self) -> Self {
        self.c = true; self
    }
    pub fn from_zn(result: u8) -> FlagSet {
        let mut fs = FlagSet::new();
        fs.z = result == 0;
        fs.n = result > 127;
        fs
    } 
    pub fn to_partial(&self) -> PartialFlagSet {
        PartialFlagSet { z: Some(self.z), c: Some(self.c), n: Some(self.n), o: Some(self.o), zn_from_cmp: None, zcno_from_replacement_instruction: false }
    }
}

pub struct PartialFlagSet {
    zn_from_cmp: Option<StmtArg>,
    zcno_from_replacement_instruction: bool,
    // ^ is overridden by v
    z: Option<bool>,
    c: Option<bool>,
    n: Option<bool>,
    o: Option<bool>,
}

impl PartialFlagSet {
    pub fn new() -> Self {
        Self { zn_from_cmp: None, z: None, c: None, n: None, o: None, zcno_from_replacement_instruction:false }
    }
    pub fn nz_from_cmp(arg: &StmtArg) -> Self {
        let mut pfs = Self::new();
        pfs.zn_from_cmp = Some(arg.clone());
        pfs
    }
    pub fn nz_from_lit(result: u8) -> Self {
        let mut fs = Self::new();
        fs.z = Some(result == 0);
        fs.n = Some(result > 127);
        fs
    }
    pub fn zcno_from_replacement_instruction() -> Self {
        let mut pfs = Self::new();
        pfs.zcno_from_replacement_instruction = true; pfs
    }
    pub fn z(mut self) -> Self {
        self.z = Some(true); self
    }
    pub fn c(mut self) -> Self {
        self.c = Some(true); self
    }
    pub fn n(mut self) -> Self {
        self.n = Some(true); self
    }
    pub fn o(mut self) -> Self {
        self.c = Some(true); self
    }
    pub fn nz(mut self) -> Self {
        self.z = Some(false); self
    }
    pub fn nc(mut self) -> Self {
        self.c = Some(false); self
    }
    pub fn nn(mut self) -> Self {
        self.n = Some(false); self
    }
    pub fn no(mut self) -> Self {
        self.c = Some(false); self
    }
}


// todo technically could get some info about the flags even if the constant can't be fully reduced
pub fn reduce_fl(fl: &FoldedLiteral) -> Option<(u8, FlagSet)> {
    match *fl {
        FoldedLiteral::LocationOfVariable(_) => None,
        FoldedLiteral::Literal(lit) => Some((lit, FlagSet::from_zn(lit))),
        FoldedLiteral::BitwiseNOT(ref folded_literal) => {
            let result = !reduce_fl(folded_literal)?.0;
            Some((result, FlagSet::from_zn(result)))
        },
        FoldedLiteral::Log2(ref folded_literal) => {
            let as_float = reduce_fl(folded_literal)?.0 as f32;
            if as_float <= 0.0 {
                Some((0, FlagSet::from_zn(0).o()))
            } else {
                let result = (as_float.log2().floor() as i64) as u8;
                Some((result, FlagSet::from_zn(result)))
            }
        }
        FoldedLiteral::Increment(ref folded_literal) => {
            let arg1 = reduce_fl(folded_literal)?.0 as u16;
            let arg2 = 1 as u16;
            let sum = arg1 + arg2;
            let mut fs = FlagSet::from_zn(sum as u8);
            if (sum & 256) != 0 {
                fs.c = true;
            }
            let arg1_is_negative = (arg1 & 128) != 0;
            let arg2_is_negative = (arg2 & 128) != 0;
            let sum_is_negative = (sum & 128) != 0;
            if arg1_is_negative == arg2_is_negative && arg1_is_negative != sum_is_negative {
                fs.o = true
            }
            Some((sum as u8, fs))
        }
        FoldedLiteral::Decrement(ref folded_literal) => {
            let arg1 = reduce_fl(folded_literal)?.0 as u16;
            let arg2 = 1 as u16;
            let sum = (arg1 as u16) + (!arg2 as u16) + 1;
            let mut fs = FlagSet::from_zn(sum as u8);
            if (sum & 256) != 0 {
                fs.c = true;
            }
            let arg1_is_negative = (arg1 & 128) != 0;
            let arg2_is_negative = (arg2 & 128) != 0;
            let sum_is_negative = (sum & 128) != 0;
            if arg1_is_negative != arg2_is_negative && arg1_is_negative != sum_is_negative {
                fs.o = true
            }
            Some((sum as u8, fs))
        }
        FoldedLiteral::TwosComplement(ref folded_literal) => {
            let arg1 = reduce_fl(folded_literal)?.0 as u16;
            let result = (!arg1 as u16) + 1;
            let mut fs = FlagSet::from_zn(result as u8);
            if (result & 256) != 0 {
                fs.c = true;
            }
            if arg1 == 128 {
                fs.o = true;
            }
            Some((result as u8, fs))
        }
        FoldedLiteral::Add(ref folded_literal, ref folded_literal1) => {
            let arg1 = reduce_fl(folded_literal)?.0 as u16;
            let arg2 = reduce_fl(folded_literal1)?.0 as u16;
            let sum = arg1 + arg2;
            let mut fs = FlagSet::from_zn(sum as u8);
            if (sum & 256) != 0 {
                fs.c = true;
            }
            let arg1_is_negative = (arg1 & 128) != 0;
            let arg2_is_negative = (arg2 & 128) != 0;
            let sum_is_negative = (sum & 128) != 0;
            if arg1_is_negative == arg2_is_negative && arg1_is_negative != sum_is_negative {
                fs.o = true
            }
            Some((sum as u8, fs))

        },
        FoldedLiteral::Subtract(ref folded_literal, ref folded_literal1) => {
            let arg1 = reduce_fl(folded_literal)?.0;
            let arg2 = reduce_fl(folded_literal1)?.0;
            let sum = (arg1 as u16) + (!arg2 as u16) + 1;
            let mut fs = FlagSet::from_zn(sum as u8);
            if (sum & 256) != 0 {
                fs.c = true;
            }
            let arg1_is_negative = (arg1 & 128) != 0;
            let arg2_is_negative = (arg2 & 128) != 0;
            let sum_is_negative = (sum & 128) != 0;
            if arg1_is_negative != arg2_is_negative && arg1_is_negative != sum_is_negative {
                fs.o = true
            }
            Some((sum as u8, fs))
        },
        FoldedLiteral::Multiply(ref folded_literal, ref folded_literal1) => {
            let product = (reduce_fl(folded_literal)?.0 as u16 * reduce_fl(folded_literal1)?.0 as u16) as u8;
            Some((product, FlagSet::from_zn(product)))
        },
        FoldedLiteral::Divide(ref folded_literal, ref folded_literal1) => {
            let arg1 = reduce_fl(folded_literal)?.0;
            let arg2 = reduce_fl(folded_literal1)?.0;
            if arg2 == 0 {
                // x/0 = 255, and Overflow flag is set
                Some((255, FlagSet::from_zn(255).o()))
            } else {
                let result = arg1 / arg2;
                Some(((result), FlagSet::from_zn(result)))
            }
        },
        FoldedLiteral::Modulo(ref folded_literal, ref folded_literal1) => {
            let arg1 = reduce_fl(folded_literal)?.0;
            let arg2 = reduce_fl(folded_literal1)?.0;
            if arg2 == 0 {
                // x%0 = x, and Overflow flag is set
                Some((arg1, FlagSet::from_zn(arg1).o()))
            } else {
                let result = arg1 % arg2;
                Some(((result), FlagSet::from_zn(result)))
            }
        },
        FoldedLiteral::BitwiseXOR(ref folded_literal, ref folded_literal1) => {
            let result = reduce_fl(folded_literal)?.0 ^ reduce_fl(folded_literal1)?.0;
            Some(((result), FlagSet::from_zn(result)))
        },
        FoldedLiteral::BitwiseAND(ref folded_literal, ref folded_literal1) => {
            let result = reduce_fl(folded_literal)?.0 & reduce_fl(folded_literal1)?.0;
            Some(((result), FlagSet::from_zn(result)))
        },
        FoldedLiteral::BitwiseOR(ref folded_literal, ref folded_literal1) => {
            let result = reduce_fl(folded_literal)?.0 | reduce_fl(folded_literal1)?.0;
            Some(((result), FlagSet::from_zn(result)))
        }
    }
}


// fn hash_of_stmt_operands(stmt: &Stmt, loc: CodeLocation, rd: &HashMap<VarID, Vec<DefinitionID>>) -> Result<Option<u64>, OptimizationError> {

//     let maybe_arg1_hash = match &stmt.arg1 {
//         Some(arg) => Some(hash_of_stmt_arg(arg, loc, rd)?),
//         None => None,
//     };
//     let maybe_arg2_hash = match &stmt.arg2 {
//         Some(arg) => Some(hash_of_stmt_arg(arg, loc, rd)?),
//         None => None,
//     };

//     Ok(match stmt.kind {
//         StmtKind::Nullary(nullary) => match nullary {
//             Nullary::UpdateScreen => None,
//             Nullary::Wait => None,
//             Nullary::GetInput => None,
//             Nullary::Stop => None,
//         },
//         StmtKind::Unary(unary) => match unary {
//             Unary::Copy |
//             Unary::Increment |
//             Unary::Decrement |
//             Unary::TwosComplement |
//             Unary::Compare |
//             Unary::BitwiseNOT |
//             Unary::Log2 => {
//                 match maybe_arg1_hash {
//                     Some(arg1_hash) => Some(calculate_hash(&(&stmt.kind, arg1_hash))),
//                     None => None
//                 }
                
//             }
//         }
//         StmtKind::Binary(binary) => match binary {
//             Binary::RAMWrite => None,
//             Binary::Add |
//             Binary::Multiply |
//             Binary::BitwiseXOR |
//             Binary::BitwiseAND |
//             Binary::BitwiseOR => {
//                 match (maybe_arg1_hash, maybe_arg2_hash) {
//                     (Some(arg1_hash), Some(arg2_hash)) => {
//                         // commutative ops
//                         // sort StmtArgs by hash value
//                         if arg1_hash < arg2_hash {
//                             Some(calculate_hash(&(&stmt.kind, arg1_hash, arg2_hash)))
//                         } else {
//                             Some(calculate_hash(&(&stmt.kind, arg2_hash, arg1_hash)))
//                         }
//                     },
//                     _ => None
//                 }
//             }
//             Binary::Subtract |
//             Binary::Divide |
//             Binary::Modulo => {
//                 match (maybe_arg1_hash, maybe_arg2_hash) {
//                     (Some(arg1_hash), Some(arg2_hash)) => {
//                         // non-commutative ops
//                         Some(calculate_hash(&(&stmt.kind, arg1_hash, arg2_hash)))
//                     },
//                     _ => None
//                 }
//             }
//         },
//     })
// }


fn hash_of_stmt_arg(arg: &StmtArg, loc: CodeLocation, rd: &HashMap<VarID, Vec<DefinitionID>>) -> Result<u64, OptimizationError> {
    match arg {
        StmtArg::Literal(_) |
        StmtArg::LocationOfVariable(_) |
        StmtArg::Constant(_) |
        StmtArg::FoldedLiteral(_) => {
            Ok(calculate_hash(arg))
        }

        StmtArg::Variable(var_id) => {
            let defns = match rd.get(var_id) {
                Some(defns) => defns,
                None => return Err(OptimizationError{loc, message: "No reaching definitions of this variable".to_string()})
            };
            // sort definitions in case they are not already
            // todo wasteful code
            let mut sorted_defns = defns.clone();
            sorted_defns.sort();
            Ok(calculate_hash(&(arg, sorted_defns)))
        },
        StmtArg::RAMPointer(pointer_arg, arr_var_id) => {
            let pointer_hash = hash_of_stmt_arg(pointer_arg, loc, rd)?;
            let defns = match rd.get(arr_var_id) {
                Some(defns) => defns,
                None => return Err(OptimizationError{loc, message: "No reaching definitions of this variable".to_string()})
            };
            // sort definitions in case they are not already
            // todo wasteful code
            let mut sorted_defns = defns.clone();
            sorted_defns.sort();
            Ok(calculate_hash(&(arg, pointer_hash, sorted_defns)))
        }
    }
    
}


fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

fn is_alu_instruction(stmt: &Stmt) -> bool {
    match stmt.kind {
        StmtKind::Nullary(nullary) => match nullary {
            Nullary::UpdateScreen |
            Nullary::Wait |
            Nullary::GetInput |
            Nullary::Stop => false
        },
        StmtKind::Unary(unary) => match unary {
            Unary::Copy => false,
            Unary::Increment |
            Unary::Decrement |
            Unary::TwosComplement |
            Unary::Compare |
            Unary::BitwiseNOT |
            Unary::Log2 => true
        },
        StmtKind::Binary(binary) => match binary {
            Binary::RAMWrite => false,
            Binary::Add |
            Binary::Subtract |
            Binary::Multiply |
            Binary::Divide |
            Binary::Modulo |
            Binary::BitwiseXOR |
            Binary::BitwiseAND |
            Binary::BitwiseOR => true
        }
    }
}


// (mask, cases_where_branch_is_taken)
fn branch_flag_to_required_mask_and_values(flag: &BranchFlag) -> (u8, Vec<u8>) {
    // bitstrings are ZCNO
    match flag {
        BranchFlag::BZ => (0b1000, vec![0b1000]),
        BranchFlag::BNZ => (0b1000, vec![0b0000]),
        BranchFlag::BC => (0b0100, vec![0b0100]),
        BranchFlag::BNC => (0b0100, vec![0b0000]),
        BranchFlag::BN => (0b0010, vec![0b0010]),
        BranchFlag::BNN => (0b0010, vec![0b0000]),
        BranchFlag::BO => (0b0001, vec![0b0001]),
        BranchFlag::BNO => (0b0001, vec![0b0000]),
        BranchFlag::BGTE => (0b0011, vec![0b0000, 0b0011]),
        BranchFlag::BNGTE => (0b0011, vec![0b0001, 0b0010]),
    }
}

// inverse of branch_flag_to_required_mask_and_values
// selects a BranchFlag if the behaviour is guaranteed to be the same
fn mask_and_values_to_branch_flag(required_mask: &u8, required_values: &Vec<u8>) -> Option<BranchFlag> {

    let mut required_mask_powerset: Vec<u8> = vec![];
    for i in 0b0000..=0b1111 {
        if (i & !required_mask) == 0 {
            required_mask_powerset.push(i);
        }
    }

    'checkflag: for flag in <BranchFlag as Enumerable>::enumerator() {
        // check that all positive patterns succeed, and all negative patterns fail
        let (candidate_mask, candidate_values) = branch_flag_to_required_mask_and_values(&flag);

        // reject if candidate branch behaviour is based on bits that required does not consider
        if candidate_mask & !required_mask != 0 {
            continue;
        } else if candidate_mask == *required_mask {
            // values must be the same
            let mut candidate_values_copy = candidate_values.clone();
            'lookforrequired: for required_value in required_values {
                for (i, candidate_value) in candidate_values_copy.iter().enumerate() {
                    if candidate_value == required_value {
                        candidate_values_copy.remove(i);
                        continue 'lookforrequired ;
                    }
                }
                // not found in candidate_values - reject
                continue 'checkflag;
            }
            if candidate_values_copy.len() != 0 {
                continue;
            }
            return Some(flag)
        } else {
            // candidate range is smaller than required
            // for each bit mask, check that the candidate BranchFlag agrees for each 
            for subset in &required_mask_powerset {
                let required_branch_outcome = required_values.contains(subset);
                let candidate_branch_outcome = candidate_values.contains(&(subset & candidate_mask));
                if required_branch_outcome != candidate_branch_outcome {
                    continue 'checkflag;
                }
            }
            return Some(flag);
        }
    }
    
    return None;
    
    
}

fn branch_is_certain_to_be_taken(flag: &BranchFlag, pfs: &PartialFlagSet) -> bool {
    match (flag, pfs) {
        (BranchFlag::BZ, PartialFlagSet{z:Some(true), ..}) |
        (BranchFlag::BNZ, PartialFlagSet{z:Some(false), ..}) |
        (BranchFlag::BC, PartialFlagSet{c:Some(true), ..}) |
        (BranchFlag::BNC, PartialFlagSet{c:Some(false), ..}) |
        (BranchFlag::BN, PartialFlagSet{n:Some(true), ..}) |
        (BranchFlag::BNN, PartialFlagSet{n:Some(false), ..}) |
        (BranchFlag::BO, PartialFlagSet{o:Some(true), ..}) |
        (BranchFlag::BNO, PartialFlagSet{o:Some(false), ..}) |
        (BranchFlag::BGTE, PartialFlagSet{n: Some(true), o:Some(true), ..}) |
        (BranchFlag::BGTE, PartialFlagSet{n: Some(false), o:Some(false), ..}) |
        (BranchFlag::BNGTE, PartialFlagSet{n: Some(true), o:Some(false), ..}) |
        (BranchFlag::BNGTE, PartialFlagSet{n: Some(false), o:Some(true), ..}) => {
            true
        }
        _ => false
    }
}

fn branch_is_certain_not_to_be_taken(flag: &BranchFlag, pfs: &PartialFlagSet) -> bool {
    match (flag, pfs)  {
        (BranchFlag::BZ, PartialFlagSet{z:Some(false), ..}) |
        (BranchFlag::BNZ, PartialFlagSet{z:Some(true), ..}) |
        (BranchFlag::BC, PartialFlagSet{c:Some(false), ..}) |
        (BranchFlag::BNC, PartialFlagSet{c:Some(true), ..}) |
        (BranchFlag::BN, PartialFlagSet{n:Some(false), ..}) |
        (BranchFlag::BNN, PartialFlagSet{n:Some(true), ..}) |
        (BranchFlag::BO, PartialFlagSet{o:Some(false), ..}) |
        (BranchFlag::BNO, PartialFlagSet{o:Some(true), ..}) |
        (BranchFlag::BGTE, PartialFlagSet{n: Some(true), o:Some(false), ..}) |
        (BranchFlag::BGTE, PartialFlagSet{n: Some(false), o:Some(true), ..}) |
        (BranchFlag::BNGTE, PartialFlagSet{n: Some(true), o:Some(true), ..}) |
        (BranchFlag::BNGTE, PartialFlagSet{n: Some(false), o:Some(false), ..}) => {
            true
        }
        _ => false
    }
}