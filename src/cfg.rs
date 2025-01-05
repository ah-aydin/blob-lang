use std::collections::HashMap;

use crate::ast::{
    expr::Expr,
    stmt::{Stmt, StmtFuncDecl, VarTypeInfo},
    Ast,
};

/// Control flow graph for a function body
#[derive(Debug, Clone)]
pub struct Cfg {
    pub blocks: Vec<CfgBlock>,
}

impl Cfg {
    fn new() -> Cfg {
        Cfg { blocks: Vec::new() }
    }

    fn add_block(&mut self, cfg_block: CfgBlock) -> usize {
        self.blocks.push(cfg_block);
        self.blocks.len() - 1
    }

    fn add_condition_block(&mut self, cfg_block_condition: CfgBlockCondition) -> usize {
        if let CfgBlock::Basic(cfg_block_basic) = self.blocks.last().unwrap() {
            if cfg_block_basic.stmts.is_empty() {
                self.blocks.pop();
            }
        }
        self.blocks.push(CfgBlock::Condition(cfg_block_condition));
        self.blocks.len() - 1
    }

    fn add_stmt_to_basic_block(&mut self, index: usize, stmt: Stmt) {
        match self.blocks.get_mut(index).unwrap() {
            CfgBlock::Basic(cfg_block_basic) => cfg_block_basic.stmts.push(stmt),
            _ => unreachable!("Expected to find a basic block"),
        }
    }

    fn add_stmts_to_basic_block(&mut self, index: usize, stmts: Vec<Stmt>) {
        match self.blocks.get_mut(index).unwrap() {
            CfgBlock::Basic(cfg_block_basic) => cfg_block_basic.stmts.extend(stmts),
            _ => unreachable!("Expected to find a basic block"),
        }
    }

    fn set_successor(&mut self, index: usize, successor_index: usize) {
        match self.blocks.get_mut(index).unwrap() {
            CfgBlock::Start(cfg_block_start) => cfg_block_start.successor = successor_index,
            CfgBlock::Basic(cfg_block_basic) => cfg_block_basic.successor = successor_index,
            CfgBlock::Condition(cfg_block_condition) => {
                cfg_block_condition.true_successor = successor_index
            }
            CfgBlock::Exit => unreachable!("Canot set successor for Exit block"),
        }
    }

    fn set_true_successor(&mut self, index: usize, successor_index: usize) {
        match self.blocks.get_mut(index).unwrap() {
            CfgBlock::Condition(cfg_block_condition) => {
                cfg_block_condition.true_successor = successor_index
            }
            _ => unreachable!("Expected a block with a true successor"),
        }
    }

    fn set_false_successor(&mut self, index: usize, successor_index: usize) {
        match self.blocks.get_mut(index).unwrap() {
            CfgBlock::Condition(cfg_block_condition) => {
                cfg_block_condition.false_successor = successor_index
            }
            _ => unreachable!("Expected a block with a false successor"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CfgBlockStart {
    pub args: Vec<VarTypeInfo>,
    pub successor: usize,
}

#[derive(Debug, Clone)]
pub struct CfgBlockBasic {
    pub stmts: Vec<Stmt>,
    pub successor: usize,
}

impl CfgBlockBasic {
    fn new() -> CfgBlockBasic {
        CfgBlockBasic {
            stmts: vec![],
            successor: 0,
        }
    }

    fn with_capacity(capacity: usize) -> CfgBlockBasic {
        CfgBlockBasic {
            stmts: Vec::with_capacity(capacity),
            successor: 0,
        }
    }

    fn contains_return(&self) -> bool {
        self.stmts.iter().any(|stmt| match stmt {
            Stmt::Return(_) => true,
            _ => false,
        })
    }
}

#[derive(Debug, Clone)]
pub struct CfgBlockCondition {
    pub condition: Expr,
    pub true_successor: usize,
    pub false_successor: usize,
}

impl CfgBlockCondition {
    fn new(condition: Expr) -> CfgBlockCondition {
        CfgBlockCondition {
            condition,
            true_successor: 0,
            false_successor: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CfgBlock {
    Start(CfgBlockStart),
    Basic(CfgBlockBasic),
    Condition(CfgBlockCondition),
    Exit,
}

impl CfgBlock {
    fn is_empty(&self) -> bool {
        match self {
            CfgBlock::Start(_cfg_block_start) => false,
            CfgBlock::Basic(cfg_block_basic) => cfg_block_basic.stmts.len() == 0,
            CfgBlock::Condition(_cfg_block_condition) => false,
            CfgBlock::Exit => false,
        }
    }
}

pub fn build_cfgs(ast: Ast) -> HashMap<String, Cfg> {
    let mut cfgs: HashMap<String, Cfg> = HashMap::new();
    for stmt in ast {
        if let Stmt::FuncDecl(stmt_func_decl) = stmt {
            cfgs.insert(stmt_func_decl.ident.clone(), build_cfg(stmt_func_decl));
        }
    }
    cfgs
}

fn build_cfg(stmt_func_decl: StmtFuncDecl) -> Cfg {
    let mut cfg = Cfg::new();
    let starter_block_index = cfg.add_block(CfgBlock::Start(CfgBlockStart {
        args: stmt_func_decl.args,
        successor: 1,
    }));

    let stmts = match *stmt_func_decl.body {
        Stmt::Block(stmt_block) => stmt_block.stmts,
        _ => unreachable!("Expected a block for the function body"),
    };

    let current_block_index = cfg.add_block(CfgBlock::Basic(CfgBlockBasic::new()));
    cfg.set_successor(starter_block_index, current_block_index);

    build_cfg_helper(&mut cfg, stmts, current_block_index);
    cleanup_empty_blocks(&mut cfg);
    add_exit_block(&mut cfg);
    cfg
}

fn build_cfg_helper(cfg: &mut Cfg, stmts: Vec<Stmt>, mut current_block_index: usize) -> usize {
    for stmt in stmts {
        match stmt {
            Stmt::Expr(_) | Stmt::Return(_) | Stmt::VarDecl(_) | Stmt::Assign(_) => {
                cfg.add_stmt_to_basic_block(current_block_index, stmt);
            }

            Stmt::Block(stmt_block) => {
                cfg.add_stmts_to_basic_block(current_block_index, stmt_block.stmts);
            }

            Stmt::If(stmt_if) => {
                let condtion_block_index =
                    cfg.add_condition_block(CfgBlockCondition::new(stmt_if.condition));
                cfg.set_successor(current_block_index, condtion_block_index);

                let true_body = stmt_if.body.get_block_body();
                let true_block_index = cfg.add_block(CfgBlock::Basic(
                    CfgBlockBasic::with_capacity(true_body.len()),
                ));
                cfg.set_true_successor(condtion_block_index, true_block_index);
                let last_block_in_true_index = build_cfg_helper(cfg, true_body, true_block_index);

                let merge_block_index = cfg.add_block(CfgBlock::Basic(CfgBlockBasic::new()));

                cfg.set_false_successor(condtion_block_index, merge_block_index);
                cfg.set_successor(last_block_in_true_index, merge_block_index);

                current_block_index = merge_block_index;
            }
            Stmt::IfElse(stmt_if_else) => {
                let condition_block_index =
                    cfg.add_condition_block(CfgBlockCondition::new(stmt_if_else.condition));
                cfg.set_successor(current_block_index, condition_block_index);

                let true_body = stmt_if_else.if_body.get_block_body();
                let true_block_index = cfg.add_block(CfgBlock::Basic(
                    CfgBlockBasic::with_capacity(true_body.len()),
                ));
                cfg.set_true_successor(condition_block_index, true_block_index);
                let last_block_in_true_index = build_cfg_helper(cfg, true_body, true_block_index);

                let false_body = stmt_if_else.else_body.get_block_body();
                let false_block_index = cfg.add_block(CfgBlock::Basic(
                    CfgBlockBasic::with_capacity(false_body.len()),
                ));
                cfg.set_false_successor(condition_block_index, false_block_index);
                let last_block_in_false_index =
                    build_cfg_helper(cfg, false_body, false_block_index);

                let merge_block_index = cfg.add_block(CfgBlock::Basic(CfgBlockBasic::new()));
                cfg.set_successor(last_block_in_true_index, merge_block_index);
                cfg.set_successor(last_block_in_false_index, merge_block_index);

                current_block_index = merge_block_index;
            }
            Stmt::While(stmt_while) => {
                let condition_block_index =
                    cfg.add_condition_block(CfgBlockCondition::new(stmt_while.condition));
                cfg.set_successor(current_block_index, condition_block_index);

                let while_body = stmt_while.body.get_block_body();
                let while_body_index = cfg.add_block(CfgBlock::Basic(
                    CfgBlockBasic::with_capacity(while_body.len()),
                ));
                cfg.set_true_successor(condition_block_index, while_body_index);
                let last_block_in_while_index = build_cfg_helper(cfg, while_body, while_body_index);

                let merge_block_index = cfg.add_block(CfgBlock::Basic(CfgBlockBasic::new()));

                cfg.set_false_successor(condition_block_index, merge_block_index);
                cfg.set_successor(last_block_in_while_index, condition_block_index);

                current_block_index = merge_block_index;
            }

            Stmt::StructDecl(_) => {
                unreachable!("Did not expect a struct declartion inside a function")
            }
            Stmt::FuncDecl(_) => {
                unreachable!("Did not expect a function declration inside another function")
            }
        }
    }

    current_block_index
}

fn cleanup_empty_blocks(cfg: &mut Cfg) {
    let mut shifting_per_block = vec![0usize; cfg.blocks.len()];
    let mut empty_blocks = Vec::with_capacity(cfg.blocks.len());

    // Mark empty blocks and calculate the amount of shifting that will take place for each block after the removals
    let mut current_shifting = 0usize;
    for (i, block) in cfg.blocks.iter().enumerate() {
        shifting_per_block[i] = current_shifting;
        if block.is_empty() {
            current_shifting += 1;
            empty_blocks.push(i);
        }
    }
    println!("{:?}", shifting_per_block);

    // Update successor indexes with the shiftings
    cfg.blocks.iter_mut().for_each(|block| match block {
        CfgBlock::Start(cfg_block_start) => {
            cfg_block_start.successor -= shifting_per_block[cfg_block_start.successor]
        }
        CfgBlock::Basic(cfg_block_basic) => {
            cfg_block_basic.successor -= shifting_per_block[cfg_block_basic.successor]
        }
        CfgBlock::Condition(cfg_block_condition) => {
            cfg_block_condition.true_successor -=
                shifting_per_block[cfg_block_condition.true_successor];
            cfg_block_condition.false_successor -=
                shifting_per_block[cfg_block_condition.false_successor];
        }
        CfgBlock::Exit => {}
    });

    // Remove empty blocks
    empty_blocks.iter().rev().for_each(|empty_block| {
        cfg.blocks.remove(*empty_block);
    });
}

fn add_exit_block(cfg: &mut Cfg) {
    let exit_block_index = cfg.add_block(CfgBlock::Exit);
    cfg.blocks.iter_mut().for_each(|block| match block {
        CfgBlock::Start(cfg_block_start) => {
            if cfg_block_start.successor == 0 {
                cfg_block_start.successor = exit_block_index;
            }
        }
        CfgBlock::Basic(cfg_block_basic) => {
            if cfg_block_basic.contains_return() {
                cfg_block_basic.successor = exit_block_index;
            }
        }
        CfgBlock::Condition(_cfg_block_condition) => {}
        CfgBlock::Exit => {}
    });
}
