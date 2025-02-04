use std::collections::{HashMap, HashSet};

use crate::{cfg::Cfg, info};

#[derive(Debug, Clone)]
struct IDomNode {
    block_index: usize,
    parent_index: usize,
    children_indicies: Vec<usize>,
}

impl IDomNode {
    fn new(block_index: usize, parent_index: usize, children_indicies: Vec<usize>) -> IDomNode {
        IDomNode {
            block_index,
            parent_index,
            children_indicies,
        }
    }

    #[allow(dead_code)]
    fn print(&self) {
        print!("index: {} - ", self.block_index);
        print!("parent: {} - ", self.parent_index);
        print!("children: ");
        self.children_indicies
            .iter()
            .for_each(|child| print!("{} ", child));
        println!();
    }
}

pub fn cfgs_to_ssa(cfgs: HashMap<String, Cfg>) {
    for (func, cfg) in cfgs {
        info!("Processing function '{func}'");
        cfg_to_ssa(cfg);
    }
}

fn cfg_to_ssa(mut cfg: Cfg) {
    let dominators = compute_dominators(&mut cfg);
    let idom_nodes = compute_immediate_dominator_tree(&mut cfg, &dominators);
    let dominance_frontiers = compute_dominance_frontiers(&mut cfg, &idom_nodes);
    for (i, df) in dominance_frontiers.iter().enumerate() {
        println!("{i}: {:?}", df);
    }
}

fn compute_dominators(cfg: &mut Cfg) -> Vec<HashSet<usize>> {
    let block_count = cfg.blocks.len();

    let mut dominators_per_block = vec![(0..block_count).collect(); block_count];
    dominators_per_block[0] = HashSet::from([0]);

    let mut updated;
    loop {
        updated = false;
        for block_index in 0..block_count {
            // The starting block will not have any predecessors
            if block_index == 0 {
                continue;
            }

            let mut new_dominators: Option<HashSet<usize>> = None;
            for predecessor in &cfg.predecessors()[block_index] {
                if new_dominators.is_none() {
                    new_dominators = Some(dominators_per_block[*predecessor].clone());
                    continue;
                }
                new_dominators = Some(
                    new_dominators
                        .unwrap()
                        .intersection(&dominators_per_block[*predecessor])
                        .into_iter()
                        .map(|v| *v) // Stupid stuff
                        .collect(),
                );
            }

            let mut new_dominators = new_dominators.unwrap();
            new_dominators.insert(block_index);
            if new_dominators != dominators_per_block[block_index] {
                dominators_per_block[block_index] = new_dominators;
                updated = true;
            }
        }
        if !updated {
            break;
        }
    }

    dominators_per_block
}

/// The first node in the vec is the root of the tree
fn compute_immediate_dominator_tree(
    cfg: &mut Cfg,
    dominators: &Vec<HashSet<usize>>,
) -> Vec<IDomNode> {
    let block_count = cfg.blocks.len();

    let mut immediate_dominators = Vec::with_capacity(block_count);
    immediate_dominators.push(0usize);
    for block_index in 1..block_count {
        let mut block_dominators: Vec<usize> = dominators[block_index]
            .clone()
            .into_iter()
            .filter(|index| *index != block_index)
            .collect();

        while block_dominators.len() > 1 {
            let dominator1 = block_dominators.pop().unwrap();
            let dominator2 = block_dominators.pop().unwrap();
            if dominators[dominator1].contains(&dominator2) {
                block_dominators.push(dominator1);
            } else if dominators[dominator2].contains(&dominator1) {
                block_dominators.push(dominator2);
            } else {
                unreachable!("Malformed CFG");
            }
        }

        immediate_dominators.push(block_dominators[0]);
    }

    let mut idom_nodes = Vec::with_capacity(block_count);
    for block_index in 0..block_count {
        let mut kids = vec![];
        for (index, parent) in immediate_dominators.iter().enumerate() {
            if index == 0 {
                continue;
            }
            if *parent == block_index {
                kids.push(index);
            }
        }

        idom_nodes.push(IDomNode::new(
            block_index,
            immediate_dominators[block_index],
            kids,
        ));
    }

    idom_nodes
}

fn compute_dominance_frontiers(cfg: &mut Cfg, idom_nodes: &Vec<IDomNode>) -> Vec<HashSet<usize>> {
    let block_count = cfg.blocks.len();

    let mut dominance_frontiers = vec![HashSet::new(); block_count];
    for block_index in 0..block_count {
        let predecessors = &cfg.predecessors()[block_index];
        if predecessors.len() < 2 {
            continue;
        }
        for predecessor in predecessors {
            let mut runner = *predecessor;
            while runner != idom_nodes[block_index].parent_index {
                dominance_frontiers[runner].insert(block_index);
                runner = idom_nodes[runner].parent_index;
            }
        }
    }

    dominance_frontiers
}
