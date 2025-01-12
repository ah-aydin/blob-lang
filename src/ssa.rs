use std::collections::{HashMap, HashSet};

use crate::cfg::Cfg;

pub fn cfgs_to_ssa(cfgs: HashMap<String, Cfg>) {
    for (func, cfg) in cfgs {
        println!("Processing function '{func}'");
        cfg.print();
        println!();
        cfg_to_ssa(cfg);
    }
}

fn cfg_to_ssa(mut cfg: Cfg) {
    let dominators = compute_dominators(&mut cfg);
    let immediate_dominators = compute_immediate_dominators(&mut cfg, &dominators);
}

fn compute_dominators(cfg: &mut Cfg) -> Vec<HashSet<usize>> {
    let block_count = cfg.blocks.len();

    let mut dominators_per_block = vec![(0..block_count).collect(); block_count];
    dominators_per_block[0] = HashSet::from([0]);

    let mut updated;
    loop {
        updated = false;
        for block_index in 0..block_count {
            // The tarting block will not have any predecessors
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

fn compute_immediate_dominators(cfg: &mut Cfg, dominators: &Vec<HashSet<usize>>) -> Vec<usize> {
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

    immediate_dominators
}
