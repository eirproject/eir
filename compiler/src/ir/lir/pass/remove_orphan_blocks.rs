use ::ir::lir::{ FunctionCfg, LabelN };
use ::std::collections::HashSet;

pub fn remove_orphan_blocks(cfg: &mut FunctionCfg) {
    loop {

        let mut all_labels: HashSet<LabelN> = HashSet::new();
        let mut jump_targets: HashSet<LabelN> = HashSet::new();
        jump_targets.insert(cfg.entry());

        for label in cfg.labels_iter() {
            all_labels.insert(label);
            for jump in cfg.jumps_iter(label) {
                let dest = cfg.edge_target(jump);
                jump_targets.insert(dest);
            }
        }

        println!("ALL: {:?}", all_labels);
        println!("ALIVE: {:?}", jump_targets);

        let mut found = false;
        for orphan in all_labels.difference(&jump_targets) {
            println!("Removing orphan: {}", orphan);
            found = true;
            cfg.remove_block(*orphan);
        }
        if !found { break; }

    }
}
