use std::collections::{ HashSet, HashMap };

use ::eir::FunctionBuilder;
use ::eir::op::OpKind;

/// Removes NoValues and ValueLists.
/// These are only a high level feature, and should
/// statically be removed.
pub fn remove_compounds(builder: &mut FunctionBuilder) {

    let mut novalue_ssa = HashSet::new();
    let mut packvaluelist_ssa = HashMap::new();
    let mut unpackvaluelist_ssa = HashMap::new();

    // First pass, collect all definitions
    {
        let fun = builder.function_mut();

        for ebb in fun.iter_ebb() {
            for op in fun.iter_op(ebb) {
                let reads = fun.op_reads(op);
                let writes = fun.op_writes(op);
                match fun.op_kind(op) {
                    OpKind::MakeNoValue => {
                        assert!(writes.len() == 1);
                        novalue_ssa.insert(writes[0]);
                    },
                    OpKind::PackValueList => {
                        assert!(writes.len() == 1);
                        packvaluelist_ssa.insert(
                            writes[0],
                            reads.to_vec(),
                        );
                    },
                    OpKind::UnpackValueList => {
                        assert!(reads.len() == 1);
                        unpackvaluelist_ssa.insert(
                            reads[0],
                            writes.len(),
                        );
                    },
                    _ => (),
                }
            }
        }
    }

}
