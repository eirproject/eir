/// When we have a way to calculate live values in the input IR, this transform
/// is actually pretty easy.
/// For every function call site, we generate two closures, one for the exception
/// branch and one for the ok branch.
/// These closures gets passed into the call to the function as the first two
/// arguments.
/// The result is that every function signature gets two extra arguments after
/// the transform has completed. Argument 0 is the ok continuation, argument
/// 1 is the error continuation.
///
/// TODO
/// Right now the input continuations are manually injected into every closure
/// inside the function. This is not optimal if the function terminates
/// without calling a continuation. This should be relatively rare, so this is
/// probably not a big deal.

use std::collections::HashSet;

use eir::Function;
use eir::op::OpKind;

pub fn cps_transform(fun: &Function) {
    let live = fun.live_values();

    // Identify continuation sites
    let mut cont_sites = HashSet::new();
    for op in live.flow_live.keys() {
        let kind = fun.op_kind(*op);
        match kind {
            OpKind::Call { .. } => {
                cont_sites.insert(*op);
            },
            OpKind::Apply { .. } => {
                cont_sites.insert(*op);
            },
            _ => (),
        }
    }

    // We now have every continuation site and the values that
    // need to be alive. This is all the information we need to
    // generate the lambda environments.


}
