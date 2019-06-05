mod compile_pattern;
pub use self::compile_pattern::compile_pattern;

mod simplify_branches;
pub use self::simplify_branches::simplify_branches;

mod remove_orphan_blocks;
pub use self::remove_orphan_blocks::remove_orphan_blocks;

mod remove_compounds;
pub use self::remove_compounds::remove_compounds;

mod promote_tail_calls;
pub use self::promote_tail_calls::promote_tail_calls;

use eir::FunctionBuilder;

pub fn eir_normal_passes(eir: &mut ::eir::Module) {

    let mut fun_idents: Vec<_> = eir.functions.iter()
        .map(|(k, _v)| k.clone()).collect();
    fun_idents.sort();

    println!("STAGE: Functionwise");
    for fun_ident in fun_idents.iter() {
        let mut function = eir.functions.get_mut(fun_ident).unwrap();
        println!("Function: {}", function.ident());

        let mut builder = FunctionBuilder::new(&mut function);

        promote_tail_calls(&mut builder);

        // Remove orphans in generated LIR
        remove_orphan_blocks(&mut builder);
        builder.function().validate();

        compile_pattern(&mut builder);
        simplify_branches(&mut builder);
        remove_orphan_blocks(&mut builder);

        builder.function().validate();

    }

}
