use ::ToDoc;
use super::Module;
use ::pretty::{ Doc, BoxDoc };

impl ToDoc for Module {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc> {
        let head: Doc<BoxDoc> = Doc::text(format!("module {}:", self.name));
        let attrs: Doc<BoxDoc> = Doc::newline()
            .append(Doc::text(format!("attributes: {:?}", self.attributes)))
            .nest(2);

        let funs = self.functions.iter().map(|fun| {
            let args = Doc::intersperse(
                fun.hir_fun.args.iter().map(|arg| Doc::text(format!("{:?}", arg))),
                Doc::text(",").append(Doc::space()));
            let start_signature = Doc::concat(vec![
                Doc::newline(),
                Doc::text(format!("fun {:?} {}(", fun.visibility, fun.ident)),
            ]).group();
            let args_signature = Doc::concat(vec![
                args,
                Doc::text("):")
            ]).nest(4);
            let signature = Doc::concat(vec![
                start_signature, args_signature
            ]).group();

            let hir = Doc::newline().append(fun.hir_fun.body.to_doc());

            let lir = {
                let lir = fun.lir_function.as_ref().unwrap();
                let lir_blocks = lir.graph.node_labels().map(|node_idx| {
                    let head = Doc::newline()
                        .append(Doc::text(format!("block #{}:", node_idx)));

                    let block_container = &lir.graph[node_idx];
                    let block = block_container.inner.borrow();

                    let phis = block.phi_nodes.iter().map(|phi| {
                        Doc::newline().append(Doc::text(format!("{:?}", phi)))
                    });

                    let block_ops = block.ops.iter().map(|op| {
                        Doc::newline().append(Doc::text(format!("{:?}", op)))
                    });

                    let branches_vec = lir.graph[node_idx].outgoing.iter()
                        .map(|branch| Doc::text(format!("{:?}", branch)));
                    let branches = Doc::newline()
                        .append(Doc::text("branch ["))
                        .append(Doc::intersperse(branches_vec, Doc::text(",").append(Doc::space()))
                                .nest(2).group())
                        .append(Doc::text("]"));

                    Doc::concat(vec![
                        head,
                        Doc::concat(phis).nest(2),
                        Doc::concat(block_ops).nest(2),
                        branches.nest(2),
                    ])
                });

                Doc::concat(lir_blocks)
            };

            Doc::concat(vec![
                signature,
                Doc::newline().append(Doc::text("hir:")).nest(2),
                hir.nest(4),
                Doc::newline().append(Doc::text("lir:")).nest(2),
                lir.nest(4),
            ])
        });
        let funs_doc = Doc::concat(funs).nest(2);

        Doc::concat(vec![
            head, attrs, funs_doc
        ])
    }
}
