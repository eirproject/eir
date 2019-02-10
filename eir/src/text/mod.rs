use pretty::{ Doc, BoxDoc };

use crate::{ FunctionIdent, Atom, SSAVariable };
use crate::op::{ Op, OpKind };

trait ToEirText {
    fn to_eir_text(&self) -> Doc<BoxDoc>;
}

impl ToEirText for SSAVariable {
    fn to_eir_text(&self) -> Doc<BoxDoc> {
        Doc::text(format!("{}", self))
    }
}

impl ToEirText for Atom {
    fn to_eir_text(&self) -> Doc<BoxDoc> {
        Doc::text(format!("{}", self))
    }
}

impl ToEirText for FunctionIdent {
    fn to_eir_text(&self) -> Doc<BoxDoc> {
        if let Some((env, sub)) = self.lambda {
            Doc::text(format!(
                "{}:{}/{}@{}.{}",
                self.module,
                self.name,
                self.arity,
                env.0,
                sub
            ))
        } else {
            Doc::text(format!(
                "{}:{}/{}",
                self.module,
                self.name,
                self.arity
            ))
        }
    }
}

impl ToEirText for Op {
    fn to_eir_text(&self) -> Doc<BoxDoc> {
        let kind_text = match &self.kind {
            OpKind::Arguments => Doc::text("arguments"),
            OpKind::UnpackEnv => Doc::text("unpack_env"),
            OpKind::Call { tail_call: true } =>
                Doc::text("tail_call"),
            OpKind::Call { tail_call: false } =>
                Doc::text("call"),
            OpKind::Apply {  tail_call: true } =>
                Doc::text("tail_apply"),
            OpKind::Apply {  tail_call: false } =>
                Doc::text("apply"),
            OpKind::CaptureNamedFunction(ident) =>
                Doc::text("capture")
                .append(Doc::space())
                .append(ident.to_eir_text()),
            OpKind::MakeNoValue => Doc::text("novalue"),
            OpKind::Jump => Doc::text("jump"),
            OpKind::PrimOp(atom) =>
                Doc::text("primop")
                .append(Doc::space())
                .append(atom.to_eir_text()),
            OpKind::ReturnOk => Doc::text("return_ok"),
            OpKind::ReturnThrow => Doc::text("return_throw"),
            OpKind::IfTruthy => Doc::text("if_truthy"),
            OpKind::CaseStart { .. } => {
                unimplemented!()
            },
            OpKind::Case(_) => Doc::text("case"),
            OpKind::CaseValues => Doc::text("case_values"),
            OpKind::CaseGuardOk => Doc::text("case_guard_ok"),
            OpKind::CaseGuardFail { .. } => Doc::text("case_guard_fail"),
            OpKind::ReceiveStart => Doc::text("receive_start"),
            OpKind::ReceiveWait => Doc::text("receive_wait"),
            OpKind::ReceiveGetMessage => Doc::text("receive_get_message"),
            OpKind::ReceiveFinish => Doc::text("receive_finish"),

            OpKind::TombstoneSSA(ssa) =>
                Doc::text("unreachable")
                .append(Doc::space())
                .append(ssa.to_eir_text()),
            OpKind::Unreachable => Doc::text("unreachable"),

            // Special cases
            OpKind::Move => Doc::nil(),
            OpKind::MakeTuple => Doc::nil(),
            OpKind::MakeList => Doc::nil(),
            OpKind::MakeMap => Doc::nil(),
            OpKind::MakeBinary => Doc::nil(),
            OpKind::PackValueList => Doc::nil(),
            OpKind::UnpackValueList => Doc::nil(),
            _ => unimplemented!(),
        };

        kind_text
    }
}
