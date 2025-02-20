use std::collections::BTreeMap;
use std::error::Error;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleError};
use cranelift::prelude::*;

use crate::code::{Code, FunIdx, GlobalIdx, HLType, StrIdx, TypeFun, TypeIdx, UStrIdx};
use crate::sys::{hl_type, hl_type_fun, hl_type_kind};

mod data;
mod emit;

#[derive(Default)]
struct Indexes {
    types: BTreeMap<TypeIdx, DataId>,
    ustr: BTreeMap<UStrIdx, DataId>,
    fn_map: BTreeMap<FunIdx, FuncId>,
    globals: BTreeMap<GlobalIdx, DataId>
}

pub struct CodegenCtx<'a> {
    m: &'a mut dyn Module,
    f_ctx: FunctionBuilderContext,
    ctx: Context,
    idxs: Indexes,
    code: crate::code::Code,
}

impl<'a> CodegenCtx<'a> {
    pub fn new(m: &'a mut dyn Module, code: crate::code::Code) -> Self {
        let ctx = m.make_context();
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
            code,
        }
    }

    pub fn finish(mut self) -> FuncId {
        data::declare(&mut self.m, &self.code, &mut self.idxs).unwrap();
        for fun in &self.code.functions {
            let mut signature = self.m.make_signature();
            fill_signature(&self.code, &mut signature, fun.ty);
            let id = self.m.declare_anonymous_function(&signature).unwrap();
            self.idxs.fn_map.insert(fun.idx, id);
        }
        for fun in self.code.functions.iter() {
            emit::translate_function(self.m, &self.code, &self.idxs, fun);
        }

        self.idxs.fn_map[&self.code.entrypoint]
    }
}

fn fill_signature(code: &Code, sig: &mut Signature, ty: TypeIdx) {
    let (args, ret) = match code.get_type(ty) {
        HLType::Function(TypeFun { args, ret }) => (args, ret),
        HLType::Method(TypeFun { args, ret }) => (args, ret),
        _ => panic!(),
    };
    sig.params.extend(
        args.iter()
            .map(|idx| AbiParam::new(code.get_type(*idx).cranelift_type())),
    );
    sig.returns
        .push(AbiParam::new(code.get_type(*ret).cranelift_type()));
}
