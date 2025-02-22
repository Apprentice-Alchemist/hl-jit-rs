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
    globals: BTreeMap<GlobalIdx, DataId>,
    native_calls: BTreeMap<&'static str, FuncId>,
}

static NATIVE_CALLS: &[(&'static str, &[Type], &[Type])] = &[
    ("fmod", &[types::F64], &[types::F64]),
    ("fmodf", &[types::F32], &[types::F32]),
    ("hl_alloc_obj", &[types::I64], &[types::I64]),
];

fn build_native_calls(m: &mut dyn Module, idxs: &mut Indexes) {
    for (name, args, ret) in NATIVE_CALLS {
        let mut signature = m.make_signature();
        signature.params = args.iter().map(|t| AbiParam::new(*t)).collect();
        signature.returns = ret.iter().map(|t| AbiParam::new(*t)).collect();
        let id = m.declare_function(name, Linkage::Import, &signature).unwrap();
        idxs.native_calls.insert(name, id);
    }
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
        let mut idxs = Default::default();
        data::declare(m, &code, &mut idxs).unwrap();
        build_native_calls(m, &mut idxs);
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
            code,
        }
    }

    pub fn finish(mut self) -> FuncId {
        // data::declare(&mut self.m, &self.code, &mut self.idxs).unwrap();
        // for fun in &self.code.functions {
        //     let mut signature = self.m.make_signature();
        //     fill_signature(&self.code, &mut signature, fun.ty);
        //     let id = self.m.declare_anonymous_function(&signature).unwrap();
        //     self.idxs.fn_map.insert(fun.idx, id);
        // }
        // for fun in self.code.functions.iter() {
        //     emit::translate_function(self.m, &self.code, &self.idxs, fun);
        // }

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
