use std::collections::BTreeMap;
use std::error::Error;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleError};
use cranelift::prelude::*;

use crate::code::{FunIdx, HLType, StrIdx, TypeIdx, UStrIdx};
use crate::sys::{hl_type, hl_type_fun, hl_type_kind};

mod data;
mod emit;

#[derive(Default)]
struct Indexes {
    types: BTreeMap<TypeIdx, DataId>,
    ustr: BTreeMap<UStrIdx, DataId>,
    fn_map: BTreeMap<FunIdx, FuncId>,
}

pub struct CodegenCtx<M: Module> {
    m: M,
    f_ctx: FunctionBuilderContext,
    ctx: Context,
    idxs: Indexes,
    code: crate::code::Code,
}

impl<M: Module> CodegenCtx<M> {
    pub fn new(m: M, code: crate::code::Code) -> Self {
        let ctx = m.make_context();
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
            code,
        }
    }

    pub fn finish(self) -> (M, FuncId) {
        (self.m, self.idxs.fn_map[&self.code.entrypoint])
    }
}
