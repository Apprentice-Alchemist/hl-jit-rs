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
    hash_locations: BTreeMap<UStrIdx, Vec<(DataId, usize)>>,
}

// HL_API void hl_dyn_seti( vdynamic *d, int hfield, hl_type *t, int value );
// HL_API void hl_dyn_seti64( vdynamic *d, int hfield, int64 value );
// HL_API void hl_dyn_setp( vdynamic *d, int hfield, hl_type *t, void *ptr );
// HL_API void hl_dyn_setf( vdynamic *d, int hfield, float f );
// HL_API void hl_dyn_setd( vdynamic *d, int hfield, double v );

static NATIVE_CALLS: &[(&'static str, &[Type], &[Type])] = &[
    ("fmod", &[types::F64], &[types::F64]),
    ("fmodf", &[types::F32], &[types::F32]),
    ("hl_alloc_obj", &[types::I64], &[types::I64]),
    ("hl_alloc_dynobj", &[], &[types::I64]),
    ("hl_alloc_virtual", &[types::I64], &[types::I64]),
    ("hl_hash", &[types::I64], &[types::I32]),
    (
        "hl_dyn_seti",
        &[types::I64, types::I32, types::I64, types::I32],
        &[],
    ),
    ("hl_dyn_seti64", &[types::I64, types::I32, types::I64], &[]),
    (
        "hl_dyn_setp",
        &[types::I64, types::I32, types::I64, types::I64],
        &[],
    ),
    ("hl_dyn_setf", &[types::I64, types::I32, types::F32], &[]),
    ("hl_dyn_setd", &[types::I64, types::I32, types::F64], &[]),
    (
        "hl_dyn_geti",
        &[types::I64, types::I32, types::I64],
        &[types::I32],
    ),
    ("hl_dyn_geti64", &[types::I64, types::I32], &[types::I64]),
    (
        "hl_dyn_getp",
        &[types::I64, types::I32, types::I64],
        &[types::I64],
    ),
    ("hl_dyn_getf", &[types::I64, types::I32], &[types::F32]),
    ("hl_dyn_getd", &[types::I64, types::I32], &[types::F64]),
    ("hl_alloc_enum", &[types::I64, types::I32], &[types::I64]),
    ("hl_throw", &[types::I64], &[]),
    ("hl_rethrow", &[types::I64], &[]),
    ("hl_to_virtual", &[types::I64, types::I64], &[types::I64]),
    ("hl_dyn_castf", &[types::I64, types::I64], &[types::F32]),
    ("hl_dyn_castd", &[types::I64, types::I64], &[types::F64]),
    ("hl_dyn_casti64", &[types::I64, types::I64], &[types::I64]),
    ("hl_dyn_casti", &[types::I64, types::I64, types::I64], &[types::I32]),
    ("hl_dyn_castp", &[types::I64, types::I64, types::I64], &[types::I64]),
];

fn build_native_calls(m: &mut dyn Module, idxs: &mut Indexes) {
    for (name, args, ret) in NATIVE_CALLS {
        let mut signature = m.make_signature();
        signature.params = args.iter().map(|t| AbiParam::new(*t)).collect();
        signature.returns = ret.iter().map(|t| AbiParam::new(*t)).collect();
        let id = m
            .declare_function(name, Linkage::Import, &signature)
            .unwrap();
        idxs.native_calls.insert(name, id);
    }
}

pub struct CodegenCtx<'a> {
    m: &'a mut dyn Module,
    f_ctx: FunctionBuilderContext,
    ctx: Context,
    idxs: Indexes,
}

impl<'a> CodegenCtx<'a> {
    pub fn new(m: &'a mut dyn Module) -> Self {
        let ctx = m.make_context();
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
        }
    }

    pub fn compile(&mut self, code: Code) -> FuncId {
        data::declare(self.m, &code, &mut self.idxs).unwrap();
        build_native_calls(self.m, &mut self.idxs);
        data::define_types(self.m, &code, &mut self.idxs);
        data::define_globals(self.m, &code, &self.idxs);
        data::define_strings(self.m, &code, &self.idxs);
        for fun in &code.functions {
            let mut signature = self.m.make_signature();
            fill_signature_ty(&code, &mut signature, fun.ty);
            let id = self.m.declare_anonymous_function(&signature).unwrap();
            self.idxs.fn_map.insert(fun.idx, id);
        }
        for (lib, name, ty, fun_idx) in &code.natives {
            let lib = match &code[*lib] {
                "std\0" => "hl",
                val => &val[0..val.len() - 1],
            };
            let name = &code[*name];
            let name = &name[0..name.len() - 1];
            let (name, optional) = if name.starts_with('?') {
                (&name[1..], true)
            } else {
                (name, false)
            };

            let symbol_name = format!("{lib}_{name}");
            let mut signature = self.m.make_signature();
            fill_signature_ty(&code, &mut signature, *ty);
            let id = self
                .m
                .declare_function(&symbol_name, Linkage::Import, &signature)
                .unwrap();
            self.idxs.fn_map.insert(*fun_idx, id);
        }
        for fun in code.functions.iter() {
            emit::emit_fun(self, &code, fun);
        }
        self.idxs.fn_map[&code.entrypoint]
    }
}

fn fill_signature_ty(code: &Code, sig: &mut Signature, ty: TypeIdx) {
    let (args, ret) = match code.get_type(ty) {
        HLType::Function(TypeFun { args, ret }) => (args, ret),
        HLType::Method(TypeFun { args, ret }) => (args, ret),
        _ => panic!(),
    };
    fill_signature(code, sig, args, *ret);
}

fn fill_signature(code: &Code, sig: &mut Signature, args: &[TypeIdx], ret: TypeIdx) {
    sig.params.extend(
        args.iter()
            .map(|idx| AbiParam::new(code.get_type(*idx).cranelift_type())),
    );
    let ret_ty = code.get_type(ret);
    if !ret_ty.is_void() {
        sig.returns.push(AbiParam::new(ret_ty.cranelift_type()));
    }
}
