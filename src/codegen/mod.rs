use std::collections::BTreeMap;
use std::error::Error;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleError};
use cranelift::prelude::*;

use crate::code::{Code, FunIdx, GlobalIdx, HLType, StrIdx, TypeFun, TypeIdx, UStrIdx};
use crate::sys::{hl_module_context, hl_type, hl_type_fun, hl_type_kind};

mod data;
mod emit;

struct Indexes {
    module_context_id: DataId,
    types: Vec<DataId>,
    ustr: Vec<DataId>,
    bytes: Vec<DataId>,
    fn_map: BTreeMap<FunIdx, FuncId>,
    fn_type_map: BTreeMap<FunIdx, TypeIdx>,
    globals: BTreeMap<GlobalIdx, DataId>,
    native_calls: BTreeMap<&'static str, FuncId>,
    hash_locations: BTreeMap<UStrIdx, Vec<(DataId, usize)>>,
}

static NATIVE_CALLS: &[(&str, &[Type], &[Type])] = &[
    ("fmod", &[types::F64, types::F64], &[types::F64]),
    ("fmodf", &[types::F32, types::F32], &[types::F32]),
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
    (
        "hl_dyn_casti",
        &[types::I64, types::I64, types::I64],
        &[types::I32],
    ),
    (
        "hl_dyn_castp",
        &[types::I64, types::I64, types::I64],
        &[types::I64],
    ),
    ("hl_alloc_init", &[types::I64], &[]),
    ("hl_init_virtual", &[types::I64, types::I64], &[]),
    ("hl_init_enum", &[types::I64, types::I64], &[]),
    ("hl_alloc_dynbool", &[types::I8], &[types::I64]),
    ("hl_alloc_dynamic", &[types::I64], &[types::I64]),
    ("hl_add_root", &[types::I64], &[]),
    (
        "hl_alloc_closure_ptr",
        &[types::I64, types::I64, types::I64],
        &[types::I64],
    ),
    ("hl_alloc_enum", &[types::I64, types::I32], &[types::I64]),
    (
        "hl_dyn_call_obj",
        &[types::I64, types::I64, types::I32, types::I64, types::I64],
        &[types::I64],
    ),
    (
        "hl_dyn_call",
        &[types::I64, types::I64, types::I32],
        &[types::I64],
    ),
    ("hl_assert", &[], &[]),
    ("hl_null_access", &[], &[]),
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
        let module_context_id = m.declare_anonymous_data(true, false).unwrap();
        let idxs = Indexes {
            module_context_id,
            types: Default::default(),
            ustr: Default::default(),
            bytes: Default::default(),
            fn_map: Default::default(),
            fn_type_map: Default::default(),
            globals: Default::default(),
            native_calls: Default::default(),
            hash_locations: Default::default(),
        };
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs,
        }
    }

    pub fn compile(&mut self, code: Code) -> FuncId {
        data::declare(self.m, &code, &mut self.idxs).unwrap();
        build_native_calls(self.m, &mut self.idxs);
        data::define_types(self.m, &code, &mut self.idxs).unwrap();
        data::define_globals(self.m, &code, &self.idxs);
        data::define_strings(self.m, &code, &self.idxs).unwrap();
        for fun in &code.functions {
            let mut signature = self.m.make_signature();
            fill_signature_ty(&code, &mut signature, fun.ty);
            let id = self
                .m
                .declare_function(&format!("fun@{}", fun.idx.0), Linkage::Local, &signature)
                .unwrap();
            self.idxs.fn_map.insert(fun.idx, id);
            self.idxs.fn_type_map.insert(fun.idx, fun.ty);
        }
        for (lib, name, ty, fun_idx) in &code.natives {
            let lib = match &code[*lib] {
                "std\0" => "hl",
                "?std\0" => "hl",
                val => &val[0..val.len() - 1],
            };
            let name = &code[*name];
            let name = &name[0..name.len() - 1];

            let symbol_name = format!("{lib}_{name}");
            let mut signature = self.m.make_signature();
            fill_signature_ty(&code, &mut signature, *ty);
            let id = self
                .m
                .declare_function(&symbol_name, Linkage::Import, &signature)
                .unwrap();
            self.idxs.fn_map.insert(*fun_idx, id);
            self.idxs.fn_type_map.insert(*fun_idx, *ty);
        }
        for fun in code.functions.iter() {
            emit::emit_fun(self, &code, fun);
        }
        data::define_module_context(&mut self.m, &code, &mut self.idxs);
        self.emit_entrypoint(&code)
    }

    fn emit_entrypoint(&mut self, code: &Code) -> FuncId {
        let sig = self.m.make_signature();
        let fun_id = self
            .m
            .declare_function("hl_entry_point", Linkage::Export, &sig)
            .unwrap();
        self.m.clear_context(&mut self.ctx);
        let mut bcx = FunctionBuilder::new(&mut self.ctx.func, &mut self.f_ctx);
        let entry_block = bcx.create_block();
        bcx.seal_block(entry_block);
        bcx.switch_to_block(entry_block);

        let hl_alloc_id = self.idxs.native_calls["hl_alloc_init"];
        let hl_alloc_ref = self.m.declare_func_in_func(hl_alloc_id, bcx.func);
        let module_context = self
            .m
            .declare_data_in_func(self.idxs.module_context_id, bcx.func);
        let module_context_val = bcx.ins().global_value(types::I64, module_context);
        assert_eq!(offset_of!(hl_module_context, alloc), 0);
        bcx.ins().call(hl_alloc_ref, &[module_context_val]);

        let hl_hash_id = self.idxs.native_calls["hl_hash"];
        let hl_hash_ref = self.m.declare_func_in_func(hl_hash_id, &mut bcx.func);
        for (str, locs) in &self.idxs.hash_locations {
            let gv = self.m.declare_data_in_func(self.idxs.ustr[str.0], bcx.func);
            let str_val = bcx.ins().global_value(types::I64, gv);
            let inst = bcx.ins().call(hl_hash_ref, &[str_val]);
            let hash = bcx.inst_results(inst)[0];
            for (d, offset) in locs {
                let gv = self.m.declare_data_in_func(*d, bcx.func);
                let loc = bcx.ins().global_value(types::I64, gv);
                bcx.ins()
                    .store(MemFlags::trusted(), hash, loc, *offset as i32);
            }
        }

        let init_enum_id = self.idxs.native_calls["hl_init_enum"];
        let init_enum_ref = self.m.declare_func_in_func(init_enum_id, &mut bcx.func);
        let init_virtual_id = self.idxs.native_calls["hl_init_virtual"];
        let init_virtual_ref = self.m.declare_func_in_func(init_virtual_id, &mut bcx.func);
        let module_context_gv = self
            .m
            .declare_data_in_func(self.idxs.module_context_id, &mut bcx.func);
        let module_context_val = bcx.ins().global_value(types::I64, module_context_gv);
        for (ty, data) in self.idxs.types.iter().enumerate() {
            match &code[TypeIdx(ty)] {
                HLType::Enum(_) => {
                    let val = self.m.declare_data_in_func(*data, &mut bcx.func);
                    let val = bcx.ins().global_value(types::I64, val);
                    bcx.ins().call(init_enum_ref, &[val, module_context_val]);
                }
                HLType::Virtual(_) => {
                    let val = self.m.declare_data_in_func(*data, &mut bcx.func);
                    let val = bcx.ins().global_value(types::I64, val);
                    bcx.ins().call(init_virtual_ref, &[val, module_context_val]);
                }
                _ => continue,
            }
        }

        let hl_add_root_id = self.idxs.native_calls["hl_add_root"];
        let hl_add_root_ref = self.m.declare_func_in_func(hl_add_root_id, bcx.func);
        for (gv, data) in &self.idxs.globals {
            if !code.constants.contains_key(&gv) {
                let gv = self.m.declare_data_in_func(*data, bcx.func);
                let val = bcx.ins().global_value(types::I64, gv);
                bcx.ins().call(hl_add_root_ref, &[val]);
            }
        }

        let f_ref = self
            .m
            .declare_func_in_func(self.idxs.fn_map[&code.entrypoint], &mut bcx.func);
        bcx.ins().call(f_ref, &[]);
        bcx.ins().return_(&[]);
        bcx.finalize();
        self.m.define_function(fun_id, &mut self.ctx).unwrap();
        fun_id
    }
}

fn fill_signature_ty(code: &Code, sig: &mut Signature, ty: TypeIdx) {
    let (args, ret) = match &code[ty] {
        HLType::Function(TypeFun { args, ret }) => (args, ret),
        HLType::Method(TypeFun { args, ret }) => (args, ret),
        _ => panic!(),
    };
    fill_signature(code, sig, args, *ret);
}

fn fill_signature(code: &Code, sig: &mut Signature, args: &[TypeIdx], ret: TypeIdx) {
    sig.params.extend(
        args.iter()
            .filter_map(|idx| {
                if !code[*idx].is_void() {
                    let clir_ty = cranelift_type(&code[*idx]);
                    Some(AbiParam::new(clir_ty))
                } else {
                    None
                }
            }),
    );
    let ret_ty = &code[ret];
    if !ret_ty.is_void() {
        sig.returns.push(AbiParam::new(cranelift_type(ret_ty)));
    }
}

pub fn cranelift_type(ty: &HLType) -> cranelift::prelude::Type {
    use cranelift::prelude::types;
    match ty {
        HLType::Void => panic!("HVOID should not be used in CLIR"),
        HLType::UInt8 => types::I8,
        HLType::UInt16 => types::I16,
        HLType::Int32 => types::I32,
        HLType::Int64 => types::I64,
        HLType::Float32 => types::F32,
        HLType::Float64 => types::F64,
        HLType::Boolean => types::I8,
        HLType::Bytes => types::I64,
        HLType::Dynamic => types::I64,
        HLType::Function(_) => types::I64,
        HLType::Object(_) => types::I64,
        HLType::Array => types::I64,
        HLType::Type => types::I64,
        HLType::Reference(_) => types::I64,
        HLType::Virtual(_) => types::I64,
        HLType::Dynobj => types::I64,
        HLType::Abstract(_) => types::I64,
        HLType::Enum(_) => types::I64,
        HLType::Null(_) => types::I64,
        HLType::Method(_) => types::I64,
        HLType::Struct(_) => types::I64,
        HLType::Packed(_) => panic!("HPACKED should not be used in CLIR"),
        HLType::Guid => types::I64,
    }
}