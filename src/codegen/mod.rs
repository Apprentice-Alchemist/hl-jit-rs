use std::alloc::Layout;
use std::collections::BTreeMap;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::codegen::ir;
use cranelift::module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift::prelude::*;
use hl_sys::hl_thread_info;
use hl_sys::hl_trap_ctx;
use hl_sys::vclosure;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;

use crate::code::{Code, FunIdx, GlobalIdx, HLType, TypeFun, TypeIdx, UStrIdx};
use crate::unwind::UnwindModule;
use hl_sys::hl_module_context;

mod data;
mod emit;

struct ObjLayout {
    layout: Layout,
    fields: Vec<(u32, TypeIdx)>,
}

struct EnumLayout {
    variants: Vec<Vec<u32>>,
}

struct Indexes {
    module_context_id: DataId,
    types: Vec<DataId>,
    ustr: Vec<DataId>,
    bytes: Vec<DataId>,
    fn_map: BTreeMap<FunIdx, (FuncId, Signature)>,
    fn_type_map: BTreeMap<FunIdx, TypeIdx>,
    globals: BTreeMap<GlobalIdx, DataId>,
    native_calls: BTreeMap<&'static str, (FuncId, Signature)>,
    hash_locations: BTreeMap<UStrIdx, Vec<(DataId, usize)>>,
    static_closures: BTreeMap<FunIdx, DataId>,
    obj_layouts: BTreeMap<TypeIdx, ObjLayout>,
    enum_layouts: BTreeMap<TypeIdx, EnumLayout>,
    hdyn_index: Option<TypeIdx>,
}

pub static LIBHL_NATIVE_CALLS: &[(&str, &[Type], &[Type])] = &[
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
    ("hl_get_thread", &[], &[types::I64]),
    ("hl_dyn_compare", &[types::I64, types::I64], &[types::I32]),
    ("hl_same_type", &[types::I64, types::I64], &[types::I8]),
    ("hl_global_init", &[], &[]),
    ("hl_global_free", &[], &[]),
    ("hl_register_thread", &[types::I64], &[]),
    ("hl_sys_init", &[types::I64, types::I32, types::I64], &[]),
    ("hl_setup_callbacks", &[types::I64, types::I64], &[]),
    ("hl_setup_exception", &[types::I64, types::I64], &[]),
    ("hl_print_exception_with_stack", &[types::I64], &[]),
];

static OTHER_NATIVES: &[(&str, &[Type], &[Type])] = &[
    ("fmod", &[types::F64, types::F64], &[types::F64]),
    ("fmodf", &[types::F32, types::F32], &[types::F32]),
    ("setjmp", &[types::I64], &[types::I32]),
    (
        "hlc_static_call",
        &[types::I64, types::I64, types::I64, types::I64],
        &[types::I64],
    ),
    ("hlc_get_wrapper", &[types::I64], &[types::I64]),
    (
        "hlc_resolve_symbol",
        &[types::I64, types::I64, types::I64],
        &[types::I64],
    ),
    (
        "hlc_capture_stack",
        &[types::I64, types::I32],
        &[types::I32],
    ),
];

fn build_native_calls(m: &mut dyn Module, idxs: &mut Indexes) {
    for (name, args, ret) in LIBHL_NATIVE_CALLS.iter().chain(OTHER_NATIVES.iter()) {
        let mut signature = m.make_signature();
        signature.params = args.iter().map(|t| AbiParam::new(*t)).collect();
        signature.returns = ret.iter().map(|t| AbiParam::new(*t)).collect();
        let id = m
            .declare_function(name, Linkage::Import, &signature)
            .unwrap();
        idxs.native_calls.insert(name, (id, signature));
    }
}

pub struct CodegenCtx<'a, T> {
    m: &'a mut UnwindModule<T>,
    idxs: Indexes,
}

impl<'a, T: Module> CodegenCtx<'a, T> {
    pub fn new(m: &'a mut UnwindModule<T>) -> Self {
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
            static_closures: Default::default(),
            obj_layouts: Default::default(),
            enum_layouts: Default::default(),
            hdyn_index: None
        };
        Self { m, idxs }
    }

    pub fn compile(&mut self, code: &Code, generate_main: bool) -> FuncId {
        self.idxs.hdyn_index = Some(TypeIdx(code.types.iter().enumerate().find(|(_, ty) | {
            matches!(ty, HLType::Dynamic)
        }).expect("missing type HDyn").0));
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
            self.idxs.fn_map.insert(fun.idx, (id, signature));
            self.idxs.fn_type_map.insert(fun.idx, fun.ty);
        }
        for native in code.natives() {
            let symbol_name = native.symbol_name();
            let mut signature = self.m.make_signature();
            fill_signature_ty(&code, &mut signature, native.ty);
            let id = self
                .m
                .declare_function(&symbol_name, Linkage::Import, &signature)
                .unwrap();
            self.idxs.fn_map.insert(native.fun, (id, signature));
            self.idxs.fn_type_map.insert(native.fun, native.ty);
        }
        for fun in code.functions.iter() {
            for fid in fun.static_closures.iter() {
                if self.idxs.static_closures.contains_key(fid) {
                    continue;
                }
                let func_id = self.idxs.fn_map[fid].0;

                let id = self.m.declare_anonymous_data(false, false).unwrap();
                let mut data = DataDescription::new();
                data.define(vec![0u8; size_of::<vclosure>()].into_boxed_slice());

                let ty_id = self.m.declare_data_in_data(
                    self.idxs.types[self.idxs.fn_type_map[&fid].0],
                    &mut data,
                );
                data.write_data_addr(0, ty_id, 0);
                let fn_id = self.m.declare_func_in_data(func_id, &mut data);
                data.write_function_addr(8, fn_id);

                self.m.define_data(id, &data).unwrap();
                self.idxs.static_closures.insert(*fid, id);
            }
        }
        let isa = self.m.isa();
        let fns = code
            .functions
            .par_iter()
            .map(|fun| {
                let mut ctx = Context::new();
                emit::emit_fun(isa, &self.idxs, &code, fun, &mut ctx);
                ctx.compile(isa, &mut Default::default()).unwrap();
                let id = self.idxs.fn_map[&fun.idx].0;
                let compiled_code = ctx.take_compiled_code().unwrap();
                let unwind_info = compiled_code.create_unwind_info(isa).unwrap();
                let buffer = &compiled_code.buffer;
                let relocs = buffer
                    .relocs()
                    .iter()
                    .map(|reloc| {
                        cranelift::module::ModuleReloc::from_mach_reloc(&reloc, &ctx.func, id)
                    })
                    .collect::<Vec<_>>();
                (id, compiled_code, relocs, unwind_info)
            })
            .collect::<Vec<_>>();
        for (func_id, compiled_code, relocs, unwind_info) in fns {
            if let Some(unwind_info) = unwind_info {
                self.m.add_unwind_info(func_id, unwind_info);
            }
            self.m
                .define_function_bytes(
                    func_id,
                    compiled_code.buffer.alignment as u64,
                    compiled_code.buffer.data(),
                    &relocs,
                )
                .unwrap();
        }
        data::define_module_context(&mut self.m, &code, &mut self.idxs);
        let entrypoint_id = self.emit_entrypoint(&code);
        if generate_main {
            self.emit_main(&code, entrypoint_id);
        }
        entrypoint_id
    }

    fn native_fun_ref(&mut self, name: &str, func: &mut ir::Function) -> ir::FuncRef {
        let (id, signature) = &self.idxs.native_calls[name];
        emit::declare_func_in_func_with_sig(*id, signature, false, func)
    }

    fn emit_main(&mut self, _code: &Code, entrypoint_id: FuncId) -> FuncId {
        let mut sig = self.m.make_signature();
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        let fun_id = self
            .m
            .declare_function("main", Linkage::Export, &sig)
            .unwrap();
        let mut ctx = self.m.make_context();
        let mut f_ctx = FunctionBuilderContext::new();
        let mut bcx = FunctionBuilder::new(&mut ctx.func, &mut f_ctx);
        bcx.func.signature = sig;
        let entry_block = bcx.create_block();
        bcx.append_block_params_for_function_params(entry_block);
        bcx.seal_block(entry_block);
        bcx.switch_to_block(entry_block);

        let dummy_slot = bcx.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size_of::<bool>() as u32,
            1,
        ));

        let hl_global_init_ref = self.native_fun_ref("hl_global_init", bcx.func);
        bcx.ins().call(hl_global_init_ref, &[]);

        // let hlc_static_call_id = self.idxs.native_calls["hlc_static_call"];
        // let hlc_static_call_ref = self.m.declare_func_in_func(hlc_static_call_id, bcx.func);
        // let hlc_static_call_val = bcx.ins().func_addr(types::I64, hlc_static_call_ref);
        // let hlc_get_wrapper_id = self.idxs.native_calls["hlc_get_wrapper"];
        // let hlc_get_wrapper_ref = self.m.declare_func_in_func(hlc_get_wrapper_id, bcx.func);
        // let hlc_get_wrapper_val = bcx.ins().func_addr(types::I64, hlc_get_wrapper_ref);
        // let hl_setup_callbacks_id = self.idxs.native_calls["hl_setup_callbacks"];
        // let hl_setup_callbacks_ref = self.m.declare_func_in_func(hl_setup_callbacks_id, bcx.func);
        // bcx.ins().call(
        //     hl_setup_callbacks_ref,
        //     &[hlc_static_call_val, hlc_get_wrapper_val],
        // );

        // let hlc_resolve_symbol_id = self.idxs.native_calls["hlc_resolve_symbol"];
        // let hlc_resolve_symbol_ref = self.m.declare_func_in_func(hlc_resolve_symbol_id, bcx.func);
        // let hlc_resolve_symbol_val = bcx.ins().func_addr(types::I64, hlc_resolve_symbol_ref);
        // let hlc_capture_stack_id = self.idxs.native_calls["hlc_capture_stack"];
        // let hlc_capture_stack_ref = self.m.declare_func_in_func(hlc_capture_stack_id, bcx.func);
        // let hlc_capture_stack_val = bcx.ins().func_addr(types::I64, hlc_get_wrapper_ref);
        // let hl_setup_exception_id = self.idxs.native_calls["hl_setup_exception"];
        // let hl_setup_exception_ref = self.m.declare_func_in_func(hl_setup_exception_id, bcx.func);
        // bcx.ins().call(hl_setup_exception_ref, &[hlc_resolve_symbol_val, hlc_capture_stack_val]);

        let hl_register_thread_ref = self.native_fun_ref("hl_register_thread", bcx.func);
        let stack_top = bcx.ins().stack_addr(types::I64, dummy_slot, 0);
        bcx.ins().call(hl_register_thread_ref, &[stack_top]);

        let hl_sys_init_ref = self.native_fun_ref("hl_sys_init", bcx.func);

        let argc = bcx.block_params(entry_block)[0];
        let argv = bcx.block_params(entry_block)[1];
        let argv = bcx.ins().iadd_imm(argv, 8);
        let argc = bcx.ins().iadd_imm(argc, -1);
        let zero = bcx.ins().iconst(types::I64, 0);
        bcx.ins().call(hl_sys_init_ref, &[argv, argc, zero]);

        let hl_get_thread_ref = self.native_fun_ref("hl_get_thread", bcx.func);
        let setjmp_ref = self.native_fun_ref("setjmp", bcx.func);

        let end_block = bcx.create_block();
        bcx.append_block_param(end_block, types::I32);

        {
            let slot = bcx.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                size_of::<hl_trap_ctx>() as u32,
                3,
            ));
            let zero = bcx.ins().iconst(types::I64, 0);
            bcx.ins()
                .stack_store(zero, slot, offset_of!(hl_trap_ctx, tcheck) as i32);

            let tinf = {
                let inst = bcx.ins().call(hl_get_thread_ref, &[]);
                bcx.inst_results(inst)[0]
            };

            let trap_current = bcx.ins().load(
                types::I64,
                MemFlags::trusted(),
                tinf,
                offset_of!(hl_thread_info, trap_current) as i32,
            );
            bcx.ins()
                .stack_store(trap_current, slot, offset_of!(hl_trap_ctx, prev) as i32);
            let ctx_addr = bcx.ins().stack_addr(types::I64, slot, 0);
            bcx.ins().store(
                MemFlags::trusted(),
                ctx_addr,
                tinf,
                offset_of!(hl_thread_info, trap_current) as i32,
            );

            let env = bcx
                .ins()
                .stack_addr(types::I64, slot, offset_of!(hl_trap_ctx, buf) as i32);
            let exc_block = bcx.create_block();

            let call_block = bcx.create_block();

            let setjmp_inst = bcx.ins().call(setjmp_ref, &[env]);
            let r = bcx.inst_results(setjmp_inst)[0];
            bcx.ins().brif(r, exc_block, &[], call_block, &[]);
            bcx.seal_block(call_block);
            bcx.switch_to_block(call_block);

            let entrypoint_ref = emit::declare_func_in_func_with_sig(
                entrypoint_id,
                &self
                    .m
                    .declarations()
                    .get_function_decl(entrypoint_id)
                    .signature,
                false,
                bcx.func,
            );
            bcx.ins().call(entrypoint_ref, &[]);

            let zero = bcx.ins().iconst(types::I32, 0);
            bcx.ins().jump(end_block, &[zero.into()]);

            bcx.switch_to_block(exc_block);
            let exc_value = bcx.ins().load(
                types::I64,
                MemFlags::trusted(),
                tinf,
                offset_of!(hl_thread_info, exc_value) as i32,
            );

            let hl_print_exception_ref =
                self.native_fun_ref("hl_print_exception_with_stack", bcx.func);
            bcx.ins().call(hl_print_exception_ref, &[exc_value]);

            let ret = bcx.ins().iconst(types::I32, 1);
            bcx.ins().jump(end_block, &[ret.into()]);
            bcx.seal_block(exc_block);
        }
        bcx.seal_block(end_block);
        bcx.switch_to_block(end_block);

        let hl_global_free_ref = self.native_fun_ref("hl_global_free", bcx.func);
        bcx.ins().call(hl_global_free_ref, &[]);
        let rval = bcx.block_params(end_block)[0];
        bcx.ins().return_(&[rval]);
        bcx.finalize();
        self.m.define_function(fun_id, &mut ctx).unwrap();
        fun_id
    }

    fn emit_entrypoint(&mut self, code: &Code) -> FuncId {
        let sig = self.m.make_signature();
        let fun_id = self
            .m
            .declare_function("hl_entry_point", Linkage::Export, &sig)
            .unwrap();
        let mut ctx = self.m.make_context();
        let mut f_ctx = FunctionBuilderContext::new();
        let mut bcx = FunctionBuilder::new(&mut ctx.func, &mut f_ctx);
        let entry_block = bcx.create_block();
        bcx.seal_block(entry_block);
        bcx.switch_to_block(entry_block);

        let hl_alloc_init_ref = self.native_fun_ref("hl_alloc_init", bcx.func);
        let module_context = self
            .m
            .declare_data_in_func(self.idxs.module_context_id, bcx.func);
        let module_context_val = bcx.ins().global_value(types::I64, module_context);
        assert_eq!(offset_of!(hl_module_context, alloc), 0);
        bcx.ins().call(hl_alloc_init_ref, &[module_context_val]);

        let hl_hash_ref = self.native_fun_ref("hl_hash", bcx.func);
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

        let init_enum_ref = self.native_fun_ref("hl_init_enum", bcx.func);
        let init_virtual_ref = self.native_fun_ref("hl_init_virtual", bcx.func);
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

        let hl_add_root_ref = self.native_fun_ref("hl_add_root", bcx.func);
        for (gv, data) in &self.idxs.globals {
            if !code.constants.contains_key(&gv) {
                let gv = self.m.declare_data_in_func(*data, bcx.func);
                let val = bcx.ins().global_value(types::I64, gv);
                bcx.ins().call(hl_add_root_ref, &[val]);
            }
        }

        let entrypoint_id = self.idxs.fn_map[&code.entrypoint].0;
        let f_ref = emit::declare_func_in_func_with_sig(
            entrypoint_id,
            &self
                .m
                .declarations()
                .get_function_decl(entrypoint_id)
                .signature,
            false,
            bcx.func,
        );
        bcx.ins().call(f_ref, &[]);
        bcx.ins().return_(&[]);
        bcx.finalize();
        self.m.define_function(fun_id, &mut ctx).unwrap();
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
    sig.params.extend(args.iter().filter_map(|idx| {
        if !code[*idx].is_void() {
            let clir_ty = cranelift_type(&code[*idx]);
            Some(AbiParam::new(clir_ty))
        } else {
            None
        }
    }));
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
