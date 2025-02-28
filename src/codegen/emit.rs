use std::alloc::Layout;
use std::ffi::c_int;
use std::{collections::BTreeMap, mem::offset_of};

use cranelift::codegen::ir::{BlockCall, Inst, SourceLoc, ValueListPool};
use cranelift::frontend::Switch;
use cranelift::prelude::*;
use cranelift::{codegen::ir::StackSlot, module::Module};

use crate::code::TypeFun;
use crate::sys::{vclosure, vdynamic, venum};
use crate::{
    code::{Code, HLFunction, HLType, TypeIdx, TypeObj, UStrIdx},
    opcode::{Idx, OpCode, Reg},
    sys::{hl_type, varray, vvirtual},
};

use super::{CodegenCtx, Indexes};

pub fn emit_fun(ctx: &mut CodegenCtx, code: &Code, fun: &HLFunction) {
    let mut emit_ctx = EmitCtx::new(ctx, code, fun);
    emit_ctx.translate_body();
    emit_ctx.finish();
    if let Err(e) = ctx.ctx.verify(ctx.m.isa()) {
        eprintln!(
            "{}",
            cranelift::codegen::print_errors::pretty_verifier_error(&ctx.ctx.func, None, e)
        );
        std::process::exit(1);
    }
    ctx.m
        .define_function(ctx.idxs.fn_map[&fun.idx], &mut ctx.ctx)
        .unwrap();
}

struct EmitCtx<'a> {
    m: &'a mut dyn Module,
    code: &'a Code,
    idxs: &'a Indexes,
    fun: &'a HLFunction,
    builder: FunctionBuilder<'a>,
    blocks: BTreeMap<usize, Block>,
    // position of current HL opcode
    pos: usize,
    field_offsets: BTreeMap<TypeIdx, Vec<u32>>,
    enum_offsets: BTreeMap<(TypeIdx, usize), Vec<u32>>,
    regs: BTreeMap<Reg, (StackSlot, Type)>,
}

impl<'a> std::ops::Deref for EmitCtx<'a> {
    type Target = FunctionBuilder<'a>;

    fn deref(&self) -> &Self::Target {
        &self.builder
    }
}

impl<'a> std::ops::DerefMut for EmitCtx<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.builder
    }
}

impl<'a> EmitCtx<'a> {
    pub fn new(c_ctx: &'a mut CodegenCtx, code: &'a Code, fun: &'a HLFunction) -> EmitCtx<'a> {
        let CodegenCtx {
            m,
            f_ctx,
            ctx,
            idxs,
        } = c_ctx;

        m.clear_context(ctx);

        let function_signature = m
            .declarations()
            .get_function_decl(idxs.fn_map[&fun.idx])
            .signature
            .clone();
        ctx.func.signature = function_signature.clone();

        let mut regs = BTreeMap::new();

        let mut builder = FunctionBuilder::new(&mut ctx.func, f_ctx);
        for (idx, ty) in fun.regs.iter().enumerate() {
            if !code[*ty].is_void() {
                let t = code[*ty].cranelift_type();
                regs.insert(
                    Reg(idx),
                    (
                        builder.create_sized_stack_slot(StackSlotData {
                            kind: StackSlotKind::ExplicitSlot,
                            size: t.bytes(),
                            align_shift: 0,
                        }),
                        t,
                    ),
                );
                // builder.declare_var(Variable::new(idx), code[*ty].cranelift_type());
            }
        }

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        builder.append_block_params_for_function_params(entry_block);
        for (idx, arg) in function_signature.params.iter().enumerate() {
            let value = builder.block_params(entry_block)[idx];
            let (slot, _) = regs[&Reg(idx)];
            builder.ins().stack_store(value, slot, 0);
        }

        let mut blocks = BTreeMap::new();
        if !matches!(fun.opcodes[0], OpCode::Label) {
            blocks.insert(0, entry_block);
        }

        EmitCtx {
            m,
            code,
            idxs,
            fun,
            builder,
            blocks,
            pos: 0,
            field_offsets: BTreeMap::new(),
            enum_offsets: BTreeMap::new(),
            regs,
        }
    }

    // TODO: only spill regs to stack that are later used with ORef

    fn load_reg(&mut self, r: &Reg) -> Value {
        let (slot, ty) = self.regs[r];
        self.ins().stack_load(ty, slot, 0)
    }

    fn store_reg(&mut self, r: &Reg, val: Value) {
        let (slot, ty) = self.regs[r];
        self.ins().stack_store(val, slot, 0);
    }

    fn reg_addr(&mut self, r: &Reg) -> Value {
        let (slot, _) = self.regs[r];
        self.ins().stack_addr(types::I64, slot, 0)
    }

    fn trap(&mut self, code: &OpCode) {
        let value = self.ins().iconst(types::I64, 0);
        self.ins().debugtrap();
    }

    fn lookup_enum_offset(&mut self, ty: TypeIdx, construct_idx: usize, field_idx: usize) -> u32 {
        if let Some(offsets) = self.enum_offsets.get(&(ty, construct_idx)) {
            return offsets[field_idx];
        }

        let mut offsets = Vec::<u32>::new();
        let construct = &self.code[ty].type_enum().unwrap().constructs[construct_idx];
        let mut layout = Layout::new::<*mut u8>();
        let (mut layout, mut offset) = layout.extend(Layout::new::<c_int>()).unwrap();

        for (pos, ty) in construct.1.iter().enumerate() {
            let field_layout = match &self.code[*ty] {
                HLType::UInt8 => Layout::new::<u8>(),
                HLType::UInt16 => Layout::new::<u16>(),
                HLType::Int32 => Layout::new::<i32>(),
                HLType::Int64 => Layout::new::<i64>(),
                HLType::Float32 => Layout::new::<f32>(),
                HLType::Float64 => Layout::new::<f64>(),
                HLType::Boolean => Layout::new::<bool>(),
                _ => Layout::new::<*mut u8>(),
            };
            (layout, offset) = layout.extend(field_layout).unwrap();
            offsets.push(offset.try_into().unwrap())
        }
        let offset = offsets[field_idx];
        self.enum_offsets.insert((ty, construct_idx), offsets);

        offset
    }

    fn lookup_field_offset(&mut self, ty: TypeIdx, field_idx: usize) -> Option<u32> {
        if let Some(offsets) = self.field_offsets.get(&ty) {
            return Some(offsets[field_idx]);
        }

        let mut offsets = Vec::<u32>::new();

        fn fill_offsets(code: &Code, ty: TypeIdx, offsets: &mut Vec<u32>) -> (usize, Layout) {
            let o = code[ty].type_obj().unwrap();
            let (nfields, layout) = if let Some(ty) = o.super_ {
                fill_offsets(code, ty, offsets)
            } else {
                (
                    0,
                    if matches!(code[ty], HLType::Object(_)) {
                        Layout::new::<*mut u8>()
                    } else {
                        Layout::from_size_align(0, 1).unwrap()
                    },
                )
            };

            let mut layout = layout;
            for (i, (str_idx, type_idx)) in o.fields.iter().enumerate() {
                let field_layout = match &code[*type_idx] {
                    HLType::UInt8 => Layout::new::<u8>(),
                    HLType::UInt16 => Layout::new::<u16>(),
                    HLType::Int32 => Layout::new::<i32>(),
                    HLType::Int64 => Layout::new::<i64>(),
                    HLType::Float32 => Layout::new::<f32>(),
                    HLType::Float64 => Layout::new::<f64>(),
                    HLType::Boolean => Layout::new::<bool>(),
                    _ => Layout::new::<*mut u8>(),
                };
                let (new_layout, offset) = layout.extend(field_layout).unwrap();
                layout = new_layout;
                offsets.push(offset.try_into().unwrap());
            }

            (nfields + o.fields.len(), layout)
        }

        let (nfields, size) = fill_offsets(self.code, ty, &mut offsets);
        let offset = offsets[field_idx];
        self.field_offsets.insert(ty, offsets);
        Some(offset)
    }

    pub fn ensure_block(&mut self, pos: usize) -> Block {
        (*self
            .blocks
            .entry(pos)
            .or_insert_with(|| self.builder.create_block()))
    }

    pub fn reg_type(&self, reg: &Reg) -> &HLType {
        &self.code[self.fun[*reg]]
    }

    pub fn translate_body(&mut self) {
        let mut has_switched = true;
        for (pos, op) in self.fun.opcodes.iter().enumerate() {
            self.pos = pos;
            if has_switched {
                has_switched = false;
            } else {
                if let Some(block) = self.blocks.get(&pos).map(|b| *b) {
                    if let Some(current_block) = self.current_block() {
                        if block != current_block {
                            if !self.builder.func.dfg.insts
                                [Inst::new(self.builder.func.dfg.num_insts() - 1)]
                            .opcode()
                            .is_terminator()
                            {
                                self.ins().jump(block, &[]);
                            }
                            self.switch_to_block(block);
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
            self.set_srcloc(SourceLoc::new(pos.try_into().unwrap()));
            match op {
                OpCode::Mov { dst, src } => {
                    let val = self.load_reg(src);
                    self.store_reg(dst, val)
                }
                OpCode::Int { dst, idx } => {
                    let val = self
                        .builder
                        .ins()
                        .iconst(types::I32, self.code.ints[idx.0 as usize] as i64);
                    self.store_reg(dst, val)
                }
                OpCode::Float { dst, idx } => {
                    let val = self
                        .builder
                        .ins()
                        .f64const(self.code.floats[idx.0 as usize]);
                    self.store_reg(dst, val);
                }
                OpCode::Bool { dst, val } => {
                    let val = self.ins().iconst(types::I8, *val as i64);
                    self.store_reg(dst, val)
                }
                OpCode::Bytes { dst, idx } => {
                    let gval = if self.code.version >= 5 {
                        todo!("handle OBytes for version >= 5")
                    } else {
                        self.m.declare_data_in_func(
                            self.idxs.ustr[&UStrIdx(idx.0 as usize)],
                            self.builder.func,
                        )
                    };
                    let val = self.ins().global_value(types::I64, gval);
                    self.store_reg(dst, val);
                }
                OpCode::String { dst, idx } => {
                    let gval = self
                        .m
                        .declare_data_in_func(self.idxs.ustr[idx], self.builder.func);
                    let val = self.ins().global_value(types::I64, gval);
                    self.store_reg(dst, val);
                }
                OpCode::Null { dst } => {
                    let val = self.ins().iconst(types::I64, 0);
                    self.store_reg(dst, val);
                }
                OpCode::Add { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().fadd(a, b);
                        self.store_reg(dst, val)
                    } else {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().iadd(a, b);
                        self.store_reg(dst, val)
                    }
                }
                OpCode::Sub { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().fsub(a, b);
                        self.store_reg(dst, val)
                    } else {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().isub(a, b);
                        self.store_reg(dst, val)
                    }
                }
                OpCode::Mul { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().fmul(a, b);
                        self.store_reg(dst, val)
                    } else {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().imul(a, b);
                        self.store_reg(dst, val)
                    }
                }
                OpCode::SDiv { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().fdiv(a, b);
                        self.store_reg(dst, val)
                    } else {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().sdiv(a, b);
                        self.store_reg(dst, val)
                    }
                }
                OpCode::UDiv { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().udiv(a, b);
                    self.store_reg(dst, val)
                }
                OpCode::SMod { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let name = match self.reg_type(dst) {
                            HLType::Float32 => "fmodf",
                            HLType::Float32 => "fmod",
                            _ => unreachable!(),
                        };
                        let id = self.idxs.native_calls[name];
                        let f = self.m.declare_func_in_func(id, self.builder.func);
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let i = self.ins().call(f, &[a, b]);
                        self.store_reg(dst, self.builder.inst_results(i)[0]);
                    } else {
                        let a = self.load_reg(a);
                        let b = self.load_reg(b);
                        let val = self.ins().srem(a, b);
                        self.store_reg(dst, val);
                    }
                }
                OpCode::UMod { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().urem(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::Shl { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().ishl(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::SShr { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().sshr(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::UShr { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().ushr(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::And { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().band(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::Or { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().bor(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::Xor { dst, a, b } => {
                    let a = self.load_reg(a);
                    let b = self.load_reg(b);
                    let val = self.ins().bxor(a, b);
                    self.store_reg(dst, val);
                }
                OpCode::Neg { dst, val } => {
                    let ty = self.reg_type(val);
                    if ty.is_float() {
                        let val = self.load_reg(val);
                        let val = self.ins().fneg(val);
                        self.store_reg(dst, val);
                    } else {
                        let val = self.load_reg(val);
                        let val = self.ins().ineg(val);
                        self.store_reg(dst, val);
                    }
                }
                OpCode::Not { dst, val } => {
                    let val = self.load_reg(val);
                    let val = self.ins().bnot(val);
                    self.store_reg(dst, val);
                }
                OpCode::Incr { dst } => {
                    let val = self.load_reg(dst);
                    let ty = self.reg_type(dst).cranelift_type();
                    let one = self.ins().iconst(ty, 1i64);
                    let new_val = self.ins().iadd(val, one);
                    self.store_reg(dst, new_val);
                }
                OpCode::Decr { dst } => {
                    let val = self.load_reg(dst);
                    let ty = self.reg_type(dst).cranelift_type();
                    let one = self.ins().iconst(ty, 1i64);
                    let new_val = self.ins().iadd(val, one);
                    self.store_reg(dst, new_val);
                }
                OpCode::Call0 { dst, f } => {
                    let f_ref = self
                        .m
                        .declare_func_in_func(self.idxs.fn_map[f], self.builder.func);
                    let i = self.ins().call(f_ref, &[]);
                    if !self.reg_type(dst).is_void() {
                        self.store_reg(dst, self.builder.inst_results(i)[0]);
                    }
                }
                OpCode::Call1 { dst, f, args }
                | OpCode::Call2 { dst, f, args }
                | OpCode::Call3 { dst, f, args }
                | OpCode::Call4 { dst, f, args }
                | OpCode::CallN { dst, f, args } => {
                    let f_ref = self
                        .m
                        .declare_func_in_func(self.idxs.fn_map[f], self.builder.func);
                    let args = &args
                        .iter()
                        .map(|r| self.load_reg(r))
                        .collect::<Vec<Value>>();
                    let i = self.ins().call(f_ref, args);
                    if !self.reg_type(dst).is_void() {
                        self.store_reg(dst, self.builder.inst_results(i)[0]);
                    }
                }

                OpCode::CallMethod { dst, fid, args } => match self.reg_type(&args[0]) {
                    HLType::Object(obj) => {
                        let vargs: Vec<Value> = args.iter().map(|r| self.load_reg(r)).collect();
                        let ty_val = self.ins().load(types::I64, MemFlags::new(), vargs[0], 0);
                        let proto_val = self.ins().load(
                            types::I64,
                            MemFlags::new(),
                            ty_val,
                            offset_of!(hl_type, vobj_proto) as i32,
                        );
                        let fun_ptr = self.ins().load(
                            types::I64,
                            MemFlags::new(),
                            proto_val,
                            (fid.0 as usize * size_of::<*mut u8>()) as i32,
                        );
                        let mut sig = self.m.make_signature();
                        super::fill_signature(
                            self.code,
                            &mut sig,
                            &args
                                .iter()
                                .map(|reg| self.fun[*reg])
                                .collect::<Vec<TypeIdx>>(),
                            self.fun[*dst],
                        );
                        let sig_ref = self.import_signature(sig);
                        let i = self.ins().call_indirect(sig_ref, fun_ptr, &vargs);
                        if !self.reg_type(dst).is_void() {
                            self.store_reg(dst, self.builder.inst_results(i)[0]);
                        }
                    }
                    HLType::Virtual(virt) => self.trap(op),
                    _ => unimplemented!("OCallMethod only works with HObj or HVirt"),
                },
                OpCode::CallThis { dst, fid, args } => self.trap(op),
                OpCode::CallClosure { dst, closure, args } => {
                    // self.ins().debugtrap();
                    match self.reg_type(closure) {
                        HLType::Dynamic => {
                            //	vdynamic *args[] = {args};
                            //  vdynamic *ret = hl_dyn_call(closure,args,nargs);
                            //  dst = hl_dyncast(ret,t_dynamic,t_dst);
                            // }
                            self.trap(op);
                        }
                        _ => {
                            // if( c->hasValue ) c->fun(value,args) else c->fun(args)
                            let closure_val = self.load_reg(closure);
                            let has_value_val = self.ins().load(
                                types::I8,
                                MemFlags::trusted(),
                                closure_val,
                                offset_of!(vclosure, hasValue) as i32,
                            );
                            let fun_ptr = self.ins().load(
                                types::I64,
                                MemFlags::trusted(),
                                closure_val,
                                offset_of!(vclosure, fun) as i32,
                            );
                            let next_block = self.next_block();
                            self.emit_brif(
                                has_value_val,
                                |ecx| {
                                    let this_val = ecx.ins().load(
                                        types::I64,
                                        MemFlags::trusted(),
                                        closure_val,
                                        offset_of!(vclosure, value) as i32,
                                    );
                                    let mut sig = ecx.m.make_signature();
                                    sig.params.push(AbiParam::new(types::I64));
                                    super::fill_signature(
                                        ecx.code,
                                        &mut sig,
                                        &args
                                            .iter()
                                            .map(|reg| ecx.fun[*reg])
                                            .collect::<Vec<TypeIdx>>(),
                                        ecx.fun[*dst],
                                    );
                                    let mut vargs = Vec::new();
                                    vargs.push(this_val);
                                    vargs.extend(args.iter().map(|a| ecx.load_reg(a)));
                                    let sig_ref = ecx.import_signature(sig);
                                    let i = ecx.ins().call_indirect(sig_ref, fun_ptr, &vargs);
                                    if !ecx.reg_type(dst).is_void() {
                                        ecx.store_reg(dst, ecx.builder.inst_results(i)[0]);
                                    }
                                },
                                |ecx| {
                                    let mut sig = ecx.m.make_signature();
                                    super::fill_signature(
                                        ecx.code,
                                        &mut sig,
                                        &args
                                            .iter()
                                            .map(|reg| ecx.fun[*reg])
                                            .collect::<Vec<TypeIdx>>(),
                                        ecx.fun[*dst],
                                    );
                                    let mut vargs = Vec::new();
                                    vargs.extend(args.iter().map(|a| ecx.load_reg(a)));
                                    let sig_ref = ecx.import_signature(sig);
                                    let i = ecx.ins().call_indirect(sig_ref, fun_ptr, &vargs);
                                    if !ecx.reg_type(dst).is_void() {
                                        ecx.store_reg(dst, ecx.builder.inst_results(i)[0]);
                                    }
                                },
                                next_block,
                            );
                        }
                    }
                }
                OpCode::StaticClosure { dst, fid } => {
                    // TODO: avoid allocation
                    let alloc_id = self.idxs.native_calls["hl_alloc_closure_ptr"];
                    let alloc_ref = self.m.declare_func_in_func(alloc_id, self.builder.func);
                    let ty_id = self.idxs.fn_type_map[fid];
                    let ty = self
                        .m
                        .declare_data_in_func(self.idxs.types[&ty_id], self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, ty);
                    let func_id = self.idxs.fn_map[fid];
                    let func_ref = self.m.declare_func_in_func(func_id, self.builder.func);
                    let func_addr = self.ins().func_addr(types::I64, func_ref);
                    let obj_val = self.ins().iconst(types::I64, 0);
                    let inst = self.ins().call(alloc_ref, &[ty_val, func_addr, obj_val]);
                    self.store_reg(dst, self.inst_results(inst)[0]);
                }
                OpCode::InstanceClosure { dst, idx, obj } => {
                    let alloc_id = self.idxs.native_calls["hl_alloc_closure_ptr"];
                    let alloc_ref = self.m.declare_func_in_func(alloc_id, self.builder.func);
                    let ty_id = self.idxs.fn_type_map[idx];
                    let ty = self
                        .m
                        .declare_data_in_func(self.idxs.types[&ty_id], self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, ty);
                    let func_id = self.idxs.fn_map[idx];
                    let func_ref = self.m.declare_func_in_func(func_id, self.builder.func);
                    let func_addr = self.ins().func_addr(types::I64, func_ref);
                    let obj_val = self.load_reg(obj);
                    let inst = self.ins().call(alloc_ref, &[ty_val, func_addr, obj_val]);
                    self.store_reg(dst, self.inst_results(inst)[0]);
                }
                OpCode::VirtualClosure { dst, obj, idx } => self.trap(op),
                OpCode::GetGlobal { dst, idx } => {
                    let global_value = self
                        .m
                        .declare_data_in_func(self.idxs.globals[idx], self.builder.func);
                    let val = self.ins().symbol_value(types::I64, global_value);
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), val, 0);
                    self.store_reg(dst, val);
                }
                OpCode::SetGlobal { idx, val } => {
                    let global_value = self
                        .m
                        .declare_data_in_func(self.idxs.globals[idx], self.builder.func);
                    let global_value = self.ins().symbol_value(types::I64, global_value);
                    let val = self.load_reg(val);
                    self.ins().store(MemFlags::new(), val, global_value, 0);
                }
                OpCode::Field { dst, obj, fid } => {
                    match &self.code[self.fun[*obj]] {
                        HLType::Struct(_) | HLType::Object(_) => {
                            let offset = self
                                .lookup_field_offset(self.fun[*obj], fid.0 as usize)
                                .unwrap();
                            let obj = self.load_reg(obj);
                            let ty = self.reg_type(dst).cranelift_type();
                            let val = self.ins().load(ty, MemFlags::new(), obj, offset as i32);
                            self.store_reg(dst, val);
                        }
                        HLType::Virtual(virt) => {
                            //  #define hl_vfields(v) ((void**)(((vvirtual*)(v))+1))
                            //  if( hl_vfields(obj)[fid] )
                            //      *hl_vfields(obj)[fid] = val;
                            //  else
                            //      hl_dyn_set(obj,hash(field),vt,val)

                            let obj_val = self.load_reg(obj);

                            let field_addr = self.ins().load(
                                types::I64,
                                MemFlags::new(),
                                obj_val,
                                size_of::<vvirtual>() as i32
                                    + (fid.0 as usize * size_of::<usize>()) as i32,
                            );

                            let next_block = self.next_block();
                            self.emit_brif(
                                field_addr,
                                |this| {
                                    let ty = this.reg_type(dst).cranelift_type();
                                    let val = this.ins().load(ty, MemFlags::new(), field_addr, 0);
                                    this.store_reg(dst, val);
                                },
                                |this| {
                                    this.emit_dyn_get(dst, obj, virt.fields[fid.0 as usize].0);
                                },
                                next_block,
                            );
                        }
                        _ => panic!(),
                    }
                }
                OpCode::SetField { obj, fid, val } => match &self.code[self.fun[*obj]] {
                    HLType::Struct(_) | HLType::Object(_) => {
                        let offset = self
                            .lookup_field_offset(self.fun[*obj], fid.0 as usize)
                            .unwrap();
                        let val = self.load_reg(val);
                        let obj = self.load_reg(obj);
                        self.ins().store(MemFlags::new(), val, obj, offset as i32);
                    }
                    HLType::Virtual(virt) => {
                        //  #define hl_vfields(v) ((void**)(((vvirtual*)(v))+1))
                        //  if( hl_vfields(obj)[fid] )
                        //      *hl_vfields(obj)[fid] = val;
                        //  else
                        //      hl_dyn_set(obj,hash(field),vt,val)

                        let obj_val = self.load_reg(obj);
                        let val_val = self.load_reg(val);
                        let field_addr = self.ins().iadd_imm(
                            obj_val,
                            size_of::<vvirtual>() as i64
                                + (fid.0 as usize * size_of::<usize>()) as i64,
                        );

                        let next_block = self.next_block();
                        self.emit_brif(
                            field_addr,
                            |this| {
                                this.ins().store(MemFlags::new(), val_val, field_addr, 0);
                            },
                            |this| {
                                this.emit_dyn_set(obj, virt.fields[fid.0 as usize].0, val);
                            },
                            next_block,
                        );
                    }
                    _ => panic!(),
                },
                OpCode::GetThis { dst, fid } => {
                    let offset = self
                        .lookup_field_offset(self.fun[Reg(0)], fid.0 as usize)
                        .unwrap();
                    let obj = self.load_reg(&Reg(0));
                    let ty = self.code[self.fun[*dst]].cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), obj, offset as i32);
                    self.store_reg(dst, val);
                }
                OpCode::SetThis { fid, val } => {
                    let offset = self
                        .lookup_field_offset(self.fun[Reg(0)], fid.0 as usize)
                        .unwrap();
                    let val = self.load_reg(val);
                    let obj = self.load_reg(&Reg(0));
                    self.ins().store(MemFlags::new(), val, obj, offset as i32);
                }
                OpCode::DynGet { dst, obj, str_idx } => {
                    self.emit_dyn_get(dst, obj, *str_idx);
                }
                OpCode::DynSet { obj, str_idx, val } => {
                    self.emit_dyn_set(obj, *str_idx, val);
                }
                OpCode::JTrue { val, offset } | OpCode::JNotNull { val, offset } => {
                    let block_then_label = self.block_for_offset(offset);
                    let block_else_label = self.next_block();
                    let val = self.load_reg(val);
                    self.ins()
                        .brif(val, block_then_label, &[], block_else_label, &[]);
                    self.switch_to_block(block_else_label);
                }
                OpCode::JFalse { val, offset } | OpCode::JNull { val, offset } => {
                    let block_else_label = self.block_for_offset(offset);
                    let block_then_label = self.next_block();
                    let val = self.load_reg(val);
                    self.builder
                        .ins()
                        .brif(val, block_then_label, &[], block_else_label, &[]);
                    self.switch_to_block(block_then_label);
                }
                OpCode::JSLt { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::SignedLessThan, Some(FloatCC::LessThan))
                }
                OpCode::JSGte { a, b, offset } => self.emit_jump(
                    a,
                    b,
                    offset,
                    IntCC::SignedGreaterThanOrEqual,
                    Some(FloatCC::GreaterThanOrEqual),
                ),
                OpCode::JSGt { a, b, offset } => self.emit_jump(
                    a,
                    b,
                    offset,
                    IntCC::SignedGreaterThan,
                    Some(FloatCC::GreaterThan),
                ),
                OpCode::JSLte { a, b, offset } => self.emit_jump(
                    a,
                    b,
                    offset,
                    IntCC::SignedLessThanOrEqual,
                    Some(FloatCC::LessThanOrEqual),
                ),
                OpCode::JULt { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::SignedLessThan, None)
                }
                OpCode::JUGte { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::SignedLessThan, None)
                }
                OpCode::JNotLt { a, b, offset } => self.emit_jump(
                    a,
                    b,
                    offset,
                    IntCC::SignedGreaterThanOrEqual,
                    Some(FloatCC::GreaterThanOrEqual),
                ),
                OpCode::JNotGte { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::SignedLessThan, Some(FloatCC::LessThan))
                }
                OpCode::JEq { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::Equal, Some(FloatCC::Equal))
                }
                OpCode::JNotEq { a, b, offset } => {
                    self.emit_jump(a, b, offset, IntCC::NotEqual, Some(FloatCC::NotEqual))
                }
                OpCode::JAlways { offset } => {
                    let block = self.block_for_offset(offset);
                    self.ins().jump(block, &[]);
                }
                OpCode::ToDyn { dst, val } => match self.reg_type(val) {
                    HLType::Boolean => {
                        let func_id = self.idxs.native_calls["hl_alloc_dynbool"];
                        let func_ref = self.m.declare_func_in_func(func_id, self.builder.func);
                        let val = self.load_reg(val);
                        let inst = self.ins().call(func_ref, &[val]);
                        self.store_reg(dst, self.inst_results(inst)[0]);
                    }
                    ty => {
                        let is_ptr = ty.is_ptr();
                        let val_val = self.load_reg(val);
                        let emit_alloc_dynamic = |ecx: &mut EmitCtx<'_>| {
                            let func_id = ecx.idxs.native_calls["hl_alloc_dynamic"];
                            let func_ref = ecx.m.declare_func_in_func(func_id, ecx.builder.func);
                            let gv = ecx.m.declare_data_in_func(
                                ecx.idxs.types[&ecx.fun[*val]],
                                ecx.builder.func,
                            );
                            let clir_ty = ecx.ins().global_value(types::I64, gv);
                            let inst = ecx.ins().call(func_ref, &[clir_ty]);
                            let dyn_val = ecx.inst_results(inst)[0];
                            ecx.ins().store(
                                MemFlags::new(),
                                val_val,
                                dyn_val,
                                offset_of!(vdynamic, v) as i32,
                            );
                            ecx.store_reg(dst, dyn_val);
                        };
                        if is_ptr {
                            let next_block = self.next_block();
                            self.emit_brif(
                                val_val,
                                emit_alloc_dynamic,
                                |ecx| {
                                    let null = ecx.ins().iconst(types::I64, 0);
                                    ecx.store_reg(dst, null);
                                },
                                next_block,
                            );
                        } else {
                            emit_alloc_dynamic(self);
                        }
                    }
                },
                OpCode::ToSFloat { dst, val } => {
                    let src_ty = self.reg_type(val).cranelift_type();
                    let dst_ty = self.reg_type(dst).cranelift_type();
                    let val = self.load_reg(val);
                    let val = match (src_ty, dst_ty) {
                        (types::F32, types::F64) => self.ins().fpromote(types::F64, val),
                        (types::F64, types::F32) => self.ins().fdemote(types::F32, val),
                        (a, b) if a.is_int() && b.is_float() => {
                            self.ins().fcvt_from_sint(dst_ty, val)
                        }
                        _ => panic!("Invalid OToSFloat"),
                    };
                    self.store_reg(dst, val)
                }
                OpCode::ToUFloat { dst, val } => {
                    let val = self.load_reg(val);
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.builder.ins().fcvt_from_uint(ty, val);
                    self.store_reg(dst, val)
                }
                OpCode::ToInt { dst, val } => {
                    let src_ty = self.reg_type(val).cranelift_type();
                    let dst_ty = self.reg_type(dst).cranelift_type();
                    let val = self.load_reg(val);
                    let val = if src_ty.is_int() && dst_ty.is_int() {
                        if src_ty.wider_or_equal(dst_ty) {
                            self.ins().ireduce(dst_ty, val)
                        } else {
                            self.ins().sextend(dst_ty, val)
                        }
                    } else {
                        if dst_ty.bytes() < 4 {
                            let val = self.ins().fcvt_to_sint_sat(types::I32, val);
                            self.ins().ireduce(dst_ty, val)
                        } else {
                            self.ins().fcvt_to_sint_sat(dst_ty, val)
                        }
                    };
                    self.store_reg(dst, val)
                }
                OpCode::SafeCast { dst, val } => {
                    let val_val = self.reg_addr(val);
                    let dst_val = self.load_reg(val);

                    let cast_name = match self.reg_type(dst) {
                        HLType::Float32 => "hl_dyn_castf",
                        HLType::Float64 => "hl_dyn_castd",
                        HLType::Int64 => "hl_dyn_casti64",
                        HLType::Int32 | HLType::UInt16 | HLType::UInt8 => "hl_dyn_casti",
                        _ => "hl_dyn_castp",
                    };
                    let cast_id = self.idxs.native_calls[cast_name];
                    let cast_ref = self.m.declare_func_in_func(cast_id, self.builder.func);
                    let inst = match self.reg_type(dst) {
                        HLType::Float32 | HLType::Float64 | HLType::Int64 => {
                            let data_id = self.idxs.types[&self.fun[*val]];
                            let global_value =
                                self.m.declare_data_in_func(data_id, self.builder.func);
                            let ty = self.ins().global_value(types::I64, global_value);
                            self.ins().call(cast_ref, &[val_val, ty])
                        }
                        _ => {
                            let data_id = self.idxs.types[&self.fun[*val]];
                            let global_value =
                                self.m.declare_data_in_func(data_id, self.builder.func);
                            let src_ty = self.ins().global_value(types::I64, global_value);
                            let data_id = self.idxs.types[&self.fun[*dst]];
                            let global_value =
                                self.m.declare_data_in_func(data_id, self.builder.func);
                            let dst_ty = self.ins().global_value(types::I64, global_value);
                            self.ins().call(cast_ref, &[val_val, src_ty, dst_ty])
                        }
                    };
                    self.store_reg(dst, self.inst_results(inst)[0]);
                }
                OpCode::UnsafeCast { dst, val } => {
                    let val = self.load_reg(val);
                    self.store_reg(dst, val);
                }
                OpCode::ToVirtual { dst, val } => {
                    let val = self.load_reg(val);

                    let ty_id = self.idxs.types[&self.fun[*dst]];
                    let gv = self.m.declare_data_in_func(ty_id, self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, gv);

                    let id = self.idxs.native_calls["hl_to_virtual"];
                    let f = self.m.declare_func_in_func(id, self.builder.func);
                    let inst = self.ins().call(f, &[ty_val, val]);
                    self.store_reg(dst, self.builder.inst_results(inst)[0]);
                }
                OpCode::Label => {
                    if !self.blocks.contains_key(&pos) {
                        let next_block = self.ensure_block(pos);
                        self.ins().jump(next_block, &[]);
                        self.switch_to_block(next_block);
                    }
                }
                OpCode::Ret(reg) => {
                    if self.reg_type(reg).is_void() {
                        self.ins().return_(&[]);
                    } else {
                        let val = self.load_reg(reg);
                        self.ins().return_(&[val]);
                    }
                    let next_block = self.next_block();
                    self.switch_to_block(next_block);
                }
                OpCode::Throw(reg) => {
                    let val = self.load_reg(reg);
                    let id = self.idxs.native_calls["hl_rethrow"];
                    let f = self.m.declare_func_in_func(id, self.builder.func);
                    self.ins().call(f, &[val]);
                    self.ins().trap(TrapCode::unwrap_user(1)); // terminate block
                }
                OpCode::Rethrow(reg) => {
                    let val = self.load_reg(reg);
                    let id = self.idxs.native_calls["hl_throw"];
                    let f = self.m.declare_func_in_func(id, self.builder.func);
                    self.ins().call(f, &[val]);
                    self.ins().trap(TrapCode::unwrap_user(1)); // terminate block
                }
                OpCode::Switch { val, cases, end } => {
                    let val = self.load_reg(val);
                    let mut switch = Switch::new();
                    for (val, case) in cases.iter().enumerate() {
                        switch.set_entry(val as u128, self.block_for_offset(case));
                    }
                    let end_block = self.block_for_offset(end);
                    switch.emit(self, val, end_block);
                    let next_block = self.next_block();
                    self.switch_to_block(next_block);
                }
                OpCode::NullCheck(reg) => {
                    let next_block = self.next_block();
                    let val = self.load_reg(reg);
                    let null_block = self.create_block();
                    self.ins().brif(val, next_block, &[], null_block, &[]);
                    self.switch_to_block(null_block);
                    self.ins().trap(TrapCode::unwrap_user(2)); // TODO: hl_null_access
                    self.switch_to_block(next_block);
                }
                OpCode::Trap { dst, jump_off } => {
                    self.block_for_offset(jump_off);
                }
                OpCode::EndTrap { something } => {
                    self.block_for_offset(something);
                }
                OpCode::GetI8 { dst, mem, offset } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.ins().uload8(types::I32, MemFlags::new(), ptr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::GetI16 { dst, mem, offset } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.ins().uload8(types::I32, MemFlags::new(), ptr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::GetMem { dst, mem, offset } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), ptr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::GetArray { dst, mem, offset } => {
                    let ty = self.reg_type(dst).cranelift_type();

                    let arr_addr = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let mem_addr = self.ins().iadd_imm(arr_addr, size_of::<varray>() as i64);
                    let offset = self.ins().imul_imm(offset, ty.bytes() as i64);
                    let val_addr = self.ins().iadd(mem_addr, offset);
                    let val = self.ins().load(ty, MemFlags::trusted(), val_addr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::SetI8 { mem, offset, val } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.load_reg(val);
                    self.ins().istore8(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetI16 { mem, offset, val } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.load_reg(val);
                    self.ins().istore16(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetMem { mem, offset, val } => {
                    let mem = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.load_reg(val);
                    self.ins().store(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetArray { mem, offset, val } => {
                    let ty = self.reg_type(val).cranelift_type();

                    let arr_addr = self.load_reg(mem);
                    let offset = self.load_reg(offset);
                    let offset = self.ins().sextend(types::I64, offset);
                    let mem_addr = self.ins().iadd_imm(arr_addr, size_of::<varray>() as i64);
                    let offset = self.ins().imul_imm(offset, ty.bytes() as i64);
                    let val_addr = self.ins().iadd(mem_addr, offset);
                    let val = self.load_reg(val);
                    self.ins().store(MemFlags::trusted(), val, val_addr, 0);
                }
                OpCode::New { dst } => match self.reg_type(dst) {
                    HLType::Object(_) | HLType::Struct(_) => {
                        let ty_id = self.idxs.types[&self.fun[*dst]];
                        let gv = self.m.declare_data_in_func(ty_id, self.builder.func);
                        let ptr_ty = self.m.isa().pointer_type();
                        let val = self.ins().global_value(ptr_ty, gv);
                        let func_ref = self.m.declare_func_in_func(
                            self.idxs.native_calls["hl_alloc_obj"],
                            self.builder.func,
                        );
                        let inst = self.ins().call(func_ref, &[val]);
                        self.store_reg(dst, self.builder.inst_results(inst)[0]);
                    }
                    HLType::Dynobj => {
                        let func_ref = self.m.declare_func_in_func(
                            self.idxs.native_calls["hl_alloc_dynobj"],
                            self.builder.func,
                        );
                        let inst = self.ins().call(func_ref, &[]);
                        self.store_reg(dst, self.builder.inst_results(inst)[0]);
                    }
                    HLType::Virtual(_) => {
                        let ty_id = self.idxs.types[&self.fun[*dst]];
                        let gv = self.m.declare_data_in_func(ty_id, self.builder.func);
                        let ptr_ty = self.m.isa().pointer_type();
                        let val = self.ins().global_value(ptr_ty, gv);
                        let func_ref = self.m.declare_func_in_func(
                            self.idxs.native_calls["hl_alloc_virtual"],
                            self.builder.func,
                        );
                        let inst = self.ins().call(func_ref, &[val]);
                        self.store_reg(dst, self.builder.inst_results(inst)[0]);
                    }
                    _ => panic!("invalid ONew"),
                },
                OpCode::ArraySize { dst, arr } => {
                    let arr = self.load_reg(arr);
                    let val = self.ins().load(
                        types::I32,
                        MemFlags::trusted(),
                        arr,
                        offset_of!(varray, size) as i32,
                    );
                    self.store_reg(dst, val);
                }
                OpCode::Type { dst, idx } => {
                    let ty_id = self.idxs.types[idx];
                    let gv = self.m.declare_data_in_func(ty_id, self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, gv);
                    self.store_reg(dst, ty_val);
                }
                OpCode::GetType { dst, val } => {
                    let val = self.load_reg(val);
                    let next_block = self.next_block();
                    self.emit_brif(
                        val,
                        |ecx| {
                            let ty_val = ecx.ins().load(types::I64, MemFlags::new(), val, 0);
                            ecx.store_reg(dst, ty_val);
                        },
                        |ecx| {
                            assert!(
                                matches!(ecx.code[TypeIdx(0)], HLType::Void),
                                "HVoid does not have index 0"
                            );

                            let ty_id = ecx.idxs.types[&TypeIdx(0)];
                            let gv = ecx.m.declare_data_in_func(ty_id, ecx.builder.func);
                            let ty_val = ecx.ins().global_value(types::I64, gv);
                            ecx.store_reg(dst, ty_val);
                        },
                        next_block,
                    );
                }
                OpCode::GetTid { dst, val } => {
                    let val = self.load_reg(val);
                    let kind_val = self.ins().load(types::I32, MemFlags::new(), val, 0);
                    self.store_reg(dst, kind_val);
                }
                OpCode::Ref { dst, val } => {
                    let (stack_slot, _) = self.regs[val];
                    let val = self.ins().stack_addr(types::I64, stack_slot, 0);
                    self.store_reg(dst, val);
                }
                OpCode::Unref { dst, r } => {
                    let addr = self.load_reg(r);
                    let ty = match self.reg_type(r) {
                        HLType::Reference(t) => self.code[*t].cranelift_type(),
                        _ => panic!(),
                    };
                    let val = self.ins().load(ty, MemFlags::trusted(), addr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::Setref { r, val } => {
                    let addr = self.load_reg(r);
                    let val = self.load_reg(val);
                    self.ins().store(MemFlags::trusted(), val, addr, 0);
                }
                OpCode::MakeEnum {
                    dst,
                    construct_idx,
                    params,
                } => {
                    let alloc_id = self.idxs.native_calls["hl_alloc_enum"];
                    let alloc_ref = self.m.declare_func_in_func(alloc_id, self.builder.func);
                    let ty = self.idxs.types[&self.fun[*dst]];
                    let gv = self.m.declare_data_in_func(ty, self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, gv);
                    let idx_val = self.ins().iconst(types::I32, construct_idx.0 as i64);
                    let inst = self.ins().call(alloc_ref, &[ty_val, idx_val]);
                    let enum_val = self.builder.inst_results(inst)[0];
                    for (pos, param) in params.iter().enumerate() {
                        let offset =
                            self.lookup_enum_offset(self.fun[*dst], construct_idx.0 as usize, pos);
                        let val = self.load_reg(param);
                        self.ins()
                            .store(MemFlags::new(), val, enum_val, offset as i32);
                    }
                    self.store_reg(dst, enum_val);
                }
                OpCode::EnumAlloc { dst, idx } => {
                    let alloc_id = self.idxs.native_calls["hl_alloc_enum"];
                    let alloc_ref = self.m.declare_func_in_func(alloc_id, self.builder.func);
                    let ty = self.idxs.types[&self.fun[*dst]];
                    let gv = self.m.declare_data_in_func(ty, self.builder.func);
                    let ty_val = self.ins().global_value(types::I64, gv);
                    let idx_val = self.ins().iconst(types::I32, idx.0 as i64);
                    let inst = self.ins().call(alloc_ref, &[ty_val, idx_val]);
                    self.store_reg(dst, self.builder.inst_results(inst)[0]);
                }
                OpCode::EnumIndex { dst, val } => {
                    let enum_val = self.load_reg(val);
                    let idx = self.ins().load(
                        types::I32,
                        MemFlags::new(),
                        enum_val,
                        offset_of!(venum, index) as i32,
                    );
                    self.store_reg(dst, idx);
                }
                OpCode::EnumField {
                    dst,
                    obj,
                    construct_idx,
                    field_idx,
                } => {
                    let offset = self.lookup_enum_offset(
                        self.fun[*obj],
                        construct_idx.0 as usize,
                        field_idx.0 as usize,
                    );
                    let dst_ty = self.reg_type(dst).cranelift_type();
                    let obj_val = self.load_reg(obj);
                    let val = self
                        .ins()
                        .load(dst_ty, MemFlags::new(), obj_val, offset as i32);
                    self.store_reg(dst, val);
                }
                OpCode::SetEnumField {
                    obj,
                    field_idx,
                    val,
                } => {
                    let enum_val = self.load_reg(obj);
                    let offset = self.lookup_enum_offset(self.fun[*obj], 0, field_idx.0 as usize);
                    let val = self.load_reg(val);
                    self.ins()
                        .store(MemFlags::new(), val, enum_val, offset as i32);
                }
                OpCode::Assert => self.trap(op),
                OpCode::RefData { dst, r } => match self.reg_type(r) {
                    HLType::Array => {
                        let r = self.load_reg(r);
                        let addr = self.ins().iadd_imm(r, size_of::<varray>() as i64);
                        self.store_reg(dst, addr);
                    }
                    _ => panic!("ORefData only supports arrays"),
                },
                OpCode::RefOffset { dst, r, off } => {
                    let ty = self.reg_type(dst).cranelift_type();
                    let r = self.load_reg(r);
                    let off = self.load_reg(off);
                    let off = self.ins().imul_imm(off, ty.bytes() as i64);
                    let addr = self.ins().iadd(r, off);
                    let val = self.ins().load(ty, MemFlags::new(), addr, 0);
                    self.store_reg(dst, val);
                }
                OpCode::Nop => (),
                OpCode::Prefetch { args } => panic!("unsupported instruction: OPrefetch"),
                OpCode::Asm { args } => panic!("unsupported instruction: OAsm"),
            };
        }
        self.seal_all_blocks();
    }

    fn emit_dyn_get(&mut self, dst: &Reg, obj: &Reg, field_name: UStrIdx) {
        let obj_val = self.load_reg(obj);
        let hash_ref = self
            .m
            .declare_func_in_func(self.idxs.native_calls["hl_hash"], self.builder.func);
        let field_name_data_id = self.idxs.ustr[&field_name];
        let field_name_global_value = self
            .m
            .declare_data_in_func(field_name_data_id, self.builder.func);
        let field_name_value = self.ins().global_value(types::I64, field_name_global_value);
        let hash_inst = self.ins().call(hash_ref, &[field_name_value]);
        let hashed_field = self.builder.inst_results(hash_inst)[0];

        let dyn_get_name = match &self.code[self.fun[*dst]] {
            HLType::Float32 => "hl_dyn_getf",
            HLType::Float64 => "hl_dyn_getd",
            HLType::Int64 => "hl_dyn_geti64",
            HLType::Boolean | HLType::UInt8 | HLType::UInt16 | HLType::Int32 => "hl_dyn_geti",
            _ => "hl_dyn_getp",
        };

        let dyn_get_ref = self
            .m
            .declare_func_in_func(self.idxs.native_calls[dyn_get_name], self.builder.func);

        let inst = match &self.code[self.fun[*dst]] {
            HLType::Float32 | HLType::Float64 | HLType::Int64 => {
                self.ins().call(dyn_get_ref, &[obj_val, hashed_field])
            }
            _ => {
                let dst_ty = self
                    .m
                    .declare_data_in_func(self.idxs.types[&self.fun[*dst]], self.builder.func);
                let dst_ty_val = self.ins().global_value(types::I64, dst_ty);
                self.ins()
                    .call(dyn_get_ref, &[obj_val, hashed_field, dst_ty_val])
            }
        };
        let dst_val = self.inst_results(inst)[0];
        self.store_reg(dst, dst_val);
    }

    fn emit_dyn_set(&mut self, obj: &Reg, field_name: UStrIdx, val: &Reg) {
        let obj_val = self.load_reg(obj);
        let val_val = self.load_reg(val);
        let hash_ref = self
            .m
            .declare_func_in_func(self.idxs.native_calls["hl_hash"], self.builder.func);
        let field_name_data_id = self.idxs.ustr[&field_name];
        let field_name_global_value = self
            .m
            .declare_data_in_func(field_name_data_id, self.builder.func);
        let field_name_value = self.ins().global_value(types::I64, field_name_global_value);
        let hash_inst = self.ins().call(hash_ref, &[field_name_value]);
        let hashed_field = self.builder.inst_results(hash_inst)[0];

        let dyn_set_name = match &self.code[self.fun[*val]] {
            HLType::Float32 => "hl_dyn_setf",
            HLType::Float64 => "hl_dyn_setd",
            HLType::Int64 => "hl_dyn_seti64",
            HLType::Boolean | HLType::UInt8 | HLType::UInt16 | HLType::Int32 => "hl_dyn_seti",
            _ => "hl_dyn_setp",
        };
        let dyn_set_ref = self
            .m
            .declare_func_in_func(self.idxs.native_calls[dyn_set_name], self.builder.func);

        match &self.code[self.fun[*val]] {
            HLType::Float32 | HLType::Float64 | HLType::Int64 => self
                .ins()
                .call(dyn_set_ref, &[obj_val, hashed_field, val_val]),
            _ => {
                let gv = self
                    .m
                    .declare_data_in_func(self.idxs.types[&self.fun[*val]], self.builder.func);
                let val_ty = self.ins().global_value(types::I64, gv);
                self.ins()
                    .call(dyn_set_ref, &[obj_val, hashed_field, val_ty, val_val])
            }
        };
    }

    fn emit_brif(
        &mut self,
        val: Value,
        then_cb: impl FnOnce(&mut EmitCtx<'_>),
        else_cb: impl FnOnce(&mut EmitCtx<'_>),
        next_block: Block,
    ) {
        let block_then_label = self.create_block();
        let block_else_label = self.create_block();
        self.ins()
            .brif(val, block_then_label, &[], block_else_label, &[]);
        self.seal_block(block_then_label);
        self.seal_block(block_else_label);

        self.switch_to_block(block_then_label);
        then_cb(self);
        self.ins().jump(next_block, &[]);
        self.switch_to_block(block_else_label);
        else_cb(self);
        self.ins().jump(next_block, &[]);
        self.switch_to_block(next_block);
    }

    pub fn block_for_offset(&mut self, offset: &Idx) -> Block {
        let Some(p) = (self.pos + 1).checked_add_signed(offset.0) else {
            panic!("offset overflow")
        };
        self.ensure_block(p)
    }

    pub fn next_block(&mut self) -> Block {
        self.ensure_block(self.pos + 1)
    }

    pub fn emit_jump(
        &mut self,
        a: &crate::opcode::Reg,
        b: &crate::opcode::Reg,
        offset: &crate::opcode::Idx,
        int_cc: IntCC,
        float_cc: Option<FloatCC>,
    ) {
        let block_then_label = self.block_for_offset(offset);
        let block_else_label = self.next_block();
        let a_ty = self.reg_type(a);
        let b_ty = self.reg_type(b);
        let val = if a_ty.is_float() {
            assert!(b_ty.is_float());
            let Some(float_cc) = float_cc else {
                panic!("unsupported float comparison")
            };
            let a = self.load_reg(a);
            let b = self.load_reg(b);
            self.ins().fcmp(float_cc, a, b)
        } else {
            assert!(!b_ty.is_float());
            let a = self.load_reg(a);
            let b = self.load_reg(b);
            self.ins().icmp(int_cc, a, b)
        };
        self.ins()
            .brif(val, block_then_label, &[], block_else_label, &[]);
        self.switch_to_block(block_else_label);
    }

    pub fn finish(self) {
        self.builder.finalize();
    }
}
