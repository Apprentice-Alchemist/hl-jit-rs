use std::{collections::BTreeMap, mem::offset_of};

use cranelift::module::Module;
use cranelift::prelude::*;

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

        let mut builder = FunctionBuilder::new(&mut ctx.func, f_ctx);
        for (idx, ty) in fun.regs.iter().enumerate() {
            if !code[*ty].is_void() {
                builder.declare_var(Variable::new(idx), code[*ty].cranelift_type());
            }
        }

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        for (idx, arg) in function_signature.params.iter().enumerate() {
            let value = builder.append_block_param(entry_block, arg.value_type);
            builder.def_var(Variable::new(idx), value);
        }

        let mut blocks = BTreeMap::new();
        blocks.insert(0, entry_block);

        EmitCtx {
            m,
            code,
            idxs,
            fun,
            builder,
            blocks,
            pos: 0,
            field_offsets: BTreeMap::new(),
        }
    }

    fn lookup_field_offset(&mut self, ty: TypeIdx, field_idx: usize) -> Option<u32> {
        if let Some(offsets) = self.field_offsets.get(&ty) {
            return Some(offsets[field_idx]);
        }

        let mut offsets = Vec::<u32>::new();

        fn fill_offsets(code: &Code, ty: TypeIdx, offsets: &mut Vec<u32>) -> (usize, u32) {
            let o = code[ty].type_obj().unwrap();
            let (nfields, super_size) = if let Some(ty) = o.super_ {
                fill_offsets(code, ty, offsets)
            } else {
                (
                    0,
                    if matches!(code[ty], HLType::Struct(_)) {
                        8
                    } else {
                        0
                    },
                )
            };

            let mut size = super_size;
            for (i, (str_idx, type_idx)) in o.fields.iter().enumerate() {
                let byte_size = code[*type_idx].cranelift_type().bytes();
                if (size % byte_size) != 0 {
                    size += (size % byte_size);
                }
                offsets.push(byte_size);
                size += byte_size;
            }

            (nfields + o.fields.len(), size)
        }

        let (nfields, size) = fill_offsets(self.code, ty, &mut offsets);
        let offset = offsets[field_idx];
        self.field_offsets.insert(ty, offsets);
        Some(offset)
    }

    pub fn ensure_block(&mut self, pos: usize) -> Block {
        *self
            .blocks
            .entry(pos)
            .or_insert_with(|| self.builder.create_block())
    }

    pub fn reg_type(&self, reg: &Reg) -> &HLType {
        &self.code[self.fun[*reg]]
    }

    pub fn translate_body(&mut self) {
        for (pos, op) in self.fun.opcodes.iter().enumerate() {
            self.pos = pos;
            if let Some(block) = self.blocks.get(&pos).map(|b| *b) {
                if let Some(current_block) = self.current_block() {
                    if block != current_block {
                        self.ins().jump(block, &[]);
                        self.switch_to_block(block);
                    }
                } else {
                    unreachable!()
                }
            }
            match op {
                OpCode::Mov { dst, src } => {
                    let val = self.use_var(src.var());
                    self.def_var(dst.var(), val)
                }
                OpCode::Int { dst, idx } => {
                    let val = self
                        .builder
                        .ins()
                        .iconst(types::I32, self.code.ints[idx.0 as usize] as i64);
                    self.def_var(dst.var(), val)
                }
                OpCode::Float { dst, idx } => {
                    let val = self
                        .builder
                        .ins()
                        .f64const(self.code.floats[idx.0 as usize]);
                    self.def_var(dst.var(), val);
                }
                OpCode::Bool { dst, val } => {
                    let val = self.ins().iconst(types::I8, *val as i64);
                    self.def_var(dst.var(), val)
                }
                OpCode::Bytes { dst, idx } => todo!(),
                OpCode::String { dst, idx } => {
                    let gval = self
                        .m
                        .declare_data_in_func(self.idxs.ustr[idx], self.builder.func);
                    let val = self.ins().global_value(types::I64, gval);
                    self.def_var(dst.var(), val);
                }
                OpCode::Null { dst } => {
                    let val = self.ins().iconst(types::I64, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::Add { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().fadd(a, b);
                        self.def_var(dst.var(), val)
                    } else {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().iadd(a, b);
                        self.def_var(dst.var(), val)
                    }
                }
                OpCode::Sub { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().fsub(a, b);
                        self.def_var(dst.var(), val)
                    } else {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().isub(a, b);
                        self.def_var(dst.var(), val)
                    }
                }
                OpCode::Mul { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().fmul(a, b);
                        self.def_var(dst.var(), val)
                    } else {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().imul(a, b);
                        self.def_var(dst.var(), val)
                    }
                }
                OpCode::SDiv { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().fdiv(a, b);
                        self.def_var(dst.var(), val)
                    } else {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().sdiv(a, b);
                        self.def_var(dst.var(), val)
                    }
                }
                OpCode::UDiv { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().udiv(a, b);
                    self.def_var(dst.var(), val)
                }
                OpCode::SMod { dst, a, b } => {
                    if self.reg_type(dst).is_float() {
                        // TODO: handle importing of fmod somewhere else
                        let name = match self.reg_type(dst) {
                            HLType::Float32 => "fmodf",
                            HLType::Float32 => "fmod",
                            _ => unreachable!(),
                        };
                        let id = self.idxs.native_calls[name];
                        let f = self.m.declare_func_in_func(id, self.builder.func);
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let i = self.ins().call(f, &[a, b]);
                        self.builder
                            .def_var(dst.var(), self.builder.inst_results(i)[0]);
                    } else {
                        let a = self.use_var(a.var());
                        let b = self.use_var(b.var());
                        let val = self.ins().srem(a, b);
                        self.def_var(dst.var(), val);
                    }
                }
                OpCode::UMod { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().urem(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::Shl { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().ishl(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::SShr { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().sshr(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::UShr { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().ushr(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::And { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().band(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::Or { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().bor(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::Xor { dst, a, b } => {
                    let a = self.use_var(a.var());
                    let b = self.use_var(b.var());
                    let val = self.ins().bxor(a, b);
                    self.def_var(dst.var(), val);
                }
                OpCode::Neg { dst, val } => {
                    let ty = self.reg_type(val);
                    if ty.is_float() {
                        let val = self.use_var(val.var());
                        let val = self.ins().fneg(val);
                        self.def_var(dst.var(), val);
                    } else {
                        let val = self.use_var(val.var());
                        let val = self.ins().ineg(val);
                        self.def_var(dst.var(), val);
                    }
                }
                OpCode::Not { dst, val } => {
                    let val = self.use_var(val.var());
                    let val = self.ins().bnot(val);
                    self.def_var(dst.var(), val);
                }
                OpCode::Incr { dst } => {
                    let val = self.use_var(dst.var());
                    let ty = self.reg_type(dst).cranelift_type();
                    let one = self.ins().iconst(ty, 1i64);
                    let new_val = self.ins().iadd(val, one);
                    self.def_var(dst.var(), new_val);
                }
                OpCode::Decr { dst } => {
                    let val = self.use_var(dst.var());
                    let ty = self.reg_type(dst).cranelift_type();
                    let one = self.ins().iconst(ty, 1i64);
                    let new_val = self.ins().iadd(val, one);
                    self.def_var(dst.var(), new_val);
                }
                OpCode::Call0 { dst, f } => {
                    let f_ref = self
                        .m
                        .declare_func_in_func(self.idxs.fn_map[f], self.builder.func);
                    let i = self.ins().call(f_ref, &[]);
                    let val = self.inst_results(i)[0];
                    self.def_var(dst.var(), val);
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
                        .map(|r| self.use_var(r.var()))
                        .collect::<Vec<Value>>();
                    let i = self.ins().call(f_ref, args);
                    self.builder
                        .def_var(dst.var(), self.builder.inst_results(i)[0]);
                }

                OpCode::CallMethod { dst, fid, args } => match self.reg_type(&args[0]) {
                    HLType::Object(obj) => {
                        let vargs: Vec<Value> =
                            args.iter().map(|r| self.use_var(r.var())).collect();
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
                        let sig = self.m.make_signature();
                        todo!(); // super::fill_signature(code, &mut sig, ty);
                        let sig_ref = self.import_signature(sig);
                        self.ins().call_indirect(sig_ref, fun_ptr, &vargs);
                    }
                    HLType::Virtual(virt) => {
                        todo!()
                    }
                    _ => unimplemented!("OCallMethod only works with HObj or HVirt"),
                },
                OpCode::CallThis { dst, fid, args } => todo!(),
                OpCode::CallClosure { dst, closure, args } => todo!(),
                OpCode::StaticClosure { dst, fid } => todo!(),
                OpCode::InstanceClosure { dst, obj, idx } => todo!(),
                OpCode::VirtualClosure { dst, obj, idx } => todo!(),
                OpCode::GetGlobal { dst, idx } => {
                    let global_value = self
                        .m
                        .declare_data_in_func(self.idxs.globals[idx], self.builder.func);
                    let val = self.ins().symbol_value(types::I64, global_value);
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), val, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::SetGlobal { idx, val } => todo!(),
                OpCode::Field { dst, obj, fid } => {
                    match &self.code[self.fun[*obj]] {
                        HLType::Struct(_) | HLType::Object(_) => {
                            let offset = self
                                .lookup_field_offset(self.fun[*obj], fid.0 as usize)
                                .unwrap();
                            let obj = self.use_var(obj.var());
                            let ty = self.reg_type(dst).cranelift_type();
                            let val = self.ins().load(ty, MemFlags::new(), obj, offset as i32);
                            self.def_var(dst.var(), val);
                        }
                        HLType::Virtual(virt) => {
                            //  #define hl_vfields(v) ((void**)(((vvirtual*)(v))+1))
                            //  if( hl_vfields(obj)[fid] )
                            //      *hl_vfields(obj)[fid] = val;
                            //  else
                            //      hl_dyn_set(obj,hash(field),vt,val)

                            let obj_val = self.use_var(obj.var());
                            let field_addr = self.ins().iadd_imm(
                                obj_val,
                                size_of::<vvirtual>() as i64
                                    + (fid.0 as usize * size_of::<usize>()) as i64,
                            );

                            let next_block = self.next_block();
                            self.emit_brif(
                                field_addr,
                                |this| {
                                    let ty = this.reg_type(dst).cranelift_type();
                                    let val = this.ins().load(ty, MemFlags::new(), obj_val, 0);
                                    this.def_var(dst.var(), val);
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
                        let val = self.use_var(val.var());
                        let obj = self.use_var(obj.var());
                        self.ins().store(MemFlags::new(), val, obj, offset as i32);
                    }
                    HLType::Virtual(virt) => {
                        //  #define hl_vfields(v) ((void**)(((vvirtual*)(v))+1))
                        //  if( hl_vfields(obj)[fid] )
                        //      *hl_vfields(obj)[fid] = val;
                        //  else
                        //      hl_dyn_set(obj,hash(field),vt,val)

                        let obj_val = self.use_var(obj.var());
                        let val_val = self.use_var(val.var());
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
                    let obj = self.use_var(Reg(0).var());
                    let ty = self.code[self.fun[*dst]].cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), obj, offset as i32);
                    self.def_var(dst.var(), val);
                }
                OpCode::SetThis { fid, val } => {
                    let offset = self
                        .lookup_field_offset(self.fun[Reg(0)], fid.0 as usize)
                        .unwrap();
                    let val = self.use_var(val.var());
                    let obj = self.use_var(Reg(0).var());
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
                    let val = self.use_var(val.var());
                    self.ins()
                        .brif(val, block_then_label, &[], block_else_label, &[]);
                    self.switch_to_block(block_else_label);
                }
                OpCode::JFalse { val, offset } | OpCode::JNull { val, offset } => {
                    let block_else_label = self.block_for_offset(offset);
                    let block_then_label = self.next_block();
                    let val = self.use_var(val.var());
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
                OpCode::ToDyn { dst, val } => todo!(),
                OpCode::ToSFloat { dst, val } => {
                    let val = self.use_var(val.var());
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.ins().fcvt_from_sint(ty, val);
                    self.def_var(dst.var(), val)
                }
                OpCode::ToUFloat { dst, val } => {
                    let val = self.use_var(val.var());
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.builder.ins().fcvt_from_uint(ty, val);
                    self.def_var(dst.var(), val)
                }
                OpCode::ToInt { dst, val } => {
                    let val = self.use_var(val.var());
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.builder.ins().fcvt_to_sint_sat(ty, val);
                    self.def_var(dst.var(), val)
                }
                OpCode::SafeCast { dst, val } => todo!(),
                OpCode::UnsafeCast { dst, val } => todo!(),
                OpCode::ToVirtual { dst, val } => todo!(),
                OpCode::Label => {
                    let next_block = self.ensure_block(pos);
                    self.ins().jump(next_block, &[]);
                    self.switch_to_block(next_block);
                }
                OpCode::Ret(reg) => {
                    if self.reg_type(reg).is_void() {
                        self.ins().return_(&[]);
                    } else {
                        let val = self.use_var(reg.var());
                        self.ins().return_(&[val]);
                    }
                    let next_block = self.next_block();
                    self.switch_to_block(next_block);
                }
                OpCode::Throw(reg) => todo!(),
                OpCode::Rethrow(reg) => todo!(),
                OpCode::Switch { val, cases, end } => todo!(),
                OpCode::NullCheck(reg) => todo!(),
                OpCode::Trap { dst, jump_off } => (), // TODO
                OpCode::EndTrap { something } => (),  // TODO
                OpCode::GetI8 { dst, mem, offset } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.ins().uload8(types::I32, MemFlags::new(), ptr, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::GetI16 { dst, mem, offset } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.ins().uload8(types::I32, MemFlags::new(), ptr, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::GetMem { dst, mem, offset } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let ty = self.reg_type(dst).cranelift_type();
                    let val = self.ins().load(ty, MemFlags::new(), ptr, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::GetArray { dst, mem, offset } => {
                    let ty = self.reg_type(dst).cranelift_type();

                    let arr_addr = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let mem_addr = self.ins().iadd_imm(arr_addr, size_of::<varray>() as i64);
                    let offset = self.ins().imul_imm(offset, ty.bytes() as i64);
                    let val_addr = self.ins().iadd(mem_addr, offset);
                    let val = self.ins().load(ty, MemFlags::trusted(), val_addr, 0);
                    self.def_var(dst.var(), val);
                }
                OpCode::SetI8 { mem, offset, val } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.use_var(val.var());
                    self.ins().istore8(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetI16 { mem, offset, val } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.use_var(val.var());
                    self.ins().istore16(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetMem { mem, offset, val } => {
                    let mem = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let ptr = self.ins().iadd(mem, offset);
                    let val = self.use_var(val.var());
                    self.ins().store(MemFlags::new(), val, ptr, 0);
                }
                OpCode::SetArray { mem, offset, val } => {
                    let ty = self.reg_type(val).cranelift_type();

                    let arr_addr = self.use_var(mem.var());
                    let offset = self.use_var(offset.var());
                    let mem_addr = self.ins().iadd_imm(arr_addr, size_of::<varray>() as i64);
                    let offset = self.ins().imul_imm(offset, ty.bytes() as i64);
                    let val_addr = self.ins().iadd(mem_addr, offset);
                    let val = self.use_var(val.var());
                    self.ins().store( MemFlags::trusted(), val, val_addr, 0);
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
                        self.builder
                            .def_var(dst.var(), self.builder.inst_results(inst)[0]);
                    }
                    HLType::Dynobj => {
                        let func_ref = self.m.declare_func_in_func(
                            self.idxs.native_calls["hl_alloc_dynobj"],
                            self.builder.func,
                        );
                        let inst = self.ins().call(func_ref, &[]);
                        self.builder
                            .def_var(dst.var(), self.builder.inst_results(inst)[0]);
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
                        self.builder
                            .def_var(dst.var(), self.builder.inst_results(inst)[0]);
                    }
                    _ => panic!("invalid ONew"),
                },
                OpCode::ArraySize { dst, arr } => {
                    let arr = self.use_var(arr.var());
                    let val = self.ins().load(types::I32, MemFlags::trusted(), arr, offset_of!(varray, size) as i32);
                    self.def_var(dst.var(), val);
                },
                OpCode::Type { dst, idx } => todo!(),
                OpCode::GetType { dst, val } => todo!(),
                OpCode::GetTid { dst, val } => todo!(),
                OpCode::Ref { dst, val } => todo!(),
                OpCode::Unref { dst, r } => todo!(),
                OpCode::Setref { r, val } => todo!(),
                OpCode::MakeEnum {
                    dst,
                    construct_idx,
                    params,
                } => todo!(),
                OpCode::EnumAlloc { dst, idx } => todo!(),
                OpCode::EnumIndex { dst, val } => todo!(),
                OpCode::EnumField {
                    dst,
                    obj,
                    construct_idx,
                    field_idx,
                } => todo!(),
                OpCode::SetEnumField {
                    obj,
                    field_idx,
                    val,
                } => todo!(),
                OpCode::Assert => todo!(),
                OpCode::RefData { dst, r } => todo!(),
                OpCode::RefOffset { dst, r, off } => todo!(),
                OpCode::Nop => (),
                OpCode::Prefetch { args } => panic!("unsupported instruction: OPrefetch"),
                OpCode::Asm { args } => panic!("unsupported instruction: OAsm"),
            };
        }
    }

    fn emit_dyn_get(&mut self, dst: &Reg, obj: &Reg, field_name: UStrIdx) {
        let obj_val = self.use_var(obj.var());
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
        let virt_ty = self
            .m
            .declare_data_in_func(self.idxs.types[&self.fun[*obj]], self.builder.func);
        let virt_ty_val = self.ins().global_value(types::I64, virt_ty);
        let inst = match &self.code[self.fun[*dst]] {
            HLType::Float32 | HLType::Float64 | HLType::Int64 => {
                self.ins().call(dyn_get_ref, &[obj_val, hashed_field])
            }
            _ => self
                .ins()
                .call(dyn_get_ref, &[obj_val, hashed_field, virt_ty_val]),
        };
        let dst_val = self.inst_results(inst)[0];
        self.def_var(dst.var(), dst_val);
    }

    fn emit_dyn_set(&mut self, obj: &Reg, field_name: UStrIdx, val: &Reg) {
        let obj_val = self.use_var(obj.var());
        let val_val = self.use_var(val.var());
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
        let virt_ty = self
            .m
            .declare_data_in_func(self.idxs.types[&self.fun[*obj]], self.builder.func);
        let virt_ty_val = self.ins().global_value(types::I64, virt_ty);
        match &self.code[self.fun[*val]] {
            HLType::Float32 | HLType::Float64 | HLType::Int64 => self
                .ins()
                .call(dyn_set_ref, &[obj_val, hashed_field, val_val]),
            _ => self
                .ins()
                .call(dyn_set_ref, &[obj_val, hashed_field, virt_ty_val, val_val]),
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
            let a = self.use_var(a.var());
            let b = self.use_var(b.var());
            self.ins().fcmp(float_cc, a, b)
        } else {
            assert!(!b_ty.is_float());
            let a = self.use_var(a.var());
            let b = self.use_var(b.var());
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
