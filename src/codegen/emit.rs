use std::{collections::BTreeMap, mem::offset_of};

use cranelift::module::Module;
use cranelift::prelude::*;

use crate::{
    code::{Code, HLFunction, HLType},
    opcode::OpCode,
    sys::hl_type,
};

use super::Indexes;

pub fn translate_function(m: &mut dyn Module, code: &Code, idxs: &Indexes, fun: &HLFunction) {
    let mut ctx = m.make_context();
    let function_signature = m
        .declarations()
        .get_function_decl(idxs.fn_map[&fun.idx])
        .signature
        .clone();
    ctx.func.signature = function_signature.clone();

    let mut fctx_builder = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fctx_builder);

    for (idx, ty) in fun.regs.iter().enumerate() {
        builder.declare_var(Variable::new(idx), code[*ty].cranelift_type());
    }

    let entry_block = builder.create_block();
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    for (idx, arg) in function_signature.params.iter().enumerate() {
        let value = builder.append_block_param(entry_block, arg.value_type);
        builder.def_var(Variable::new(idx), value);
    }

    let mut blocks = BTreeMap::<usize, Block>::new();

    for (pos, op) in fun.opcodes.iter().enumerate() {
        if let Some(block) = blocks.get(&pos).map(|b| *b) {
            if let Some(current_block) = builder.current_block() {
                if block != current_block {
                    builder.ins().jump(block, &[]);
                    builder.switch_to_block(block);
                }
            } else {
                unreachable!()
            }
        }
        match op {
            OpCode::Mov { dst, src } => {
                let val = builder.use_var(src.var());
                builder.def_var(dst.var(), val)
            }
            OpCode::Int { dst, idx } => {
                let val = builder
                    .ins()
                    .iconst(types::I32, code.ints[idx.0 as usize] as i64);
                builder.def_var(dst.var(), val)
            }
            OpCode::Float { dst, idx } => {
                let val = builder.ins().f64const(code.floats[idx.0 as usize]);
                builder.def_var(dst.var(), val);
            }
            OpCode::Bool { dst, val } => {
                let val = builder.ins().iconst(types::I8, *val as i64);
                builder.def_var(dst.var(), val)
            }
            OpCode::Bytes { dst, idx } => todo!(),
            OpCode::String { dst, idx } => {
                let gval = m.declare_data_in_func(idxs.ustr[idx], builder.func);
                let val = builder.ins().global_value(types::I64, gval);
                builder.def_var(dst.var(), val);
            }
            OpCode::Null { dst } => {
                let val = builder.ins().iconst(types::I64, 0);
                builder.def_var(dst.var(), val);
            }
            OpCode::Add { dst, a, b } => {
                if code[fun[*dst]].is_float() {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().fadd(a, b);
                    builder.def_var(dst.var(), val)
                } else {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().iadd(a, b);
                    builder.def_var(dst.var(), val)
                }
            }
            OpCode::Sub { dst, a, b } => {
                if code[fun[*dst]].is_float() {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().fsub(a, b);
                    builder.def_var(dst.var(), val)
                } else {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().isub(a, b);
                    builder.def_var(dst.var(), val)
                }
            }
            OpCode::Mul { dst, a, b } => {
                if code[fun[*dst]].is_float() {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().fmul(a, b);
                    builder.def_var(dst.var(), val)
                } else {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().imul(a, b);
                    builder.def_var(dst.var(), val)
                }
            }
            OpCode::SDiv { dst, a, b } => {
                if code[fun[*dst]].is_float() {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().fdiv(a, b);
                    builder.def_var(dst.var(), val)
                } else {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().sdiv(a, b);
                    builder.def_var(dst.var(), val)
                }
            }
            OpCode::UDiv { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().udiv(a, b);
                builder.def_var(dst.var(), val)
            }
            OpCode::SMod { dst, a, b } => {
                if code[fun[*dst]].is_float() {
                    // TODO: handle importing of fmod somewhere else
                    let (name, t) = match code[fun[*dst]] {
                        HLType::Float32 => ("fmodf", types::F32),
                        HLType::Float32 => ("fmod", types::F64),
                        _ => unreachable!(),
                    };
                    let mut sig = m.make_signature();
                    sig.params.push(AbiParam::new(t));
                    sig.params.push(AbiParam::new(t));
                    sig.returns.push(AbiParam::new(t));
                    let id = m
                        .declare_function(name, cranelift::module::Linkage::Import, &sig)
                        .unwrap();
                    let f = m.declare_func_in_func(id, builder.func);
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let i = builder.ins().call(f, &[a, b]);
                    builder.def_var(dst.var(), builder.inst_results(i)[0]);
                } else {
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    let val = builder.ins().srem(a, b);
                    builder.def_var(dst.var(), val);
                }
            }
            OpCode::UMod { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().urem(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::Shl { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().ishl(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::SShr { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().sshr(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::UShr { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().ushr(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::And { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().band(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::Or { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().bor(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::Xor { dst, a, b } => {
                let a = builder.use_var(a.var());
                let b = builder.use_var(b.var());
                let val = builder.ins().bxor(a, b);
                builder.def_var(dst.var(), val);
            }
            OpCode::Neg { dst, val } => {
                let ty = &code[fun[*val]];
                if ty.is_float() {
                    let val = builder.use_var(val.var());
                    let val = builder.ins().fneg(val);
                    builder.def_var(dst.var(), val);
                } else {
                    let val = builder.use_var(val.var());
                    let val = builder.ins().ineg(val);
                    builder.def_var(dst.var(), val);
                }
            }
            OpCode::Not { dst, val } => {
                let val = builder.use_var(val.var());
                let val = builder.ins().bnot(val);
                builder.def_var(dst.var(), val);
            }
            OpCode::Incr { dst } => {
                let val = builder.use_var(dst.var());
                let one = builder.ins().iconst(code[fun[*dst]].cranelift_type(), 1i64);
                let new_val = builder.ins().iadd(val, one);
                builder.def_var(dst.var(), new_val);
            }
            OpCode::Decr { dst } => {
                let val = builder.use_var(dst.var());
                let one = builder.ins().iconst(code[fun[*dst]].cranelift_type(), 1i64);
                let new_val = builder.ins().iadd(val, one);
                builder.def_var(dst.var(), new_val);
            }
            OpCode::Call0 { dst, f } => {
                let f_ref = m.declare_func_in_func(idxs.fn_map[f], builder.func);
                let i = builder.ins().call(f_ref, &[]);
                builder.def_var(dst.var(), builder.inst_results(i)[0]);
            }
            OpCode::Call1 { dst, f, args }
            | OpCode::Call2 { dst, f, args }
            | OpCode::Call3 { dst, f, args }
            | OpCode::Call4 { dst, f, args }
            | OpCode::CallN { dst, f, args } => {
                let f_ref = m.declare_func_in_func(idxs.fn_map[f], builder.func);
                let args = &args
                    .iter()
                    .map(|r| builder.use_var(r.var()))
                    .collect::<Vec<Value>>();
                let i = builder.ins().call(f_ref, args);
                builder.def_var(dst.var(), builder.inst_results(i)[0]);
            }

            OpCode::CallMethod { dst, fid, args } => match &code[fun[args[0]]] {
                HLType::Object(obj) => {
                    let vargs: Vec<Value> = args.iter().map(|r| builder.use_var(r.var())).collect();
                    let ty_val = builder.ins().load(types::I64, MemFlags::new(), vargs[0], 0);
                    let proto_val = builder.ins().load(
                        types::I64,
                        MemFlags::new(),
                        ty_val,
                        offset_of!(hl_type, vobj_proto) as i32,
                    );
                    let fun_ptr = builder.ins().load(
                        types::I64,
                        MemFlags::new(),
                        proto_val,
                        (fid.0 as usize * size_of::<*mut u8>()) as i32,
                    );
                    let sig = m.make_signature();
                    todo!(); // super::fill_signature(code, &mut sig, ty);
                    let sig_ref = builder.import_signature(sig);
                    builder.ins().call_indirect(sig_ref, fun_ptr, &vargs);
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
                let global_value = m.declare_data_in_func(idxs.globals[idx], builder.func);
                let val = builder.ins().symbol_value(types::I64, global_value);
                let val =
                    builder
                        .ins()
                        .load(code[fun[*dst]].cranelift_type(), MemFlags::new(), val, 0);
                builder.def_var(dst.var(), val);
            }
            OpCode::SetGlobal { idx, val } => todo!(),
            OpCode::Field { dst, obj, fid } => todo!(),
            OpCode::SetField { obj, fid, val } => todo!(),
            OpCode::GetThis { dst, fid } => todo!(),
            OpCode::SetThis { fid, val } => todo!(),
            OpCode::DynGet {
                dst,
                obj,
                hashed_name,
            } => todo!(),
            OpCode::DynSet {
                obj,
                hashed_name,
                val,
            } => todo!(),
            OpCode::JTrue { val, offset } | OpCode::JNotNull { val, offset } => {
                let p = (((pos + 1) as isize) + offset.0) as usize;
                let block_then_label = *blocks.entry(p).or_insert_with(|| builder.create_block());
                let block_else_label = *blocks
                    .entry(pos + 1)
                    .or_insert_with(|| builder.create_block());
                let val = builder.use_var(val.var());
                builder
                    .ins()
                    .brif(val, block_then_label, &[], block_else_label, &[]);
                builder.switch_to_block(block_else_label);
            }
            OpCode::JFalse { val, offset } | OpCode::JNull { val, offset } => {
                let p = (((pos + 1) as isize) + offset.0) as usize;
                let block_else_label = *blocks.entry(p).or_insert_with(|| builder.create_block());
                let block_then_label = *blocks
                    .entry(pos + 1)
                    .or_insert_with(|| builder.create_block());
                let val = builder.use_var(val.var());
                builder
                    .ins()
                    .brif(val, block_then_label, &[], block_else_label, &[]);
                builder.switch_to_block(block_then_label);
            }
            OpCode::JSLt { a, b, offset } => {
                let a_ty = &code[fun[*a]];
                let b_ty = &code[fun[*b]];
                let p = (((pos + 1) as isize) + offset.0) as usize;
                let block_then_label = *blocks.entry(p).or_insert_with(|| builder.create_block());
                let block_else_label = *blocks
                    .entry(pos + 1)
                    .or_insert_with(|| builder.create_block());
                let val = if a_ty.is_float() {
                    assert!(b_ty.is_float());
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    builder.ins().fcmp(FloatCC::LessThan, a, b)
                } else {
                    assert!(!b_ty.is_float());
                    let a = builder.use_var(a.var());
                    let b = builder.use_var(b.var());
                    builder.ins().icmp(IntCC::SignedLessThan, a, b)
                };
                builder
                    .ins()
                    .brif(val, block_then_label, &[], block_else_label, &[]);
                builder.switch_to_block(block_else_label);
            }
            OpCode::JSGte { a, b, offset } => todo!(),
            OpCode::JSGt { a, b, offset } => todo!(),
            OpCode::JSLte { a, b, offset } => todo!(),
            OpCode::JULt { a, b, offset } => todo!(),
            OpCode::JUGte { a, b, offset } => todo!(),
            OpCode::JNotLt { a, b, offset } => todo!(),
            OpCode::JNotGte { a, b, offset } => todo!(),
            OpCode::JEq { a, b, offset } => todo!(),
            OpCode::JNotEq { a, b, offset } => todo!(),
            OpCode::JAlways { offset } => {
                let block = blocks[&(((pos + 1) as isize + offset.0) as usize)];
                builder.ins().jump(block, &[]);
            }
            OpCode::ToDyn { dst, val } => todo!(),
            OpCode::ToSFloat { dst, val } => todo!(),
            OpCode::ToUFloat { dst, val } => todo!(),
            OpCode::ToInt { dst, val } => todo!(),
            OpCode::SafeCast { dst, val } => todo!(),
            OpCode::UnsafeCast { dst, val } => todo!(),
            OpCode::ToVirtual { dst, val } => todo!(),
            OpCode::Label => {
                let next_block = *blocks.entry(pos).or_insert_with(|| builder.create_block());
                builder.ins().jump(next_block, &[]);
                builder.switch_to_block(next_block);
            }
            OpCode::Ret(reg) => {
                let val = builder.use_var(reg.var());
                builder.ins().return_(&[val]);
                let next_block = *blocks
                    .entry(pos + 1)
                    .or_insert_with(|| builder.create_block());
                builder.switch_to_block(next_block);
            }
            OpCode::Throw(reg) => todo!(),
            OpCode::Rethrow(reg) => todo!(),
            OpCode::Switch { val, cases, end } => todo!(),
            OpCode::NullCheck(reg) => todo!(),
            OpCode::Trap { dst, jump_off } => todo!(),
            OpCode::EndTrap { something } => todo!(),
            OpCode::GetI8 { dst, mem, val } => todo!(),
            OpCode::GetI16 { dst, mem, val } => todo!(),
            OpCode::GetMem { dst, mem, val } => todo!(),
            OpCode::GetArray { dst, mem, val } => todo!(),
            OpCode::SetI8 { mem, offset, val } => todo!(),
            OpCode::SetI16 { mem, offset, val } => todo!(),
            OpCode::SetMem { mem, offset, val } => todo!(),
            OpCode::SetArray { mem, offset, val } => todo!(),
            OpCode::New { dst } => todo!(),
            OpCode::ArraySize { dst, arr } => todo!(),
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

    builder.finalize();
}
