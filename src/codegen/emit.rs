use std::collections::BTreeMap;

use cranelift::module::Module;
use cranelift::prelude::*;

use crate::{
    code::{Code, HLFunction},
    opcode::OpCode,
};

use super::Indexes;

fn translate_function(m: &mut dyn Module, code: &Code, idxs: &Indexes, fun: &HLFunction) {
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
            OpCode::Mul { dst, a, b } => todo!(),
            OpCode::SDiv { dst, a, b } => todo!(),
            OpCode::UDiv { dst, a, b } => todo!(),
            OpCode::SMod { dst, a, b } => todo!(),
            OpCode::UMod { dst, a, b } => todo!(),
            OpCode::Shl { dst, a, b } => todo!(),
            OpCode::SShr { dst, a, b } => todo!(),
            OpCode::UShr { dst, a, b } => todo!(),
            OpCode::And { dst, a, b } => todo!(),
            OpCode::Or { dst, a, b } => todo!(),
            OpCode::Xor { dst, a, b } => todo!(),
            OpCode::Neg { dst, val } => todo!(),
            OpCode::Not { dst, val } => todo!(),
            OpCode::Incr { dst } => todo!(),
            OpCode::Decr { dst } => todo!(),
            OpCode::Call0 { dst, f } => todo!(),
            OpCode::Call1 { dst, f, args } => todo!(),
            OpCode::Call2 { dst, f, args } => todo!(),
            OpCode::Call3 { dst, f, args } => todo!(),
            OpCode::Call4 { dst, f, args } => todo!(),
            OpCode::CallN { dst, f, args } => todo!(),
            OpCode::CallMethod { dst, fid, args } => todo!(),
            OpCode::CallThis { dst, fid, args } => todo!(),
            OpCode::CallClosure { dst, closure, args } => todo!(),
            OpCode::StaticClosure { dst, fid } => todo!(),
            OpCode::InstanceClosure { dst, obj, idx } => todo!(),
            OpCode::VirtualClosure { dst, obj, idx } => todo!(),
            OpCode::GetGlobal { dst, idx } => todo!(),
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
            OpCode::JTrue { val, offset } => todo!(),
            OpCode::JFalse { val, offset } => todo!(),
            OpCode::JNull { val, offset } => todo!(),
            OpCode::JNotNull { val, offset } => todo!(),
            OpCode::JSLt { a, b, offset } => todo!(),
            OpCode::JSGte { a, b, offset } => todo!(),
            OpCode::JSGt { a, b, offset } => todo!(),
            OpCode::JSLte { a, b, offset } => todo!(),
            OpCode::JULt { a, b, offset } => todo!(),
            OpCode::JUGte { a, b, offset } => todo!(),
            OpCode::JNotLt { a, b, offset } => todo!(),
            OpCode::JNotGte { a, b, offset } => todo!(),
            OpCode::JEq { a, b, offset } => todo!(),
            OpCode::JNotEq { a, b, offset } => todo!(),
            OpCode::JAlways { offset } => todo!(),
            OpCode::ToDyn { dst, val } => todo!(),
            OpCode::ToSFloat { dst, val } => todo!(),
            OpCode::ToUFloat { dst, val } => todo!(),
            OpCode::ToInt { dst, val } => todo!(),
            OpCode::SafeCast { dst, val } => todo!(),
            OpCode::UnsafeCast { dst, val } => todo!(),
            OpCode::ToVirtual { dst, val } => todo!(),
            OpCode::Label => todo!(),
            OpCode::Ret(reg) => todo!(),
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
            OpCode::Nop => todo!(),
            OpCode::Prefetch { args } => todo!(),
            OpCode::Asm { args } => todo!(),
        };
    }
}
