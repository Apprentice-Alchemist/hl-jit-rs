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
                builder.def_var(dst.var(), val);
            }

            _ => {
                builder.ins().trap(TrapCode::unwrap_user(0));
            }
        };
    }
}
