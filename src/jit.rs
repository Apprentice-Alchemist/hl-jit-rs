use cranelift::{
    jit::{JITBuilder, JITModule},
    module::FuncId,
};

use crate::codegen::CodegenCtx;

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let jit_b = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let mut jit_m = JITModule::new(jit_b);

    let mut ctx = CodegenCtx::new(&mut jit_m);
    let entrypoint = ctx.compile(code);
    jit_m.finalize_definitions();
    (jit_m, entrypoint)
}
