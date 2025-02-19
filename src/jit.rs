use cranelift::{
    jit::{JITBuilder, JITModule},
    module::FuncId,
};

use crate::codegen::CodegenCtx;

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let jit_b = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let jit_m = JITModule::new(jit_b);

    let ctx = CodegenCtx::new(jit_m, code);
    let (mut m, entrypoint) = ctx.finish();
    m.finalize_definitions();
    (m, entrypoint)
}
