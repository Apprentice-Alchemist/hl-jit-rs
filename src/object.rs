use cranelift::{
    module::FuncId,
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::{
        isa::lookup,
        settings::{self, Flags},
    },
};

use crate::codegen::CodegenCtx;

pub fn compile_module(code: crate::code::Code) -> ObjectProduct {
    let builder = settings::builder();
    let flags = Flags::new(builder);
    let isa = cranelift::native::builder().unwrap().finish(flags).unwrap();
    let mod_builder =
        ObjectBuilder::new(isa, "foo", cranelift::module::default_libcall_names()).unwrap();
    let mut module = ObjectModule::new(mod_builder);

    let mut ctx = CodegenCtx::new(&mut module);
    let entrypoint = ctx.compile(code);
    module.finish()
}
