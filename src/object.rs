use cranelift::{
    module::{FuncId, default_libcall_names},
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::{
        Configurable,
        isa::lookup,
        settings::{self, Flags},
    },
};

use crate::{codegen::CodegenCtx, unwind::UnwindModule};

pub fn compile_module(code: crate::code::Code, name: &str) -> ObjectProduct {
    let mut builder = settings::builder();
    builder.set("is_pic", "true");
    let flags = Flags::new(builder);
    let isa = cranelift::native::builder().unwrap().finish(flags).unwrap();
    let mod_builder = ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap();
    let mut module = UnwindModule::new(ObjectModule::new(mod_builder), true);

    let mut ctx = CodegenCtx::new(&mut module);
    let entrypoint = ctx.compile(code);
    module.finish()
}
