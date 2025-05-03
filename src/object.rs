use std::str::FromStr;

use cranelift::{
    module::{FuncId, default_libcall_names},
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::{
        Configurable,
        isa::{OwnedTargetIsa, lookup},
        settings::{self, Flags},
    },
};
use target_lexicon::Triple;

use crate::{codegen::CodegenCtx, unwind::UnwindModule};

pub fn compile_module(
    code: &crate::code::Code,
    name: &str,
    target: Option<String>,
) -> (ObjectProduct, OwnedTargetIsa) {
    let mut builder = settings::builder();
    builder.set("is_pic", "true");
    builder.set("regalloc_algorithm", "backtracking");
    builder.set("opt_level", "speed");
    let flags = Flags::new(builder);
    let isa_builder = if let Some(target) = target {
        cranelift::codegen::isa::lookup(Triple::from_str(&target).unwrap()).unwrap()
    } else {
        cranelift::native::builder_with_options(false).unwrap()
    };
    let isa = isa_builder.finish(flags).unwrap();
    let mod_builder = ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap();
    let mut module = UnwindModule::new(ObjectModule::new(mod_builder), true);

    let mut ctx = CodegenCtx::new(&mut module);
    let entrypoint = ctx.compile(code, true);
    (module.finish(), isa)
}
