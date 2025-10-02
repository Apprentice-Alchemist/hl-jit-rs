use std::str::FromStr;

use cranelift::{
    module::default_libcall_names,
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::{
        Configurable,
        isa::OwnedTargetIsa,
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
    let triple = if let Some(target) = target {
        match Triple::from_str(&target) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("Invalid target triple {target}: {e}");
                std::process::exit(1)
            }
        }
    } else {
        Triple::host()
    };
    let mut builder = settings::builder();
    builder.set("is_pic", "true").unwrap();
    builder.set("opt_level", "speed").unwrap();
    builder.set("regalloc_algorithm", "backtracking").unwrap();

    let flags = Flags::new(builder);
    let isa_builder = cranelift::codegen::isa::lookup(triple).unwrap();
    let isa = isa_builder.finish(flags).unwrap();
    let mod_builder = ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap();
    let mut module = UnwindModule::new(ObjectModule::new(mod_builder), true);

    let mut ctx = CodegenCtx::new(&mut module);
    ctx.compile(code, true);
    (module.finish(), isa)
}
