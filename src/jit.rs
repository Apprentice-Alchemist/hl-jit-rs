use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{FuncId, Linkage, Module},
};
use libloading::Library;

use crate::{codegen::CodegenCtx, sys::hl_type};

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let mut jit_b = JITBuilder::with_flags(
        &[
            // This speeds up compilation by a lot
            ("regalloc_algorithm", "single_pass"),
        ],
        cranelift::module::default_libcall_names(),
    )
    .unwrap();
    let mut libs: HashMap<String, &mut Library> = HashMap::new();
    for (lib, name, _, _) in &code.natives {
        let (libname, libfile, optional) = match &code[*lib] {
            "std" => ("hl", "/usr/local/lib/libhl.so".to_string(), false),
            "?std" => ("hl", "/usr/local/lib/libhl.so".to_string(), true),
            "builtin" => continue,
            val => {
                let optional = val.starts_with('?');
                let val = if optional { &val[1..] } else { val };
                (val, format!("/usr/local/lib/{}.hdll", val), optional)
            }
        };
        let name = &code[*name];
        let symbol_name = format!("{libname}_{name}");
        let symbol = unsafe {
            libs.entry(libfile)
                .or_insert_with_key(|key| unsafe {
                    Box::leak(Box::new(libloading::Library::new(key).unwrap()))
                })
                .get::<*mut u8>(symbol_name.as_bytes())
                .unwrap()
                .try_as_raw_ptr()
                .unwrap()
        };
        jit_b.symbol(symbol_name, symbol.cast());
    }

    let mut jit_m = JITModule::new(jit_b);

    let mut ctx = CodegenCtx::new(&mut jit_m);
    let entrypoint = ctx.compile(code);
    jit_m.finalize_definitions();
    (jit_m, entrypoint)
}
