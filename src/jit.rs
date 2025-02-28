use std::{collections::HashMap, fmt::{Display, Write}};

use cranelift::{
    jit::{JITBuilder, JITModule},
    module::{FuncId, Linkage, Module},
};
use libloading::Library;

use crate::{codegen::CodegenCtx, sys::hl_type};

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let mut jit_b = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let mut libs: HashMap<String, &mut Library> = HashMap::new();
    for (lib, name, _, _) in &code.natives {
        let (libname, libfile) = match &code[*lib] {
            "std\0" => ("hl", "/usr/local/lib/libhl.so".to_string()),
            "?std\0" => ("hl", "/usr/local/lib/libhl.so".to_string()),
            "builtin\0" => continue,
            val => (
                val,
                format!("/usr/local/lib/{}.hdll", &val[0..val.len() - 1]),
            ),
        };
        let name = &code[*name];
        let name = &name[0..name.len() - 1];
        let (name, optional) = if name.starts_with('?') {
            (&name[1..], true)
        } else {
            (name, false)
        };
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
