use std::collections::HashMap;

pub use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::FuncId;
use libloading::Library;

use crate::codegen::CodegenCtx;

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let mut jit_b = JITBuilder::with_flags(
        &[
            ("regalloc_algorithm", "backtracking"),
            ("enable_verifier", "false"),
            ("opt_level", "speed"),
        ],
        cranelift::module::default_libcall_names(),
    )
    .unwrap();
    let mut libs: HashMap<String, &mut Library> = HashMap::new();
    for native in code.natives() {
        let dll_name = native.dll_name();
        let symbol_name = native.symbol_name();
        let symbol = unsafe {
            libs.entry(dll_name)
                .or_insert_with_key(|key| {
                    Box::leak(Box::new(libloading::Library::new(key).unwrap()))
                })
                .get::<*mut u8>(symbol_name.as_bytes())
                .unwrap()
                .try_as_raw_ptr()
                .unwrap()
        };
        jit_b.symbol(symbol_name, symbol.cast());
    }
    #[cfg(windows)]
    {
        use std::os::windows::io::AsRawHandle;
        use std::os::windows::io::FromRawHandle;
        use windows_sys::Win32::Foundation::HMODULE;
        use windows_sys::Win32::System::LibraryLoader;
        let handle = unsafe {
            std::os::windows::io::OwnedHandle::from_raw_handle(LibraryLoader::GetModuleHandleA(
                "libhl.dll".as_ptr(),
            ))
        };
        jit_b.symbol_lookup_fn(Box::new(move |name| {
            let c_str = std::ffi::CString::new(name).unwrap();
            let c_str_ptr = c_str.as_ptr();
            unsafe {
                LibraryLoader::GetProcAddress(handle.as_raw_handle(), c_str_ptr.cast())
                    .map(|val| val as *const u8)
            }
        }));
    }
    let jit_m = JITModule::new(jit_b);
    let mut jit_m = crate::unwind::UnwindModule::new(jit_m, false);
    let mut ctx = CodegenCtx::new(&mut jit_m);
    let entrypoint = ctx.compile(&code, false);
    jit_m.finalize_definitions();
    (jit_m.module, entrypoint)
}
