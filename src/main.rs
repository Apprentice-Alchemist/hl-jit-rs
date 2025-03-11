#![allow(unused, dead_code)]
use clap::Parser;
use code::Reader;
use cranelift::jit::{JITBuilder, JITModule};
use std::{
    error::Error,
    ffi::{CStr, CString, c_int, c_void},
    process::abort,
    ptr::{null, null_mut},
    str::FromStr,
};
use sys::{hl_type, hl_type__bindgen_ty_1, hl_type_fun, hl_type_kind_HFUN, vclosure, vdynamic};

mod code;
mod codegen;
mod jit;
mod object;
mod opcode;
mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(improper_ctypes, reason = "triggered by bindgen generated u128")]
    #![allow(dead_code)]
    #![allow(unsafe_op_in_unsafe_fn)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

    impl varray {
        /// # Safety
        /// The type parameter `T` needs to be correct
        pub unsafe fn as_slice<T>(&self) -> &[T] {
            unsafe {
                core::slice::from_raw_parts(core::ptr::from_ref(self).offset(1).cast(), self.size as usize)
            }
        }
    }
}

/// Hashlink JIT compiler
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Compile to object file
    #[arg(short, long)]
    output: Option<String>,

    /// File containing HL bytecode
    file: String,

    /// Program arguments
    args: Vec<String>,
}

extern "C" fn resolve_symbol(addr: *mut c_void, out: *mut u16, out_size: *mut c_int) -> *mut u16 {
    backtrace::resolve(addr, |sym| {
        if let Some(name) = sym.name() {
            let name = format!("{}", name);
            let mut pos = 0;
            for c in name.encode_utf16() {
                if pos >= unsafe { *out_size } as usize {
                    break;
                }
                unsafe { out.add(pos).write(c) };
                pos += 1;
            }
            unsafe {
                unsafe { out.add(pos).write(0) };
                *out_size = pos as c_int;
            }
        }
    });
    out
}
extern "C" fn capture_stack(stack: *mut *mut c_void, size: c_int) -> c_int {
    let mut pos = 0;
    backtrace::trace(|frame| {
        unsafe {
            stack.add(pos as usize).write(frame.ip());
        }
        pos += 1;
        pos < size
    });
    pos
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = Args::parse();
    let reader = Reader::open_file(&args.file)?;
    let code = crate::code::read_code(reader)?;

    if let Some(ref output) = args.output {
        let product = crate::object::compile_module(code);
        let bytes = product.emit()?;
        std::fs::write(output, bytes);
    } else {
        let (m, entrypoint) = crate::jit::compile_module(code);
        unsafe extern "C" {
            unsafe fn ffi_static_call(
                fun: *mut c_void,
                ft: *mut hl_type,
                args: *mut *mut c_void,
                out: *mut vdynamic,
            ) -> *mut c_void;
            unsafe fn ffi_get_wrapper(ty: *mut hl_type) -> *mut c_void;
        }
        unsafe {
            sys::hl_global_init();
            sys::hl_setup_callbacks(
                ffi_static_call as *mut c_void,
                ffi_get_wrapper as *mut c_void,
            );
            sys::hl_setup_exception(resolve_symbol as *mut c_void, capture_stack as *mut c_void);
            sys::hl_register_thread(core::ptr::from_mut(&mut args).cast());
            let mut args: Vec<&mut CStr> = args
                .args
                .iter()
                .map(|s| Box::leak(CString::from_str(&s).unwrap().into_boxed_c_str()))
                .collect();
            sys::hl_sys_init(args.as_mut_ptr().cast(), args.len() as i32, null_mut());
            let mut is_exception = false;

            let __bindgen_anon_1 = hl_type__bindgen_ty_1 {
                fun: &mut hl_type_fun {
                    args: null_mut(),
                    ret: &raw mut sys::hlt_void,
                    nargs: 0,
                    parent: null_mut(),
                    closure_type: core::mem::zeroed(),
                    closure: core::mem::zeroed(),
                },
            };

            let mut t = hl_type {
                kind: hl_type_kind_HFUN,
                __bindgen_anon_1,
                vobj_proto: null_mut(),
                mark_bits: null_mut(),
            };
            let mut c = vclosure {
                t: &mut t,
                fun: m
                    .get_finalized_function(entrypoint)
                    .cast::<c_void>()
                    .cast_mut(),
                hasValue: 0,
                stackCount: 0,
                value: null_mut(),
            };
            let ret = sys::hl_dyn_call_safe(&mut c, null_mut(), 0, &mut is_exception);
            if is_exception {
                let stack = sys::hl_exception_stack().as_ref().unwrap();
                eprintln!(
                    "Uncaught exception: {:#?}",
                    CStr::from_ptr(sys::hl_to_utf8(sys::hl_to_string(ret)))
                );
                for (pos, elem) in stack.as_slice::<*mut u16>().iter().enumerate() {
                    println!("  {pos}: {:#?}", CStr::from_ptr(sys::hl_to_utf8(*elem)));
                }
                std::process::exit(1);
            }
        }
    }
    Ok(())
}
