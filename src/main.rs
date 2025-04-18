#![allow(unused, dead_code)]
use clap::Parser;
use std::{
    error::Error,
    ffi::{CStr, CString, c_int, c_void},
    io::Write,
    path::{Path, PathBuf},
    process::abort,
    ptr::{null, null_mut},
    str::FromStr,
    time::Instant,
};
use hl_sys::{
    hl_get_thread, hl_type, hl_type__bindgen_ty_1, hl_type_fun, hl_type_kind_HF32, hl_type_kind_HF64, hl_type_kind_HFUN, hlt_bytes, vclosure, vdynamic, vdynamic__bindgen_ty_1
};

pub use hl_code as code;

mod codegen;
mod jit;
mod object;
mod unwind;

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
    if stack.is_null() {
        backtrace::trace(|_| { pos += 1; true });
    } else {
        backtrace::trace(|frame| {
            unsafe {
                stack.add(pos as usize).write(frame.ip());
            }
            pos += 1;
            pos < size
        });
    }
    pos
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = Args::parse();
    let code = hl_code::Code::from_file(&args.file).unwrap();
    println!("parsing done");
    if let Some(ref output) = args.output {
        let product = crate::object::compile_module(code);
        let bytes = product.emit()?;
        std::fs::write(output, bytes);
    } else {
        let start = Instant::now();
        let (m, entrypoint) = crate::jit::compile_module(code);
        println!("compiling done in {:?}", start.elapsed());

        #[cfg(not(feature = "hl-ffi"))]
        unsafe extern "C" {
            unsafe fn hlc_static_call(
                fun: *mut c_void,
                ft: *mut hl_type,
                args: *mut *mut c_void,
                out: *mut vdynamic,
            ) -> *mut c_void;
            unsafe fn hlc_get_wrapper(ty: *mut hl_type) -> *mut c_void;
        }
        unsafe {
            hl_sys::hl_global_init();
            #[cfg(feature = "hl-ffi")]
            hl_sys::hl_setup_callbacks(
                hl_ffi::static_call as *mut c_void,
                hl_ffi::get_wrapper as *mut c_void,
            );
            #[cfg(not(feature = "hl-ffi"))]
            hl_sys::hl_setup_callbacks(
                hlc_static_call as *mut c_void,
                hlc_get_wrapper as *mut c_void,
            );
            hl_sys::hl_setup_exception(resolve_symbol as *mut c_void, capture_stack as *mut c_void);
            hl_sys::hl_register_thread(core::ptr::from_mut(&mut args).cast());
            let mut args: Vec<&mut CStr> = args
                .args
                .iter()
                .map(|s| Box::leak(CString::from_str(&s).unwrap().into_boxed_c_str()))
                .collect();
            hl_sys::hl_sys_init(args.as_mut_ptr().cast(), args.len() as i32, null_mut());
            let mut is_exception = false;

            let __bindgen_anon_1 = hl_type__bindgen_ty_1 {
                fun: &mut hl_type_fun {
                    args: null_mut(),
                    ret: &raw mut hl_sys::hlt_void,
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
            let ret = hl_sys::hl_dyn_call_safe(&mut c, null_mut(), 0, &mut is_exception);
            if is_exception {
                let stack = hl_sys::hl_exception_stack().as_ref().unwrap();
                eprintln!(
                    "Uncaught exception: {:#?}",
                    CStr::from_ptr(hl_sys::hl_to_utf8(hl_sys::hl_to_string(ret)))
                );
                for (pos, elem) in stack.as_slice::<*mut u16>().iter().enumerate() {
                    println!("  {pos}: {:#?}", CStr::from_ptr(hl_sys::hl_to_utf8(*elem)));
                }
                std::process::exit(1);
            }
        }
    }
    Ok(())
}
