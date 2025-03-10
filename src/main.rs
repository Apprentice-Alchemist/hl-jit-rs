#![allow(unused, dead_code)]
use clap::Parser;
use code::Reader;
use cranelift::jit::{JITBuilder, JITModule};
use std::{error::Error, ffi::{c_void, CStr, CString}, ptr::null_mut, str::FromStr};
use sys::{hl_type, vdynamic};

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
            sys::hl_register_thread(core::ptr::from_mut(&mut args).cast());
            let mut args: Vec<&mut CStr> = args.args.iter().map(|s| {
                Box::leak(CString::from_str(&s).unwrap().into_boxed_c_str())
            }).collect();
            sys::hl_sys_init(args.as_mut_ptr().cast(), args.len() as i32, null_mut());
            let entrypoint: extern "C" fn() =
                core::mem::transmute(m.get_finalized_function(entrypoint));
            entrypoint();
        }
    }
    Ok(())
}
