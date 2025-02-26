#![allow(unused, dead_code)]
use code::Reader;
use cranelift::jit::{JITBuilder, JITModule};
use std::error::Error;

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

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args();
    let file = args.nth(1).unwrap();
    let reader = Reader::open_file(file)?;
    let code = crate::code::read_code(reader)?;

    let product = crate::object::compile_module(code);
    let bytes = product.emit()?;
    std::fs::write("foo.o", bytes);
    // let (m, entrypoint) = crate::jit::compile_module(code);
    // unsafe {
    //     let entrypoint: extern "C" fn() =
    //         core::mem::transmute(m.get_finalized_function(entrypoint));
    //     entrypoint();
    // }
    Ok(())
}
