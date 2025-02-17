use std::error::Error;
use code::Reader;

mod code;
mod jit;
mod opcode;
mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(improper_ctypes, reason = "triggered by bindgen generated u128")]

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

fn main() -> Result<(), Box<dyn Error>> {
    let reader = Reader::open_file("out.hl")?;
    let code = crate::code::read_code(reader)?;
    Ok(())
}
