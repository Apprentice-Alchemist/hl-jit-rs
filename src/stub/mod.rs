mod apple;
mod elf;
mod windows;

use std::collections::{HashMap, HashSet};

use cranelift::prelude::isa::{OwnedTargetIsa, TargetIsa};
use hl_code::Code;

use crate::codegen::LIBHL_NATIVE_CALLS;

pub fn create_stub(isa: &dyn TargetIsa, name: &str, symbols: &[String]) -> Vec<u8> {
    match isa.triple().binary_format {
        target_lexicon::BinaryFormat::Macho => apple::create_tbd_stub(isa, name, symbols),
        target_lexicon::BinaryFormat::Elf => elf::create_elf_stub(isa, name, symbols),
        target_lexicon::BinaryFormat::Coff => windows::create_import_library(isa, name, symbols),
        format => panic!("unsupported binary format {format:?}"),
    }
}

pub fn create_stubs<T>(
    code: &Code,
    isa: &dyn TargetIsa,
    write: impl Fn(&str, Vec<u8>) -> T,
) -> Vec<T> {
    let mut libs: HashMap<String, HashSet<String>> = HashMap::new();
    for f in code.natives() {
        libs.entry(f.lib.to_string())
            .or_default()
            .insert(f.symbol_name());
    }
    let std = libs.entry("std".to_owned()).or_default();
    for (name, _, _) in LIBHL_NATIVE_CALLS {
        std.insert(name.to_string());
    }
    let mut result = Vec::new();
    for (name, symbols) in libs {
        let symbols: Vec<String> = symbols.into_iter().collect();
        result.push(write(&name, create_stub(isa, &name, &symbols)));
    }

    result
}
