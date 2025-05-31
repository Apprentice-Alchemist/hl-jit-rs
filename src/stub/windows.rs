//! Windows import library generation

use std::io::Cursor;

use ar_archive_writer::{COFFShortExport, MachineTypes};
use cranelift::prelude::isa::TargetIsa;

pub fn create_import_library(isa: &dyn TargetIsa, name: &str, symbols: &[String]) -> Vec<u8> {
    let import_name = match name {
        "std" => "libhl.dll".to_string(),
        name => format!("{name}.hdll"),
    };
    let mut buf = Cursor::new(Vec::new());
    let exports: Vec<COFFShortExport> = symbols
        .iter()
        .map(|name| COFFShortExport {
            name: name.clone(),
            ext_name: None,
            symbol_name: None,
            alias_target: None,
            ordinal: 0,
            noname: false,
            data: false,
            private: false,
            constant: false,
        })
        .collect();
    let machine = match isa.triple().architecture {
        target_lexicon::Architecture::X86_64 => MachineTypes::AMD64,
        target_lexicon::Architecture::Aarch64(_) => MachineTypes::ARM64,
        _ => panic!("unsupported architecture for import libraries"),
    };
    ar_archive_writer::write_import_library(&mut buf, &import_name, &exports, machine, false, true)
        .unwrap();
    buf.into_inner()
}
