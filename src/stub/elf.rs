//! ELF stub generation

use std::collections::{HashMap, HashSet};

use cranelift::{codegen::ir, prelude::isa::{OwnedTargetIsa, TargetIsa}};
use hl_code::Code;

/// Create an ELF .so stub file.
/// It exports all the provided symbols, but is otherwise empty.
pub fn create_elf_stub(isa: &dyn TargetIsa, name: &str, symbols: &[String]) -> Vec<u8> {
    use cranelift::object::object;
    use cranelift::object::object::write::elf as write;
    use cranelift::object::object::{Architecture, Endian, elf};

    let soname = match name {
        "std" => "libhl.so".to_string(),
        name => format!("{name}.hdll")
    };

    let mut stub_buf = Vec::new();

    // Build the stub ELF using the object crate.
    // The high-level portable API does not allow for the fine-grained control we need,
    // so this uses the low-level object::write::elf API.
    // The low-level API consists of two stages: reservation and writing.
    // We first reserve space for all the things in the binary and then write them.
    // It is important that the order of reservation matches the order of writing.
    // The object crate contains many debug asserts that fire if you get this wrong.

    let endianness = match isa.endianness() {
        ir::Endianness::Little => object::Endianness::Little,
        ir::Endianness::Big => object::Endianness::Big,
    };
    let mut stub = write::Writer::new(endianness, true, &mut stub_buf);

    // These initial reservations don't reserve any bytes in the binary yet,
    // they just allocate in the internal data structures.

    // First, we crate the dynamic symbol table. It starts with a null symbol
    // and then all the symbols and their dynamic strings.
    stub.reserve_null_dynamic_symbol_index();

    let dynstrs = symbols
        .iter()
        .map(|sym| {
            stub.reserve_dynamic_symbol_index();
            (sym, stub.add_dynamic_string(sym.as_bytes()))
        })
        .collect::<Vec<_>>();

    let soname = stub.add_dynamic_string(soname.as_bytes());

    // Reserve the sections.
    // We have the minimal sections for a dynamic SO and .text where we point our dummy symbols to.
    stub.reserve_shstrtab_section_index();
    let text_section_name = stub.add_section_name(".text".as_bytes());
    let text_section = stub.reserve_section_index();
    stub.reserve_dynstr_section_index();
    stub.reserve_dynsym_section_index();
    stub.reserve_dynamic_section_index();

    // These reservations now determine the actual layout order of the object file.
    stub.reserve_file_header();
    stub.reserve_shstrtab();
    stub.reserve_section_headers();
    stub.reserve_dynstr();
    stub.reserve_dynsym();
    stub.reserve_dynamic(2); // DT_SONAME, DT_NULL

    // First write the ELF header with the arch information.
    let e_machine = match isa.triple().architecture {
		target_lexicon::Architecture::Aarch64(_) => elf::EM_AARCH64,
		target_lexicon::Architecture::X86_64 => elf::EM_X86_64,
		target_lexicon::Architecture::Riscv64(_) => elf::EM_RISCV,
		target_lexicon::Architecture::S390x => elf::EM_S390,
        arch => todo!("unsupported architecture {arch}"),
    };

    stub.write_file_header(&write::FileHeader {
        os_abi: object::elf::ELFOSABI_NONE,
        abi_version: 0,
        e_type: object::elf::ET_DYN,
        e_machine,
        e_entry: 0,
        e_flags: 0,
    })
    .unwrap();

    // .shstrtab
    stub.write_shstrtab();

    // Section headers
    stub.write_null_section_header();
    stub.write_shstrtab_section_header();
    // Create a dummy .text section for our dummy symbols.
    stub.write_section_header(&write::SectionHeader {
        name: Some(text_section_name),
        sh_type: elf::SHT_PROGBITS,
        sh_flags: 0,
        sh_addr: 0,
        sh_offset: 0,
        sh_size: 0,
        sh_link: 0,
        sh_info: 0,
        sh_addralign: 1,
        sh_entsize: 0,
    });
    stub.write_dynstr_section_header(0);
    stub.write_dynsym_section_header(0, 1);
    stub.write_dynamic_section_header(0);

    // .dynstr
    stub.write_dynstr();

    // .dynsym
    stub.write_null_dynamic_symbol();
    for (_, name) in dynstrs {
        stub.write_dynamic_symbol(&write::Sym {
            name: Some(name),
            st_info: (elf::STB_GLOBAL << 4) | elf::STT_FUNC,
            st_other: elf::STV_DEFAULT,
            section: Some(text_section),
            st_shndx: 0, // ignored by object in favor of the `section` field
            st_value: 0,
            st_size: 0,
        });
    }

    // .dynamic
    // the DT_SONAME will be used by the linker to populate DT_NEEDED
    // which the loader uses to find the library.
    // DT_NULL terminates the .dynamic table.
    stub.write_dynamic_string(elf::DT_SONAME, soname);
    stub.write_dynamic(elf::DT_NULL, 0);

    stub_buf
}
