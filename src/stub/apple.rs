//! Apple TBD (Text-based Dynamic Library Stubs) generation

use cranelift::prelude::isa::TargetIsa;
use target_lexicon::{Architecture, OperatingSystem};

pub fn create_tbd_stub(isa: &dyn TargetIsa, name: &str, symbols: &[String]) -> Vec<u8> {
    use std::io::Write;
    let install_name = match name {
        "std" => "libhl.dylib".to_string(),
        name => format!("{name}.hdll"),
    };
    let arch = match isa.triple().architecture {
        Architecture::Aarch64(_) => "arm64",
        Architecture::X86_64 => "x86_64",
        Architecture::X86_64h => "x86_64h",
        _ => panic!("unsupported architecture for Apple target"),
    };
    let os = match isa.triple().operating_system {
        OperatingSystem::Darwin(_) => {
            "macos" // todo
        }
        OperatingSystem::MacOSX(_) => "macos",
        OperatingSystem::IOS(_) => "ios",
        OperatingSystem::WatchOS(_) => "watchos",
        OperatingSystem::TvOS(_) => "tvos",
        OperatingSystem::XROS(_) => "xros",
        os => panic!("{os} is not an Apple operating system"),
    };
    let mut buf = Vec::new();
    writeln!(buf, "--- !tapi-tbd").unwrap();
    writeln!(buf, "tbd-version: 4").unwrap();
    writeln!(buf, "targets: [ {}-{} ]", arch, os).unwrap();
    writeln!(buf, "install-name: \"@rpath/{install_name}\"").unwrap();
    writeln!(buf, "exports:").unwrap();
    writeln!(buf, "  - targets: [ {}-{} ]", arch, os).unwrap();
    writeln!(
        buf,
        "    symbols: [ {} ]",
        symbols
            .iter()
            .map(|name| "_".to_owned() + name)
            .collect::<Vec<String>>()
            .join(", ")
    )
    .unwrap();
    writeln!(buf, "...").unwrap();
    buf
}
