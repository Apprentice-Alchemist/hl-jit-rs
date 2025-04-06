use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    cc::Build::new()
        .file("src/ffi_callbacks.c")
        .compile("ffi_callbacks");
    println!("cargo::rustc-link-lib=ffi");
    println!("cargo::rustc-link-search=/usr/local/lib");
    println!("cargo::rustc-link-arg=-Wl,-rpath,/usr/local/lib");
    Ok(())
}
