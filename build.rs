use std::{env, error::Error, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
    let bindings = bindgen::builder()
        .header("header.h")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .ctypes_prefix("::std::ffi")
        .generate()?;

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
    println!("cargo::rustc-link-lib=hl");
    println!("cargo::rustc-link-arg=-Wl,-rpath,/usr/local/lib");
    Ok(())
}
