use std::{env, error::Error, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
    let header = if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        println!("cargo::rerun-if-env-changed=HASHLINK");
        let v = std::env::var("HASHLINK").unwrap();
        println!("cargo::rustc-link-lib=libhl");
        println!("cargo::rustc-link-search={v}");
        PathBuf::from(v)
            .join("include")
            .join("hl.h")
            .to_string_lossy()
            .into_owned()
    } else {
        println!("cargo::rustc-link-lib=hl");
        println!("cargo::rustc-link-search=/usr/local/lib");
        println!("cargo::rustc-link-arg=-Wl,-rpath,/usr/local/lib");
        "header.h".to_string()
    };
    println!("{header}");
    let bindings = bindgen::builder()
        .header(&header)
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

    Ok(())
}
