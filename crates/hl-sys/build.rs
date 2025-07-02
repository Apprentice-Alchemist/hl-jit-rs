use std::{
    env,
    error::Error,
    io::ErrorKind,
    path::{Path, PathBuf},
};

fn find_prefix() -> PathBuf {
    let brew_path = match std::process::Command::new("brew")
        .args(["--prefix"])
        .output()
    {
        Ok(output) => output
            .status
            .success()
            .then(|| String::from_utf8(output.stdout).unwrap().trim().to_string()),
        Err(e) if e.kind() == ErrorKind::NotFound => None,
        Err(e) => {
            println!("cargo::warning=brew command failed: {e}");
            None
        }
    };
    if let Some(brew_path) = brew_path {
        let header_path = Path::new(&brew_path).join("include/hl.h");
        if header_path.exists() {
            return brew_path.into();
        }
    }
    let search_paths = ["/usr/local", "/usr"];
    for path in search_paths {
        let p = Path::new(path).join("include/hl.h");
        if p.exists() {
            return path.into();
        }
    }
    println!("cargo::error=hashlink header not found");
    std::process::exit(0);
}

fn main() -> Result<(), Box<dyn Error>> {
    let header = if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        println!("cargo::rerun-if-env-changed=HASHLINK");
        let v = std::env::var("HASHLINK").unwrap();
        println!("cargo::rustc-link-lib=libhl");
        println!("cargo::rustc-link-search={v}");
        PathBuf::from(v)
            .join("include/hl.h")
            .canonicalize().unwrap()
            .to_string_lossy()
            .into_owned()
    } else {
        let prefix = find_prefix();
        let header_path = prefix.join("include/hl.h");
        let libdir = prefix.join("lib");
        let libdir = libdir.to_str().unwrap();
        println!("cargo::rustc-link-lib=hl");
        println!("cargo::rustc-link-search={}", libdir);
        println!("cargo::metadata=RPATH={}", libdir);
        header_path.to_string_lossy().to_string()
    };

    let bindings = bindgen::builder()
        .header(&header)
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .ctypes_prefix("::std::ffi")
        .merge_extern_blocks(true)
        .generate()?;

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    Ok(())
}
