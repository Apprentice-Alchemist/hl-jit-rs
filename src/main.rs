use clap::{CommandFactory, Parser};
use std::{
    error::Error,
    ffi::{OsString, c_int, c_void},
    io::Write,
    path::PathBuf,
    sync::atomic::AtomicBool,
    time::Instant,
};
use target_lexicon::OperatingSystem;
use tempfile::TempPath;

pub use hl_code as code;

mod codegen;
mod jit;
mod object;
mod stub;
mod unwind;

/// Hashlink JIT compiler
#[derive(Parser, Debug)]
#[command(version, about, long_about = None, args_conflicts_with_subcommands = true)]
struct Args {
    /// Print timings for compilation phases
    #[arg(global = true, long)]
    timings: bool,
    #[command(subcommand)]
    compile: Option<Compile>,

    #[command(flatten)]
    run: Run,
}

#[derive(Debug, clap::Args)]
struct Run {
    /// Compile but don't run
    #[arg(long)]
    no_run: bool,
    /// Bytecode file to execute
    file: Option<String>,
    /// Program arguments
    args: Vec<String>,
}

#[derive(clap::Subcommand, Debug)]
enum Compile {
    #[command(hide = true)]
    /// Compile to object file
    Compile {
        /// File containing HL bytecode
        file: PathBuf,
        /// Output file name
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Link to executable
        #[arg(long)]
        link: bool,
        /// Target
        #[arg(long)]
        target: Option<String>,
    },
}

extern "C" fn resolve_symbol(addr: *mut c_void, out: *mut u16, out_size: *mut c_int) -> *mut u16 {
    backtrace::resolve(addr, |sym| {
        if let Some(name) = sym.name() {
            let name = format!("{}", name);
            let mut pos = 0;
            for c in name.encode_utf16() {
                if pos >= unsafe { *out_size } as usize {
                    break;
                }
                unsafe { out.add(pos).write(c) };
                pos += 1;
            }
            unsafe {
                out.add(pos).write(0);
                *out_size = pos as c_int;
            }
        }
    });
    out
}
extern "C" fn capture_stack(stack: *mut *mut c_void, size: c_int) -> c_int {
    let mut pos = 0;
    if stack.is_null() {
        backtrace::trace(|_| {
            pos += 1;
            true
        });
    } else {
        backtrace::trace(|frame| {
            unsafe {
                stack.add(pos as usize).write(frame.ip());
            }
            pos += 1;
            pos < size
        });
    }
    pos
}
// "Null Access" in UTF-16
// static NULL_ACCESS_BYTES: &[u8] = b"N\0u\0l\0l\0 \0A\0c\0c\0e\0s\0s\0\0\0";
// "SIGILL" in UTF-16
// static SIGILL_BYTES: &[u8] = b"S\0I\0G\0I\0L\0L\0\0\0";

// static mut NULL_ACCESS_EXC: vdynamic = vdynamic {
//     t: &raw mut hlt_bytes,
//     v: vdynamic__bindgen_ty_1 {
//         bytes: NULL_ACCESS_BYTES.as_ptr().cast_mut(),
//     },
// };

// static mut SIGILL_EXC: vdynamic = vdynamic {
//     t: &raw mut hlt_bytes,
//     v: vdynamic__bindgen_ty_1 {
//         bytes: SIGILL_BYTES.as_ptr().cast_mut(),
//     },
// };

static COLLECT_TIMING: AtomicBool = AtomicBool::new(false);

fn time<T>(stage: &'static str, f: impl FnOnce() -> T) -> T {
    if COLLECT_TIMING.load(std::sync::atomic::Ordering::Relaxed) {
        let start = Instant::now();
        let ret = f();
        println!("{stage}: {:?}", start.elapsed());
        ret
    } else {
        f()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    clap_complete::env::CompleteEnv::with_factory(Args::command).complete();
    let args = Args::parse();
    COLLECT_TIMING.store(args.timings, std::sync::atomic::Ordering::Relaxed);

    if let Some(Compile::Compile {
        file,
        output,
        link,
        target,
    }) = args.compile
    {
        let code = time("parsing", || hl_code::Code::from_file(&file).unwrap());
        let (product, isa) = time("compiling", || {
            crate::object::compile_module(&code, file.to_string_lossy().as_ref(), target.clone())
        });

        if !link {
            time("write_object", move || {
                let bytes = product.emit()?;
                let out_file = output.unwrap_or_else(|| file.with_extension("o"));
                std::fs::write(out_file, bytes)?;
                Ok::<(), Box<dyn Error>>(())
            })?;
        } else {
            let object_path = time("write_object", || {
                let mut file = tempfile::NamedTempFile::with_suffix(".o")?;
                product
                    .object
                    .write_stream(std::io::BufWriter::new(file.as_file_mut()))?;
                Ok::<TempPath, Box<dyn Error>>(file.into_temp_path())
            })?;

            let stub_paths: Vec<TempPath> = time("create_stubs", || {
                stub::create_stubs(&code, isa.as_ref(), |_, bytes| {
                    let mut file = tempfile::NamedTempFile::new().unwrap();
                    file.as_file_mut().write_all(&bytes).unwrap();
                    file.into_temp_path()
                })
            });

            let status = time("link_executable", || {
                let out_file = output.unwrap_or_else(|| {
                    file.with_extension(
                        if isa.triple().operating_system == OperatingSystem::Windows {
                            "exe"
                        } else {
                            ""
                        },
                    )
                });
                eprintln!(
                    "WARNING: linking to executable is experimental and will likely not work"
                );

                let mut command = std::process::Command::new("cc");
                command
                    .args([
                        "-L",
                        "/usr/local/lib",
                        "-L",
                        "target/debug",
                        // order is, unfortunately, relevant when GNU ld is used
                        object_path.to_str().unwrap(),
                        "-lhl_ffi",
                        "-lhl",
                        "-lm",
                        "-o",
                        format!("{}", out_file.file_name().unwrap().display()).as_str(),
                        "-Wl,-rpath,/usr/local/lib",
                        "-g",
                    ])
                    .args(&stub_paths);

                command.status().unwrap()
            });
            if !status.success() {
                eprintln!("failed to compile to executable");
                std::process::exit(1);
            }
        }
    } else {
        let file = args.run.file.unwrap_or_else(|| {
            Args::command().print_help().unwrap();
            std::process::exit(0);
        });
        let code = time("parsing", || hl_code::Code::from_file(&file).unwrap());
        let (m, entrypoint) = time("jit", move || crate::jit::compile_module(code));
        if !args.run.no_run {
            let args: Vec<_> = args.run.args.iter().map(|s| OsString::from(s)).collect();
            let f = m
                .get_finalized_function(entrypoint)
                .cast::<c_void>()
                .cast_mut();
            let success = time("run", || run_jit(args, file, f)).is_ok();
            if !success {
                std::process::exit(1);
            }
        }
    }
    Ok(())
}

fn run_jit(args: Vec<OsString>, file: String, fun: *mut c_void) -> Result<(), ()> {
    #[cfg(not(feature = "hl-ffi"))]
    unsafe extern "C" {
        unsafe fn hlc_static_call(
            fun: *mut c_void,
            ft: *mut hl_type,
            args: *mut *mut c_void,
            out: *mut vdynamic,
        ) -> *mut c_void;
        unsafe fn hlc_get_wrapper(ty: *mut hl_type) -> *mut c_void;
    }
    let global = hl_sys::GLOBAL
        .builder()
        .set_callbacks(hl_ffi::static_call, hl_ffi::get_wrapper)
        .set_exception_callbacks(resolve_symbol, capture_stack)
        .set_args(args)
        .set_file(&file)
        .init();
    global.with_current_thread(|thread| {
        let ty = hl_sys::Type::fun(&[], hl_sys::Type::void());
        let c = hl_sys::VClosure::new(&ty, fun.cast_const());
        match thread.dyn_call_safe(&c, &[]) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("Uncaught exception: {}", e.to_string(),);
                let stack = thread.exception_stack();
                for (pos, elem) in stack.into_iter().enumerate() {
                    println!("  {pos}: {}", elem);
                }
            }
        }
    });
    Ok(())
}
