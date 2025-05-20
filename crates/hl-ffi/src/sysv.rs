cfg_if::cfg_if! (
    if #[cfg(all(target_arch = "x86_64"))] {
        mod x86_64;
        pub use x86_64::*;
    } else if #[cfg(target_arch = "aarch64")] {
        mod aarch64;
        pub use aarch64::*;
    }
);
