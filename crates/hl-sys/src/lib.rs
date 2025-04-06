mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(improper_ctypes, reason = "triggered by bindgen generated u128")]
    #![allow(dead_code)]
    #![allow(unsafe_op_in_unsafe_fn)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

    impl varray {
        /// # Safety
        /// The type parameter `T` needs to be correct
        pub unsafe fn as_slice<T>(&self) -> &[T] {
            unsafe {
                core::slice::from_raw_parts(
                    core::ptr::from_ref(self).offset(1).cast(),
                    self.size as usize,
                )
            }
        }
    }

    unsafe impl Sync for vdynamic {}

    impl hl_type {
        pub fn fun(&self) -> &hl_type_fun {
            match self.kind {
                hl_type_kind_HFUN => unsafe { &*self.__bindgen_anon_1.fun },
                _ => panic!(),
            }
        }
    }

    impl hl_type_fun {
        pub fn args(&self) -> &[&hl_type] {
            if self.args.is_null() {
                &[]
            } else {
                unsafe { core::slice::from_raw_parts(self.args.cast(), self.nargs as usize) }
            }
        }
    }
}

pub use sys::*;
