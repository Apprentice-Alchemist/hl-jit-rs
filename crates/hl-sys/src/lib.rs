mod sys {
    #![expect(non_upper_case_globals)]
    #![expect(non_camel_case_types)]
    #![expect(non_snake_case)]
    #![allow(improper_ctypes, reason = "triggered by bindgen generated u128")]
    #![allow(unsafe_op_in_unsafe_fn)]
    #![allow(unnecessary_transmutes)]
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

use std::{
    ffi::{c_int, c_void},
    marker::PhantomData,
};

pub use sys::*;

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct UStr {
    ptr: *const u16,
}

impl UStr {
    pub unsafe fn from_ptr(ptr: *const u16) -> UStr {
        UStr { ptr }
    }

    pub fn iter(&'_ self) -> UStringIter<'_> {
        UStringIter::new(self)
    }
}

pub struct UStringIter<'a> {
    pos: usize,
    ptr: *const u16,
    phantom: PhantomData<&'a u16>,
}

impl UStringIter<'_> {
    fn new(s: &UStr) -> Self {
        UStringIter {
            pos: 0,
            ptr: s.ptr,
            phantom: PhantomData,
        }
    }
}

impl Iterator for UStringIter<'_> {
    type Item = u16;
    fn next(&mut self) -> Option<Self::Item> {
        let val = unsafe { self.ptr.add(self.pos).read() };
        if val == 0 {
            None
        } else {
            self.pos += 1;
            Some(val)
        }
    }
}

impl std::fmt::Display for UStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in char::decode_utf16(self.iter()) {
            std::fmt::Write::write_char(f, c.unwrap_or(char::REPLACEMENT_CHARACTER))?;
        }
        Ok(())
    }
}

pub static GLOBAL: Global = Global(());

pub struct Global(());

pub struct GlobalHandle<'a>(PhantomData<&'a Global>);

pub struct ThreadHandle<'thread, 'global: 'thread>(
    PhantomData<&'thread mut &'thread ()>,
    PhantomData<&'global mut &'global ()>,
);

impl Drop for GlobalHandle<'_> {
    fn drop(&mut self) {
        unsafe {
            sys::hl_global_free();
        }
    }
}

impl Global {
    pub fn builder<'a>(&'a self) -> GlobalBuilder<'a> {
        GlobalBuilder {
            callbacks: None,
            exception_callbacks: None,
            args: Vec::new(),
            file: None,
            _phantom: PhantomData,
        }
    }
}

type StaticCallCallback = extern "C-unwind" fn(
    fun: *const c_void,
    ft_ptr: *mut sys::hl_type,
    args: *const *const c_void,
    out: *mut sys::vdynamic,
) -> *mut c_void;
type GetWrapperCallback = extern "C" fn(t: *mut hl_type) -> *const c_void;

type ResolveSymbolCallback =
    extern "C" fn(addr: *mut c_void, out: *mut u16, out_size: *mut c_int) -> *mut u16;
type CaptureStackCallback = extern "C" fn(stack: *mut *mut c_void, size: c_int) -> c_int;

pub struct GlobalBuilder<'a> {
    callbacks: Option<(StaticCallCallback, GetWrapperCallback)>,
    exception_callbacks: Option<(ResolveSymbolCallback, CaptureStackCallback)>,
    args: Vec<String>,
    file: Option<String>,
    _phantom: PhantomData<&'a Global>,
}
#[cfg(windows)]
type PStr = *const u16;
#[cfg(not(windows))]
type PStr = *const u8;

fn string_into_pstr(s: String) -> PStr {
    #[cfg(windows)]
    {
        s.encode_utf16().collect::<Vec<_>>().leak().as_ptr()
    }
    #[cfg(not(windows))]
    {
        use std::ffi::CString;

        CString::new(s).unwrap().into_raw().cast()
    }
}

impl<'a> GlobalBuilder<'a> {
    pub fn set_callbacks(
        mut self,
        static_call: StaticCallCallback,
        get_wrapper: GetWrapperCallback,
    ) -> Self {
        self.callbacks = Some((static_call, get_wrapper));
        self
    }

    pub fn set_exception_callbacks(
        mut self,
        resolve_symbol: ResolveSymbolCallback,
        capture_stack: CaptureStackCallback,
    ) -> Self {
        self.exception_callbacks = Some((resolve_symbol, capture_stack));
        self
    }

    pub fn set_args(mut self, args: impl IntoIterator<Item = String>) -> Self {
        self.args = args.into_iter().collect::<Vec<_>>();
        self
    }

    pub fn set_file(mut self, file: &'a str) -> Self {
        self.file = Some(file.into());
        self
    }

    pub fn init(self) -> GlobalHandle<'a> {
        unsafe {
            sys::hl_global_init();
        }
        if let Some((static_call, get_wrapper)) = self.callbacks {
            unsafe {
                sys::hl_setup_callbacks(static_call as *mut c_void, get_wrapper as *mut c_void);
            }
        }
        if let Some((resolve_symbol, capture_stack)) = self.exception_callbacks {
            unsafe {
                sys::hl_setup_exception(
                    resolve_symbol as *mut c_void,
                    capture_stack as *mut c_void,
                );
            }
        }

        let c_file = self.file.map(|f| string_into_pstr(f));
        let args = self
            .args
            .into_iter()
            .map(|s| string_into_pstr(s))
            .collect::<Vec<PStr>>()
            .leak();
        unsafe {
            sys::hl_sys_init(
                args.as_mut_ptr().cast(),
                args.len().try_into().unwrap(),
                c_file
                    .unwrap_or(core::ptr::null_mut())
                    .cast::<c_void>()
                    .cast_mut(),
            )
        }
        GlobalHandle(PhantomData)
    }
}

impl GlobalHandle<'_> {
    /// Registers the current thread with the hashlink runtime
    /// executes `cb` and then unregisters the thread
    ///
    /// # Panics
    /// If `cb` panics and unwinding is enabled then the panic will be caught, the thread unregistered and the panic will resume
    pub fn with_current_thread<'global, T>(
        &'global self,
        cb: impl for<'thread> FnOnce(&'thread ThreadHandle<'thread, 'global>) -> T,
    ) -> T {
        let handle = ThreadHandle(PhantomData, PhantomData);
        unsafe {
            sys::hl_register_thread((&raw const handle).cast_mut().cast());
        }
        let ret = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| cb(&handle)));
        unsafe {
            sys::hl_unregister_thread();
        }

        match ret {
            Ok(val) => val,
            Err(e) => std::panic::resume_unwind(e),
        }
    }
}

#[repr(transparent)]
pub struct Type<'a>(sys::hl_type, PhantomData<&'a sys::hl_type>);

impl Type<'_> {
    pub fn void() -> &'static Type<'static> {
        // TODO: figure out why hlt_void from mod sys causes linker errors
        #[cfg(windows)]
        #[link(name = "libhl")]
        unsafe extern "C" {
            unsafe static mut hlt_void: hl_type;
        }
        // Safety: Type has #[repr(transparent)] so &Type and *const hl_type have compatible layout
        unsafe { core::mem::transmute(&raw const hlt_void) }
    }
    pub fn fun<'a>(args: &'a [&'a Type<'a>], ret: &'a Type) -> Type<'a> {
        use core::ptr::null_mut;

        let __bindgen_anon_1 = hl_type__bindgen_ty_1 {
            // TODO: get rid of Box::leak
            fun: Box::leak(Box::new(hl_type_fun {
                args: args.as_ptr().cast_mut().cast(),
                ret: (&raw const ret.0).cast_mut(),
                nargs: args.len().try_into().unwrap(),
                parent: null_mut(),
                closure_type: unsafe { core::mem::zeroed() },
                closure: unsafe { core::mem::zeroed() },
            })),
        };
        Type(
            sys::hl_type {
                kind: sys::hl_type_kind_HFUN,
                __bindgen_anon_1,
                vobj_proto: null_mut(),
                mark_bits: null_mut(),
            },
            PhantomData,
        )
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct VArray<'a, T>(&'a sys::varray, PhantomData<T>);

impl<'a, T: 'a> IntoIterator for VArray<'a, T>
where
    T: Copy,
{
    type Item = T;

    type IntoIter = VArrayIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        unsafe { VArrayIterator(self.0.as_slice::<T>().iter()) }
    }
}

pub struct VArrayIterator<'a, T: Copy>(core::slice::Iter<'a, T>);

impl<T: Copy> Iterator for VArrayIterator<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().copied()
    }
}

#[repr(transparent)]
pub struct VDynamic<'a>(&'a vdynamic);

impl VDynamic<'_> {
    pub fn ty(&'_ self) -> &'_ Type<'_> {
        unsafe { self.0.t.cast::<Type>().as_ref().unwrap() }
    }

    pub fn to_string(&self) -> UStr {
        UStr {
            ptr: unsafe { sys::hl_to_string(core::ptr::from_ref(self.0).cast_mut()) },
        }
    }
}

#[repr(transparent)]
pub struct VClosure(vclosure);

impl VClosure {
    pub fn new(t: &Type, fun: *const std::ffi::c_void) -> Self {
        VClosure(vclosure {
            t: (&raw const t.0).cast_mut(),
            fun: fun.cast_mut(),
            hasValue: 0,
            stackCount: 0,
            value: core::ptr::null_mut(),
        })
    }
    pub fn as_ptr(&self) -> *const vclosure {
        &raw const self.0
    }
}

impl<'thread, 'global> ThreadHandle<'thread, 'global> {
    pub fn dyn_call_safe(
        &'thread self,
        c: &VClosure,
        args: &[VDynamic],
    ) -> Result<Option<VDynamic<'global>>, VDynamic<'global>> {
        #[expect(clashing_extern_declarations)]
        unsafe extern "C" {
            pub unsafe fn hl_dyn_call_safe<'a>(
                c: &vclosure,
                args: *mut &vdynamic,
                nargs: ::std::ffi::c_int,
                isException: &mut bool,
            ) -> Option<&'a vdynamic>;
        }

        let mut is_exception = false;
        let ret = unsafe {
            hl_dyn_call_safe(
                &c.0,
                args.as_ptr().cast_mut().cast(),
                args.len().try_into().unwrap(),
                &mut is_exception,
            )
        };
        if is_exception {
            Err(VDynamic(ret.unwrap()))
        } else {
            Ok(ret.map(|d| VDynamic(d)))
        }
    }

    pub fn exception_stack(&'_ self) -> VArray<'_, UStr> {
        unsafe {
            VArray(
                sys::hl_exception_stack().cast_const().as_ref().unwrap(),
                PhantomData,
            )
        }
    }
}
