use std::ffi::c_void;

const CALL_REGS_COUNT: usize = 6;
const FPU_CALL_REGS: usize = 8;

#[derive(Default)]
struct CallInfo {
    cpu_regs: [u64; CALL_REGS_COUNT],
    fpu_regs: [u64; FPU_CALL_REGS],
    stack: Vec<u64>,
    num_cpu_args: usize,
    num_fpu_args: usize,
}

impl CallInfo {
    pub fn push_cpu(&mut self, val: u64) {
        if self.num_cpu_args < CALL_REGS_COUNT {
            self.cpu_regs[self.num_cpu_args] = val;
            self.num_cpu_args += 1;
        } else {
            self.stack.push(val);
        }
    }
    pub fn push_fpu(&mut self, val: u64) {
        if self.num_fpu_args < FPU_CALL_REGS {
            self.fpu_regs[self.num_fpu_args] = val;
            self.num_fpu_args += 1;
        } else {
            self.stack.push(val);
        }
    }
}

pub extern "C" fn static_call(
    fun: *const c_void,
    ft: &hl_sys::hl_type,
    args: *const *const c_void,
    out: *mut hl_sys::vdynamic,
) -> *mut c_void {
    let mut info = CallInfo::default();

    for (pos, ty) in ft.fun().args().iter().enumerate() {
        match ty.kind {
            hl_sys::hl_type_kind_HUI8 => {
                let val = unsafe { args.add(pos).read().cast::<u8>().read() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HUI16 => {
                let val = unsafe { args.add(pos).read().cast::<u16>().read() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HI32 => {
                let val = unsafe { args.add(pos).read().cast::<u32>().read() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HI64 | hl_sys::hl_type_kind_HGUID => {
                let val = unsafe { args.add(pos).read().cast::<u64>().read() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HF32 => {
                let val = unsafe { args.add(pos).read().cast::<f32>().read().to_bits() as u64 };
                info.push_fpu(val);
            }
            hl_sys::hl_type_kind_HF64 => {
                let val = unsafe { args.add(pos).read().cast::<f64>().read().to_bits() as u64 };
                info.push_fpu(val);
            }
            hl_sys::hl_type_kind_HBOOL => {
                let val = unsafe { args.add(pos).read().cast::<bool>().read() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HBYTES
            | hl_sys::hl_type_kind_HDYN
            | hl_sys::hl_type_kind_HFUN
            | hl_sys::hl_type_kind_HOBJ
            | hl_sys::hl_type_kind_HARRAY
            | hl_sys::hl_type_kind_HTYPE
            | hl_sys::hl_type_kind_HREF
            | hl_sys::hl_type_kind_HVIRTUAL
            | hl_sys::hl_type_kind_HDYNOBJ
            | hl_sys::hl_type_kind_HABSTRACT
            | hl_sys::hl_type_kind_HENUM
            | hl_sys::hl_type_kind_HNULL
            | hl_sys::hl_type_kind_HMETHOD
            | hl_sys::hl_type_kind_HSTRUCT => {
                let val = unsafe { args.add(pos).read().expose_provenance() as u64 };
                info.push_cpu(val);
            }
            hl_sys::hl_type_kind_HPACKED => panic!(),
            _ => panic!(),
        }
    }
    if info.stack.len() % 2 != 0 {
        info.stack.push(0);
    }
    info.stack.reserve(CALL_REGS_COUNT + FPU_CALL_REGS);
    info.stack.extend_from_slice(&info.cpu_regs);
    info.stack.extend_from_slice(&info.fpu_regs);
    let stack_top = info
        .stack
        .as_ptr()
        .wrapping_add(info.stack.len() - CALL_REGS_COUNT - FPU_CALL_REGS);
    let stack_bottom = info.stack.as_ptr();
    match unsafe { (*ft.fun().ret).kind } {
        hl_sys::hl_type_kind_HVOID => {
            unsafe {
                static_call_void(fun, stack_top.cast(), stack_bottom.cast());
            }
            return std::ptr::null_mut();
        }
        hl_sys::hl_type_kind_HUI8 => unsafe {
            (*out).v.ui8 = static_call_u8(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HUI16 => unsafe {
            (*out).v.ui16 = static_call_u16(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HI32 => unsafe {
            (*out).v.i = static_call_i32(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HI64 | hl_sys::hl_type_kind_HGUID => unsafe {
            (*out).v.i64_ = static_call_i64(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HF32 => unsafe {
            (*out).v.f = static_call_f32(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HF64 => unsafe {
            (*out).v.d = static_call_f64(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HBOOL => unsafe {
            (*out).v.b = static_call_bool(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HBYTES
        | hl_sys::hl_type_kind_HDYN
        | hl_sys::hl_type_kind_HFUN
        | hl_sys::hl_type_kind_HOBJ
        | hl_sys::hl_type_kind_HARRAY
        | hl_sys::hl_type_kind_HTYPE
        | hl_sys::hl_type_kind_HREF
        | hl_sys::hl_type_kind_HVIRTUAL
        | hl_sys::hl_type_kind_HDYNOBJ
        | hl_sys::hl_type_kind_HABSTRACT
        | hl_sys::hl_type_kind_HENUM
        | hl_sys::hl_type_kind_HNULL
        | hl_sys::hl_type_kind_HMETHOD
        | hl_sys::hl_type_kind_HSTRUCT => {
            return unsafe { static_call_ptr(fun, stack_top.cast(), stack_bottom.cast()) };
        }
        hl_sys::hl_type_kind_HPACKED => panic!(),
        _ => panic!(),
    }
}

#[allow(
    clashing_extern_declarations,
    reason = "static_call_impl is polymorphic"
)]
unsafe extern "C" {
    #[link_name = "static_call_impl"]
    unsafe fn static_call_void(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    );
    #[link_name = "static_call_impl"]
    unsafe fn static_call_f32(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> f32;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_f64(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> f64;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_ptr(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> *mut c_void;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_i64(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> i64;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_i32(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> i32;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_u8(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> u8;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_u16(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> u16;
    #[link_name = "static_call_impl"]
    unsafe fn static_call_bool(
        fun_ptr: *const c_void,
        stack_begin: *const u8,
        stack_end: *const u8,
    ) -> bool;
}

// sysv x64_64:
// - call regs: rdi, rsi, rdx, rcx, r8, and r9
// - fpu call regs: xmm0 through xmm7
// - preserved regs: rbx, rsp, rbp, r12, r13, r14, and r15
// - scratch regs: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
// - return in rax:

core::arch::global_asm!(
    ".global static_call_impl",
    "static_call_impl:",
    "   push rbp",
    "   mov rbp, rsp",
    // Move function ptr, stack begin and stack end
    "   mov r10, rdi",
    "   mov rax, rsi",
    "   mov r11, rdx",
    // set up call regs
    "   mov rdi, [rax]",
    "   mov rsi, [rax + 8]",
    "   mov rdx, [rax + 16]",
    "   mov rcx, [rax + 24]",
    "   mov r8,  [rax + 32]",
    "   mov r9,  [rax + 40]",
    // set up fpu call regs,
    "   movsd xmm0, [rax + 48]",
    "   movsd xmm1, [rax + 56]",
    "   movsd xmm2, [rax + 64]",
    "   movsd xmm3, [rax + 72]",
    "   movsd xmm4, [rax + 80]",
    "   movsd xmm5, [rax + 88]",
    "   movsd xmm6, [rax + 96]",
    "   movsd xmm7, [rax + 104]",
    // set up stack args
    "   0:  cmp rax, r11",
    "       jz 1f",
    "       sub rax, 8",
    "       push [rax]",
    "       jmp 0b",
    "   1: call r10",
    "   mov rsp, rbp",
    "   pop rbp",
    "   ret"
);
