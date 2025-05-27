mod sysv;

use std::{ffi::c_void, ptr::null_mut};

use hl_sys::{
    hl_is_dynamic, hl_type, hl_type_kind_HF32, hl_type_kind_HF64, hl_wrapper_call,
    vclosure_wrapper, vdynamic,
};

use sysv::CALL_REGS_COUNT;
use sysv::FPU_CALL_REGS;
use sysv::static_call_impl_naked;

#[derive(Copy, Clone)]
pub union CpuValue {
    pub i8: i8,
    pub i16: i16,
    pub i32: i32,
    pub i64: i64,
    pub ptr: *mut c_void,
}
#[derive(Copy, Clone)]
pub union FloatValue {
    pub f32: f32,
    pub f64: f64,
}

pub union Value {
    pub cpu: CpuValue,
    pub float: FloatValue,
}

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

#[unsafe(export_name = "hlc_static_call")]
pub extern "C" fn static_call(
    fun: *const c_void,
    ft_ptr: *mut hl_sys::hl_type,
    args: *const *const c_void,
    out: *mut hl_sys::vdynamic,
) -> *mut c_void {
    let mut info = CallInfo::default();
    let ft = unsafe { ft_ptr.as_ref().unwrap() };
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
                static_call_impl::<()>(fun, stack_top.cast(), stack_bottom.cast());
            }
            return std::ptr::null_mut();
        }
        hl_sys::hl_type_kind_HUI8 => unsafe {
            (*out).v.ui8 = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HUI16 => unsafe {
            (*out).v.ui16 = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HI32 => unsafe {
            (*out).v.i = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HI64 | hl_sys::hl_type_kind_HGUID => unsafe {
            (*out).v.i64_ = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HF32 => unsafe {
            (*out).v.f = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HF64 => unsafe {
            (*out).v.d = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
            return (&raw mut (*out).v).cast();
        },
        hl_sys::hl_type_kind_HBOOL => unsafe {
            (*out).v.b = static_call_impl(fun, stack_top.cast(), stack_bottom.cast());
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
            return unsafe { static_call_impl(fun, stack_top.cast(), stack_bottom.cast()) };
        }
        hl_sys::hl_type_kind_HPACKED => panic!(),
        _ => panic!(),
    }
}

#[unsafe(export_name = "hlc_get_wrapper")]
pub extern "C" fn get_wrapper(_t: *mut hl_type) -> *const c_void {
    unsafe extern "C" {
        unsafe fn wrapper_call_impl();
    }
    wrapper_call_impl as *const c_void
}

#[repr(C)]
struct Registers {
    cpu_regs: [CpuValue; CALL_REGS_COUNT],
    fpu_regs: [FloatValue; FPU_CALL_REGS],
}

extern "C" fn wrapper_inner(
    c: *const vclosure_wrapper,
    regs: &Registers,
    stack_args: *const Value,
    ret: *mut vdynamic,
) -> *mut c_void {
    let mut args = Vec::new();
    let t = unsafe { &*((*c).cl).t };
    let mut num_cpu_args = 1;
    let mut num_fpu_args = 0;
    let mut num_stack_args = 0;
    for arg in t.fun().args() {
        if unsafe { hl_is_dynamic(core::ptr::from_ref(*arg).cast_mut()) } {
            if num_cpu_args < CALL_REGS_COUNT {
                args.push(unsafe { regs.cpu_regs[num_cpu_args].ptr });
                num_cpu_args += 1;
            } else {
                unsafe {
                    args.push(stack_args.add(num_stack_args).read().cpu.ptr);
                }
                num_stack_args += 1;
            }
        } else if arg.kind == hl_type_kind_HF32 || arg.kind == hl_type_kind_HF64 {
            if num_fpu_args < FPU_CALL_REGS {
                args.push(
                    core::ptr::from_ref(&regs.fpu_regs[num_fpu_args])
                        .cast_mut()
                        .cast(),
                );
                num_fpu_args += 1;
            } else {
                unsafe {
                    args.push(stack_args.add(num_stack_args).cast_mut().cast());
                }
                num_stack_args += 1;
            }
        } else {
            if num_cpu_args < CALL_REGS_COUNT {
                args.push(
                    core::ptr::from_ref(&regs.cpu_regs[num_cpu_args])
                        .cast_mut()
                        .cast(),
                );
                num_cpu_args += 1;
            } else {
                unsafe {
                    args.push(stack_args.add(num_stack_args).cast_mut().cast());
                }
                num_stack_args += 1;
            }
        }
    }
    match unsafe { (*t.fun().ret).kind } {
        hl_sys::hl_type_kind_HVOID => unsafe {
            return hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), null_mut());
        },
        hl_sys::hl_type_kind_HUI8 => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (*ret).v.ptr;
        },
        hl_sys::hl_type_kind_HUI16 => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (*ret).v.ptr;
        },
        hl_sys::hl_type_kind_HI32 => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (*ret).v.ptr;
        },
        hl_sys::hl_type_kind_HI64 | hl_sys::hl_type_kind_HGUID => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (*ret).v.ptr;
        },
        hl_sys::hl_type_kind_HF32 => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (&raw mut (*ret).v).cast();
        },
        hl_sys::hl_type_kind_HF64 => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (&raw mut (*ret).v).cast();
        },
        hl_sys::hl_type_kind_HBOOL => unsafe {
            hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), ret);
            return (*ret).v.ptr;
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
        | hl_sys::hl_type_kind_HSTRUCT => unsafe {
            return hl_wrapper_call(c.cast_mut().cast(), args.as_mut_ptr(), null_mut());
        },
        hl_sys::hl_type_kind_HPACKED => panic!(),
        _ => panic!(),
    }
}
