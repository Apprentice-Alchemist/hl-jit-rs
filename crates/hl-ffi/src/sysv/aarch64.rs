use std::ffi::c_void;

pub const CALL_REGS_COUNT: usize = 8;
pub const FPU_CALL_REGS: usize = 8;

#[unsafe(naked)]
pub unsafe extern "C-unwind" fn static_call_impl<T>(
    fun_ptr: *const c_void,
    stack_begin: *const u8,
    stack_end: *const u8,
) -> T {
    core::arch::naked_asm!(
        "   .cfi_startproc",
        "   stp fp, lr, [sp, #-16]!",
        "   .cfi_adjust_cfa_offset 16",
        "   mov fp, sp",
        "   .cfi_def_cfa_register fp",
        // Move function ptr, stack begin and stack end
        "   mov x9, x0",
        "   mov x10, x1",
        "   mov x11, x2",
        // set up call regs
        "   ldp x0, x1, [x10]",
        "   ldp x2, x3, [x10, 16]",
        "   ldp x4, x5, [x10, 32]",
        "   ldp x6, x7, [x10, 48]",
        // set up fpu call regs,
        "   ldp d0, d1, [x10, 64]",
        "   ldp d2, d3, [x10, 80]",
        "   ldp d4, d5, [x10, 96]",
        "   ldp d6, d7, [x10, 112]",
        // set up stack args
        "   0:  cmp x10, x11",
        "       bhs 1f",
        // the stack is always aligned to 16 bytes
        "       ldp x12, x13, [x10, #-16]!",
        "       stp x12, x13, [sp, #-16]!",
        "       b 0b",
        "   1: blr x9",
        "   mov sp, fp",
        "   ldp fp, lr, [sp], #16",
        "	.cfi_def_cfa sp, 0",
        "   ret",
        "   .cfi_endproc",
    );
}

core::arch::global_asm!(
    ".global wrapper_call_impl",
    "wrapper_call_impl:",
    "   .cfi_startproc",
    "   stp fp, lr, [sp, #-16]!",
    "   .cfi_adjust_cfa_offset 16",
    "   mov fp, sp",
    "   .cfi_def_cfa_register fp",
    "   sub sp, sp, 128",
    "   stp x0, x1, [sp]",
    "   stp x2, x3, [sp, 16]",
    "   stp x4, x5, [sp, 32]",
    "   stp x6, x7, [sp, 48]",
    "   stp d0, d1, [sp, 64]",
    "   stp d2, d3, [sp, 80]",
    "   stp d4, d5, [sp, 96]",
    "   stp d6, d7, [sp, 112]",
    "   add x2, fp, 16",
    "   mov x1, sp",
    "   sub sp, sp, 16",
    "   mov x3, sp",
    "   ldr x9, [x0]", // ->t
    "   ldr x9, [x9, 8]", // ->fun
    "   ldr x9, [x9, 8]", // ->ret
    "   ldr w9, [x9]", // ->kind
    "   cmp w9, 5",
    "   beq 0f",
    "   cmp w9, 6",
    "   beq 0f",
    "   bl {wrapper_ptr}",
    "   b 1f",
    "   0: bl {wrapper_ptr}",
    "   ldr d0, [x0]",
    "   1:",
    "   mov sp, fp",
    "   ldp fp, lr, [sp], #16",
    "	.cfi_def_cfa sp, 0",
    "   ret",
    "   .cfi_endproc",
    wrapper_ptr = sym crate::wrapper_inner,
);
