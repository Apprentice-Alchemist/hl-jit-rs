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
        "   stp fp, lr, [sp, #-32]!",
        "   .cfi_adjust_cfa_offset 16",
        "   mov fp, sp",
        "   .cfi_def_cfa_register fp",
        // Move function ptr, stack begin and stack end
        "   mov x9, x0",
        "   mov x10, x1",
        "   mov x11, x2",
        // set up call regs
        "   ldr x0, [x10]",
        "   ldr x1, [x10, 8]",
        "   ldr x2, [x10, 16]",
        "   ldr x3, [x10, 24]",
        "   ldr x5, [x10, 32]",
        "   ldr x5, [x10, 40]",
        "   ldr x6, [x10, 48]",
        "   ldr x7, [x10, 56]",
        // set up fpu call regs,
        "   ldr d0, [x10, 64]",
        "   ldr d1, [x10, 72]",
        "   ldr d2, [x10, 80]",
        "   ldr d3, [x10, 88]",
        "   ldr d4, [x10, 96]",
        "   ldr d5, [x10, 104]",
        "   ldr d6, [x10, 112]",
        "   ldr d7, [x10, 120]",
        // set up stack args
        "   0:  cmp x10, x11",
        "       beq 1f",
        "       sub x10, x10, 8",
        "       str x10, [sp, #-8]!",
        "       b 0b",
        "   1: blr x9",
        "   mov sp, fp",
        "   ldp fp, lr, [sp], #32",
        "	.cfi_def_cfa sp, 0",
        "   ret",
        "   .cfi_endproc",
    );
}

core::arch::global_asm!(
    ".global wrapper_call_impl",
    "wrapper_call_impl:",
    "   .cfi_startproc",
    "   stp fp, lr, [sp, #-32]!",
    "   .cfi_adjust_cfa_offset 16",
    "   mov fp, sp",
    "   .cfi_def_cfa_register fp",
    "   sub sp, sp, 64",
    "   str d0, [sp, 56]",
    "   str d0, [sp, 48]",
    "   str d0, [sp, 40]",
    "   str d0, [sp, 32]",
    "   str d0, [sp, 24]",
    "   str d0, [sp, 16]",
    "   str d0, [sp, 8]",
    "   str d0, [sp, 0]",
    "   str x7, [sp, #-8]!",
    "   str x6, [sp, #-8]!",
    "   str x5, [sp, #-8]!",
    "   str x4, [sp, #-8]!",
    "   str x3, [sp, #-8]!",
    "   str x2, [sp, #-8]!",
    "   str x1, [sp, #-8]!",
    "   str x0, [sp, #-8]!",
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
    "   ldp fp, lr, [sp], #32",
    "	.cfi_def_cfa sp, 0",
    "   ret",
    "   .cfi_endproc",
    wrapper_ptr = sym crate::wrapper_ptr,
);
