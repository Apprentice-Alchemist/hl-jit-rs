use std::ffi::c_void;

pub const CALL_REGS_COUNT: usize = 6;
pub const FPU_CALL_REGS: usize = 8;

// sysv x64_64:
// - call regs: rdi, rsi, rdx, rcx, r8, and r9
// - fpu call regs: xmm0 through xmm7
// - preserved regs: rbx, rsp, rbp, r12, r13, r14, and r15
// - scratch regs: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
// - return in rax:

#[unsafe(naked)]
pub(crate) unsafe extern "sysv64-unwind" fn static_call_impl<T>(
    fun_ptr: *const c_void,
    stack_begin: *const u8,
    stack_end: *const u8,
) -> T {
    core::arch::naked_asm!(
        "	.cfi_startproc",
        "   push rbp",
        "   .cfi_adjust_cfa_offset 8",
        "   .cfi_offset rbp, -16",
        "   mov rbp, rsp",
        "   .cfi_def_cfa_register rbp",
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
        "   2:  cmp rax, r11",
        "       jz 3f",
        "       sub rax, 8",
        "       push [rax]",
        "       jmp 2b",
        "   3: call r10",
        "   mov rsp, rbp",
        "   pop rbp",
        "	.cfi_def_cfa rsp, 8",
        "   ret",
        "   .cfi_endproc"
    );
}

#[unsafe(naked)]
pub(crate) unsafe extern "C" fn wrapper_call_impl() {
    core::arch::naked_asm!(
        "   .cfi_startproc",
        "   push rbp",
        "   .cfi_adjust_cfa_offset 8",
        "   .cfi_offset rbp, -16",
        "   mov rbp, rsp",
        "   .cfi_def_cfa_register rbp",
        "   sub rsp, 64",
        "   movsd [rsp + 56], xmm7",
        "   movsd [rsp + 48], xmm6",
        "   movsd [rsp + 40], xmm5",
        "   movsd [rsp + 32], xmm4",
        "   movsd [rsp + 24], xmm3",
        "   movsd [rsp + 16], xmm2",
        "   movsd [rsp + 8], xmm1",
        "   movsd [rsp + 0], xmm0",
        "   push r9",
        "   push r8",
        "   push rcx",
        "   push rdx",
        "   push rsi",
        "   push rdi",
        "   lea rdx, [rbp + 16]",
        "   mov rsi, rsp",
        "   sub rsp, 16",
        "   mov rcx, rsp",
        "   mov r10, [rdi]", // ->t
        "   mov r10, [r10 + 8]", // ->fun
        "   mov r10, [r10 + 8]", // ->ret
        "   mov r10d, [r10]", // ->kind
        "   cmp r10d, 5",
        "   jz 2f",
        "   cmp r10d, 6",
        "   jz 2f",
        "   call {wrapper_inner}",
        "   jmp 3f",
        "   2: call {wrapper_inner}",
        "   movsd xmm0, [rax]",
        "   3:",
        "   mov rsp, rbp",
        "   pop rbp",
        "	.cfi_def_cfa rsp, 8",
        "   ret",
        "   .cfi_endproc",
        wrapper_inner = sym crate::wrapper_inner,
    );
}
