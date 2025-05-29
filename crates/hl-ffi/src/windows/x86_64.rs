use std::ffi::c_void;

pub const CALL_REGS_COUNT: usize = 4;
pub const FPU_CALL_REGS: usize = 4;

// windows x64_64:
// - call regs: RCX, RDX, R8, and R9
// - fpu call regs: xmm0 through xmm3
// - preserved regs: other
// - scratch regs: RAX, R10, R11, XMM4, and XMM5
// - return in RAX:

#[unsafe(naked)]
pub(crate) unsafe extern "C-unwind" fn static_call_impl<T>(
    fun_ptr: *const c_void,
    stack_top: *const u8,
    stack_bottom: *const u8,
) -> T {
    core::arch::naked_asm!(
        "	.seh_proc frame",
        "   push rbp",
        "   .seh_pushreg rbp",
        "   mov rbp, rsp",
        "   .seh_setframe rbp, 0",
        "   .seh_endprologue",
        // Move function ptr, stack begin and stack end
        "   mov r10, rcx",
        "   mov rax, rdx",
        "   mov r11, r8",
        // set up call regs
        "   mov rcx, [rax]",
        "   mov rdx, [rax + 8]",
        "   mov r8, [rax + 16]",
        "   mov r9, [rax + 24]",
        // set up fpu call regs,
        "   movsd xmm0, [rax + 32]",
        "   movsd xmm1, [rax + 40]",
        "   movsd xmm2, [rax + 48]",
        "   movsd xmm3, [rax + 56]",
        // set up stack args
        "   2:  cmp rax, r11",
        "       jz 3f",
        "       sub rax, 8",
        "       push [rax]",
        "       jmp 2b",
        // set up home area for rcx, rdx, r8 and r9
        "   3:",
        "   sub rsp, 32",
        "   call r10",
        "   mov rsp, rbp",
        "   pop rbp",
        "   ret",
        "   .seh_endproc"
    );
}

core::arch::global_asm!(
    ".global wrapper_call_impl",
    "wrapper_call_impl:",
    "	.seh_proc frame",
    "   push rbp",
    "   .seh_pushreg rbp",
    "   mov rbp, rsp",
    "   .seh_setframe rbp, 0",
    "   .seh_endprologue",
    "   sub rsp, 32",
    "   movsd [rsp + 24], xmm3",
    "   movsd [rsp + 16], xmm2",
    "   movsd [rsp + 8], xmm1",
    "   movsd [rsp + 0], xmm0",
    "   push r9",
    "   push r8",
    "   push rdx",
    "   push rcx",
    "   mov rdx, rsp",
    "   lea r8, [rbp + 48]",
    "   sub rsp, 16",
    "   mov r9, rsp",
    "   sub rsp, 32",
    "   mov r10, [rcx]", // ->t
    "   mov r10, [r10 + 8]", // ->fun
    "   mov r10, [r10 + 8]", // ->ret
    "   mov r10d, [r10]", // ->kind
    "   cmp r10d, 5",
    "   jz 0f",
    "   cmp r10d, 6",
    "   jz 0f",
    "   call {wrapper_inner}",
    "   jmp 1f",
    "   0: call {wrapper_inner}",
    "   movsd xmm0, [rax]",
    "   1:",
    "   mov rsp, rbp",
    "   pop rbp",
    "   ret",
    "   .seh_endproc",
    wrapper_inner = sym crate::wrapper_inner,
);
