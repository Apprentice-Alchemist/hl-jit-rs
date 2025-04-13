cfg_if::cfg_if! (
    if #[cfg(all(target_arch = "x86_64"))] {
        pub const CALL_REGS_COUNT: usize = 6;
        pub const FPU_CALL_REGS: usize = 8;

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
            "   ret",
        );

        core::arch::global_asm!(
            ".global wrapper_call_impl",
            "wrapper_call_impl:",
            "   push rbp",
            "   mov rbp, rsp",
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
            "   mov r10, [rdi]", // ->t
            "   mov r10, [r10 + 8]", // ->fun
            "   mov r10, [r10 + 8]", // ->ret
            "   mov r10d, [r10]", // ->kind
            "   cmp r10d, 5",
            "   jz 0f",
            "   cmp r10d, 6",
            "   jz 0f",
            "   call {wrapper_ptr}",
            "   jmp 1f",
            "   0: call {wrapper_f64}",
            "   1:",
            "   mov rsp, rbp",
            "   pop rbp",
            "   ret",
            wrapper_ptr = sym super::wrapper_ptr,
            wrapper_f64 = sym super::wrapper_f64,
        );
    } else if #[cfg(target_arch = "aarch64")] {
        pub const CALL_REGS_COUNT: usize = 8;
        pub const FPU_CALL_REGS: usize = 8;

        core::arch::global_asm!(
            ".global static_call_impl",
            "static_call_impl:",
            "   stp fp, lr, [sp, #-32]!",
            "   mov fp, sp",
            // Move function ptr, stack begin and stack end
            "   mov x9, x0",
            "   mov x10, x1",
            "   mov x11, x2",
            // set up call regs
            "   ldr x0, [x10]",
            "   ldr x1, [x10, 8]",
            "   ldr x2, [x10, 16]",
            "   ldr x3, [x10, 24]",
            "   ldr x5,  [x10, 32]",
            "   ldr x5,  [x10, 40]",
            "   ldr x6,  [x10, 48]",
            "   ldr x7,  [x10, 56]",
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
            "   ldp fp, lr, [sp], #32",
            "   ret",
        );

        core::arch::global_asm!(
            ".global wrapper_call_impl",
            "wrapper_call_impl:",
            "   stp fp, lr, [sp, #-32]!",
            "   mov fp, sp",
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
            "   0: bl {wrapper_f64}",
            "   1:",
            "   ldp fp, lr, [sp], #32",
            "   ret",
            wrapper_ptr = sym super::wrapper_ptr,
            wrapper_f64 = sym super::wrapper_f64,
        );
    }
);
