// taken from rustc_codegen_cranelift

use std::collections::HashMap;

use cranelift::codegen::control::ControlPlane;
use cranelift::codegen::ir::{Function, Signature};
use cranelift::codegen::isa::{TargetFrontendConfig, TargetIsa};
use cranelift::codegen::{Context, FinalizedMachReloc};
use cranelift::module::{
    DataDescription, DataId, FuncId, FuncOrDataId, Linkage, Module, ModuleDeclarations,
    ModuleReloc, ModuleResult,
};
use cranelift::object::{ObjectModule, ObjectProduct};
use cranelift_codegen::isa::unwind::UnwindInfo;

use crate::unwind::UnwindContext;

/// A wrapper around a [Module] which adds any defined function to the [UnwindContext].
pub(crate) struct UnwindModule<T> {
    pub(crate) module: T,
    pub unwind_context: UnwindContext,
    function_sizes: HashMap<FuncId, u32>,
}

impl<T: Module> UnwindModule<T> {
    pub(crate) fn new(module: T, pic_eh_frame: bool) -> Self {
        let unwind_context = UnwindContext::new(module.isa(), pic_eh_frame);
        UnwindModule {
            module,
            unwind_context,
            function_sizes: HashMap::new(),
        }
    }

    pub fn add_unwind_info(&mut self, func_id: FuncId, unwind_info: UnwindInfo) {
        self.unwind_context
            .add_unwind_info(func_id, unwind_info, self.module.isa());
    }
}

impl UnwindModule<ObjectModule> {
    pub(crate) fn finish(self) -> ObjectProduct {
        let mut product = self.module.finish();
        self.unwind_context.emit(&mut product);
        product
    }
}

impl UnwindModule<crate::jit::JITModule> {
    pub(crate) fn finalize_definitions(&mut self) {
        self.module.finalize_definitions().unwrap();
        let prev_unwind_context = std::mem::replace(
            &mut self.unwind_context,
            UnwindContext::new(self.module.isa(), false),
        );
        unsafe { prev_unwind_context.register_jit(&self.module) };
    }
}

impl<T: Module> Module for UnwindModule<T> {
    fn isa(&self) -> &dyn TargetIsa {
        self.module.isa()
    }

    fn declarations(&self) -> &ModuleDeclarations {
        self.module.declarations()
    }

    fn get_name(&self, name: &str) -> Option<FuncOrDataId> {
        self.module.get_name(name)
    }

    fn target_config(&self) -> TargetFrontendConfig {
        self.module.target_config()
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &Signature,
    ) -> ModuleResult<FuncId> {
        self.module.declare_function(name, linkage, signature)
    }

    fn declare_anonymous_function(&mut self, signature: &Signature) -> ModuleResult<FuncId> {
        self.module.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        self.module.declare_data(name, linkage, writable, tls)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        self.module.declare_anonymous_data(writable, tls)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
        ctrl_plane: &mut ControlPlane,
    ) -> ModuleResult<()> {
        self.module
            .define_function_with_control_plane(func, ctx, ctrl_plane)?;
        self.unwind_context
            .add_function(func, ctx, self.module.isa());
        self.function_sizes
            .insert(func, ctx.compiled_code().unwrap().code_info().total_size);
        Ok(())
    }

    fn define_function_bytes(
        &mut self,
        func_id: FuncId,
        alignment: u64,
        bytes: &[u8],
        relocs: &[ModuleReloc],
    ) -> ModuleResult<()> {
        self.module
            .define_function_bytes(func_id, alignment, bytes, relocs)?;
        self.function_sizes
            .insert(func_id, bytes.len().try_into().unwrap());
        Ok(())
    }

    fn define_data(&mut self, data_id: DataId, data: &DataDescription) -> ModuleResult<()> {
        self.module.define_data(data_id, data)
    }
}
