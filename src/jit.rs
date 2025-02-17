use std::collections::BTreeMap;
use std::error::Error;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleError};
use cranelift::prelude::*;

use crate::code::{FunIdx, HLType, StrIdx, TypeIdx, UStrIdx};
use crate::sys::{hl_type, hl_type_fun, hl_type_kind};

#[derive(Default)]
struct Indexes {
    types: BTreeMap<TypeIdx, DataId>,
    ustr: BTreeMap<UStrIdx, DataId>,
    fn_map: BTreeMap<FunIdx, FuncId>,
}

pub fn compile_module(code: crate::code::Code) -> (JITModule, FuncId) {
    let jit_b = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
    let jit_m = JITModule::new(jit_b);
    let ctx = JitCtx::new(jit_m, code);
    let (mut m, entrypoint) = ctx.finish();
    m.finalize_definitions();
    (m, entrypoint)
}

pub struct JitCtx<M: Module> {
    m: M,
    f_ctx: FunctionBuilderContext,
    ctx: Context,
    idxs: Indexes,
    code: crate::code::Code,
}

impl<M: Module> JitCtx<M> {
    pub fn new(m: M, code: crate::code::Code) -> Self {
        let ctx = m.make_context();
        Self {
            m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
            code,
        }
    }

    pub fn finish(self) -> (M, FuncId) {
        (self.m, self.idxs.fn_map[&self.code.entrypoint])
    }

    fn fill_signature(&self, sig: &mut Signature, ty: TypeIdx) {
        let (args, ret) = match self.code.get_type(ty) {
            HLType::Function { args, ret } => (args, ret),
            HLType::Method { args, ret } => (args, ret),
            _ => panic!(),
        };
        sig.params.extend(
            args.iter()
                .map(|idx| AbiParam::new(self.code.get_type(*idx).cranelift_type())),
        );
        sig.returns
            .push(AbiParam::new(self.code.get_type(*ret).cranelift_type()));
    }

    pub fn declare(&mut self) -> Result<(), Box<dyn Error>> {
        for idx in 0..self.code.types.len() {
            let id = self.m.declare_anonymous_data(true, false)?;
            self.idxs.types.insert(TypeIdx(idx), id);
        }

        for idx in 0..self.code.strings.len() {
            let id = self.m.declare_anonymous_data(true, false)?;
            self.idxs.ustr.insert(UStrIdx(idx), id);
        }

        let mut signature: Signature = self.m.make_signature();
        for f in &self.code.functions {
            self.m.clear_signature(&mut signature);
            self.fill_signature(&mut signature, f.ty);
            let id = self.m.declare_anonymous_function(&signature)?;
            self.idxs.fn_map.insert(f.idx, id);
        }
        for (lib, name, ty, idx) in &self.code.natives {
            let lib = match self.code.strings[lib.0].as_str() {
                "std\0" => "hl",
                lib => &lib[0..(lib.len() - 1)],
            };
            let name = &self.code.strings[name.0];
            self.m.clear_signature(&mut signature);
            let id =
                self.m
                    .declare_function(&format!("{lib}_{name}"), Linkage::Import, &signature)?;
            self.idxs.fn_map.insert(*idx, id);
        }

        Ok(())
    }

    pub fn define_strings(&mut self) -> Result<(), Box<dyn Error>> {
        for (idx, s) in self.code.strings.iter().enumerate() {
            let id = self.idxs.ustr[&UStrIdx(idx)];
            let mut data = DataDescription::new();
            let mut buf = Vec::new();
            for c in s.encode_utf16() {
                buf.extend_from_slice(&c.to_ne_bytes());
            }
            data.define(buf.into_boxed_slice());
            self.m.define_data(id, &data)?;
        }

        Ok(())
    }

    fn write_data(m: &M, data: &mut DataDescription, data_id: DataId, offset: usize) {
        let val = m.declare_data_in_data(data_id, data);
        data.write_data_addr(offset.try_into().unwrap(), val, 0);
    }

    fn build_type_arr(m: &mut M, idxs: &Indexes, arr: &[TypeIdx]) -> Result<DataId, ModuleError> {
        let id = m.declare_anonymous_data(true, false)?;
        let size = size_of::<*mut hl_type>() * arr.len();
        let align = align_of::<*mut hl_type>();
        let mut data = DataDescription::new();
        data.set_align(align as u64);
        data.define_zeroinit(size);
        for (pos, ty) in arr.iter().enumerate() {
            Self::write_data(
                m,
                &mut data,
                idxs.types[ty],
                size_of::<*mut hl_type>() * pos,
            );
        }
        Ok(id)
    }

    fn build_type_fun(
        m: &mut M,
        idxs: &Indexes,
        args: &[TypeIdx],
        ret: &TypeIdx,
    ) -> Result<DataId, ModuleError> {
        let id = m.declare_anonymous_data(true, false)?;
        let mut buf: Vec<u8> = vec![0u8; size_of::<hl_type_fun>()];
        let nargs: std::ffi::c_int = args.len().try_into().unwrap();
        buf[offset_of!(hl_type_fun, nargs)
            ..offset_of!(hl_type_fun, nargs) + size_of::<std::ffi::c_int>()]
            .copy_from_slice(&nargs.to_ne_bytes());
        let mut data = DataDescription::new();
        data.set_align(align_of::<hl_type_fun>() as u64);
        data.define(buf.into_boxed_slice());
        Self::write_data(m, &mut data, idxs.types[ret], offset_of!(hl_type_fun, ret));
        let arr_id = Self::build_type_arr(m, idxs, args)?;
        Self::write_data(m, &mut data, arr_id, offset_of!(hl_type_fun, args));
        Ok(id)
    }

    pub fn define_types(&mut self) -> Result<(), Box<dyn Error>> {
        for (pos, ty) in self.code.types.iter().enumerate() {
            let id = self.m.declare_anonymous_data(true, false)?;
            let mut data = DataDescription::new();
            data.set_align(align_of::<crate::sys::hl_type>() as u64);
            let mut buf: Vec<u8> = vec![0u8; size_of::<crate::sys::hl_type>()];
            let kind_offset = offset_of!(hl_type, kind);
            buf[kind_offset..kind_offset + size_of::<hl_type_kind>()]
                .copy_from_slice(&ty.type_kind().to_ne_bytes());
            data.define(buf.into_boxed_slice());
            let data_id = match ty {
                HLType::Void => None,
                HLType::UInt8 => None,
                HLType::UInt16 => None,
                HLType::Int32 => None,
                HLType::Int64 => None,
                HLType::Float32 => None,
                HLType::Float64 => None,
                HLType::Boolean => None,
                HLType::Bytes => None,
                HLType::Dynamic => None,
                HLType::Function { args, ret } => {
                    Some(Self::build_type_fun(&mut self.m, &self.idxs, args, ret)?)
                }
                HLType::Object(type_obj) => todo!(),
                HLType::Array => None,
                HLType::Type => None,
                HLType::Reference(type_idx) => Some(self.idxs.types[type_idx]),
                HLType::Virtual { fields } => todo!(),
                HLType::Dynobj => None,
                HLType::Abstract(ustr_idx) => Some(self.idxs.ustr[ustr_idx]),
                HLType::Enum {
                    name,
                    global_value,
                    constructs,
                } => todo!(),
                HLType::Null(type_idx) => Some(self.idxs.types[type_idx]),
                HLType::Method { args, ret } => todo!(),
                HLType::Struct(type_obj) => todo!(),
                HLType::Packed(type_idx) => Some(self.idxs.types[type_idx]),
                HLType::Guid => None,
            };

            if let Some(data_id) = data_id {
                Self::write_data(
                    &self.m,
                    &mut data,
                    data_id,
                    offset_of!(hl_type, __bindgen_anon_1),
                )
            }
            self.m.define_data(id, &data)?;
        }
        Ok(())
    }
}
