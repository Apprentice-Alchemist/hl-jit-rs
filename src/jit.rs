use std::collections::BTreeMap;
use std::error::Error;
use std::mem::offset_of;

use cranelift::codegen::Context;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, DataId, Module};
use cranelift::prelude::*;

use crate::code::{HLType, StrIdx, TypeIdx, UStrIdx};
use crate::sys::{hl_type, hl_type_kind};

#[derive(Default)]
struct Indexes {
    types: BTreeMap<TypeIdx, DataId>,
    ustr: BTreeMap<UStrIdx, DataId>,
    str: BTreeMap<StrIdx, DataId>,
}

struct JitCtx {
    jit_m: JITModule,
    f_ctx: FunctionBuilderContext,
    ctx: Context,
    idxs: Indexes,
    code: crate::code::Code,
}

impl JitCtx {
    pub fn new(code: crate::code::Code) -> JitCtx {
        let jit_b = JITBuilder::new(cranelift::module::default_libcall_names()).unwrap();
        let jit_m = JITModule::new(jit_b);
        let ctx = jit_m.make_context();
        Self {
            jit_m,
            f_ctx: FunctionBuilderContext::new(),
            ctx,
            idxs: Default::default(),
            code,
        }
    }

    pub fn declare(&mut self) -> Result<(), Box<dyn Error>> {
        for idx in 0..self.code.types.len() {
            let id = self.jit_m.declare_anonymous_data(true, false)?;
            self.idxs.types.insert(TypeIdx(idx), id);
        }

        for idx in 0..self.code.strings.len() {
            let id = self.jit_m.declare_anonymous_data(true, false)?;
            self.idxs.str.insert(StrIdx(idx), id);
        }

        Ok(())
    }


    pub fn jit_code(&mut self) -> Result<(), Box<dyn Error>> {
        for (pos, ty) in self.code.types.iter().enumerate() {
            let id = self.jit_m.declare_anonymous_data(true, false)?;
            let mut data = DataDescription::new();
            data.set_align(align_of::<crate::sys::hl_type>() as u64);
            let mut buf: Vec<u8> = vec![0u8; size_of::<crate::sys::hl_type>()];
            let kind_offset = offset_of!(hl_type, kind);
            buf[kind_offset..kind_offset + size_of::<hl_type_kind>()]
                .copy_from_slice(&ty.type_kind().to_ne_bytes());
            data.define(buf.into_boxed_slice());
            match ty {
                HLType::Void => todo!(),
                HLType::UInt8 => todo!(),
                HLType::UInt16 => todo!(),
                HLType::Int32 => todo!(),
                HLType::Int64 => todo!(),
                HLType::Float32 => todo!(),
                HLType::Float64 => todo!(),
                HLType::Boolean => todo!(),
                HLType::Bytes => todo!(),
                HLType::Dynamic => todo!(),
                HLType::Function { args, ret } => todo!(),
                HLType::Object(type_obj) => todo!(),
                HLType::Array => todo!(),
                HLType::Type => todo!(),
                HLType::Reference(type_idx) => todo!(),
                HLType::Virtual { fields } => todo!(),
                HLType::Dynobj => todo!(),
                HLType::Abstract(ustr_idx) => todo!(),
                HLType::Enum {
                    name,
                    global_value,
                    constructs,
                } => todo!(),
                HLType::Null(type_idx) => {
                    let data_id = self.idxs.types.get(type_idx).unwrap();
                    let val = self.jit_m.declare_data_in_data(*data_id, &mut data);
                    let offset = offset_of!(hl_type, __bindgen_anon_1);
                    data.write_data_addr(offset.try_into().unwrap(), val, 0);
                }
                HLType::Method { args, ret } => todo!(),
                HLType::Struct(type_obj) => todo!(),
                HLType::Packed(type_idx) => todo!(),
                HLType::Guid => todo!(),
            }
            // TODO: hl_type_obj etc

            self.jit_m.define_data(id, &data)?;
            // self.idxs.types.insert(TypeIdx::from_usize(pos), id);
        }

        Ok(())
    }
}
