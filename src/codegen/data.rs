use std::{error::Error, ffi::c_int, mem::offset_of};

use cranelift::{
    module::{DataDescription, DataId, Linkage, Module, ModuleError},
    prelude::{AbiParam, Signature},
};

use crate::{
    code::{Code, HLType, TypeFun, TypeIdx, TypeVirtual, UStrIdx},
    sys::{hl_obj_field, hl_type, hl_type_fun, hl_type_kind, hl_type_virtual},
};

use super::Indexes;

pub fn declare(m: &mut dyn Module, code: &Code, idxs: &mut Indexes) -> Result<(), Box<dyn Error>> {
    for idx in 0..code.types.len() {
        let id = m.declare_anonymous_data(true, false)?;
        idxs.types.insert(TypeIdx(idx), id);
    }

    for idx in 0..code.strings.len() {
        let id = m.declare_anonymous_data(true, false)?;
        idxs.ustr.insert(UStrIdx(idx), id);
    }

    Ok(())
}

pub fn define_strings(
    m: &mut dyn Module,
    code: &Code,
    idxs: &Indexes,
) -> Result<(), Box<dyn Error>> {
    for (idx, s) in code.strings.iter().enumerate() {
        let id = idxs.ustr[&UStrIdx(idx)];
        let mut data = DataDescription::new();
        let mut buf = Vec::new();
        for c in s.encode_utf16() {
            buf.extend_from_slice(&c.to_ne_bytes());
        }
        data.define(buf.into_boxed_slice());
        m.define_data(id, &data)?;
    }

    Ok(())
}

fn write_data(m: &dyn Module, data: &mut DataDescription, data_id: DataId, offset: usize) {
    let val = m.declare_data_in_data(data_id, data);
    data.write_data_addr(offset.try_into().unwrap(), val, 0);
}

fn build_type_arr(
    m: &mut dyn Module,
    idxs: &Indexes,
    arr: &[TypeIdx],
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let size = size_of::<*mut hl_type>() * arr.len();
    let align = align_of::<*mut hl_type>();
    let mut data = DataDescription::new();
    data.set_align(align as u64);
    data.define_zeroinit(size);
    for (pos, ty) in arr.iter().enumerate() {
        write_data(
            m,
            &mut data,
            idxs.types[ty],
            size_of::<*mut hl_type>() * pos,
        );
    }
    m.define_data(id, &data)?;
    Ok(id)
}

fn build_type_fun(m: &mut dyn Module, idxs: &Indexes, f: &TypeFun) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let mut buf: Vec<u8> = vec![0u8; size_of::<hl_type_fun>()];
    let nargs: std::ffi::c_int = f.args.len().try_into().unwrap();
    buf[offset_of!(hl_type_fun, nargs)
        ..offset_of!(hl_type_fun, nargs) + size_of::<std::ffi::c_int>()]
        .copy_from_slice(&nargs.to_ne_bytes());
    let mut data = DataDescription::new();
    data.set_align(align_of::<hl_type_fun>() as u64);
    data.define(buf.into_boxed_slice());
    write_data(
        m,
        &mut data,
        idxs.types[&f.ret],
        offset_of!(hl_type_fun, ret),
    );
    let arr_id = build_type_arr(m, idxs, &f.args)?;
    write_data(m, &mut data, arr_id, offset_of!(hl_type_fun, args));
    m.define_data(id, &data)?;
    Ok(id)
}

fn build_field_arr(
    m: &mut dyn Module,
    idxs: &Indexes,
    fields: &[(UStrIdx, TypeIdx)],
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let size = size_of::<*mut hl_obj_field>() * fields.len();
    let align = align_of::<*mut hl_obj_field>();
    let mut data = DataDescription::new();
    data.set_align(align as u64);
    data.define_zeroinit(size);
    for (pos, (ustr, ty)) in fields.iter().enumerate() {
        write_data(
            m,
            &mut data,
            idxs.ustr[ustr],
            size_of::<*mut hl_obj_field>() * pos + offset_of!(hl_obj_field, name),
        );
        write_data(
            m,
            &mut data,
            idxs.types[ty],
            size_of::<*mut hl_obj_field>() * pos + offset_of!(hl_obj_field, t),
        );

        // TODO: hashed_name
    }
    m.define_data(id, &data)?;
    Ok(id)
}

fn build_type_virtual(
    m: &mut dyn Module,
    idxs: &Indexes,
    virt: &TypeVirtual,
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let fields_id = build_field_arr(m, idxs, &virt.fields)?;
    let size = size_of::<*mut hl_type_virtual>();
    let align = align_of::<*mut hl_type_virtual>();
    let mut data = DataDescription::new();
    data.set_align(align as u64);
    let mut buf = vec![0u8; size_of::<hl_type_virtual>()];
    let nfields: c_int = virt.fields.len() as c_int;
    buf[offset_of!(hl_type_virtual, nfields)
        ..offset_of!(hl_type_virtual, nfields) + size_of::<c_int>()]
        .copy_from_slice(&nfields.to_ne_bytes());
    write_data(m, &mut data, fields_id, offset_of!(hl_type_virtual, fields));
    m.define_data(id, &data)?;
    Ok(id)
}

pub fn define_types(m: &mut dyn Module, code: &Code, idxs: &Indexes) -> Result<(), Box<dyn Error>> {
    for (pos, ty) in code.types.iter().enumerate() {
        let id = m.declare_anonymous_data(true, false)?;
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
            HLType::Function(fun) => Some(build_type_fun(m, &idxs, fun)?),
            HLType::Object(type_obj) => todo!(),
            HLType::Array => None,
            HLType::Type => None,
            HLType::Reference(type_idx) => Some(idxs.types[type_idx]),
            HLType::Virtual(virt) => Some(build_type_virtual(m, &idxs, virt)?),
            HLType::Dynobj => None,
            HLType::Abstract(ustr_idx) => Some(idxs.ustr[ustr_idx]),
            HLType::Enum(type_enum) => todo!(),
            HLType::Null(type_idx) => Some(idxs.types[type_idx]),
            HLType::Method(fun) => Some(build_type_fun(m, &idxs, fun)?),
            HLType::Struct(type_obj) => todo!(),
            HLType::Packed(type_idx) => Some(idxs.types[type_idx]),
            HLType::Guid => None,
        };

        if let Some(data_id) = data_id {
            write_data(
                &m,
                &mut data,
                data_id,
                offset_of!(hl_type, __bindgen_anon_1),
            )
        }
        m.define_data(id, &data)?;
    }
    Ok(())
}
