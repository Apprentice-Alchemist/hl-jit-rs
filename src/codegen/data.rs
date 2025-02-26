use std::{error::Error, ffi::c_int, mem::offset_of};

use cranelift::{
    module::{DataDescription, DataId, Linkage, Module, ModuleError},
    prelude::{AbiParam, Signature},
};

use crate::{
    code::{Code, GlobalIdx, HLType, TypeEnum, TypeFun, TypeIdx, TypeObj, TypeVirtual, UStrIdx},
    sys::{
        hl_enum_construct, hl_obj_field, hl_obj_proto, hl_type, hl_type_enum, hl_type_fun,
        hl_type_kind, hl_type_obj, hl_type_virtual,
    },
};

use super::Indexes;

pub fn declare(m: &mut dyn Module, code: &Code, idxs: &mut Indexes) -> Result<(), Box<dyn Error>> {
    for idx in 0..code.types.len() {
        let id = m.declare_data(&format!("type{idx}"), Linkage::Local, true, false)?;
        idxs.types.insert(TypeIdx(idx), id);
    }

    for idx in 0..code.strings.len() {
        let id = m.declare_data(&format!("str{idx}"), Linkage::Local, true, false)?;
        idxs.ustr.insert(UStrIdx(idx), id);
    }

    for idx in 0..code.globals.len() {
        let id = m.declare_data(&format!("global{idx}"), Linkage::Local, true, false)?;
        idxs.globals.insert(GlobalIdx(idx), id);
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
    idxs: &mut Indexes,
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

        idxs.hash_locations.entry(*ustr).or_default().push((
            id,
            size_of::<*mut hl_obj_field>() * pos + offset_of!(hl_obj_field, hashed_name),
        ));
    }
    m.define_data(id, &data)?;
    Ok(id)
}
fn build_proto_arr(
    m: &mut dyn Module,
    idxs: &mut Indexes,
    fields: &[(UStrIdx, usize, isize)],
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let align = align_of::<hl_obj_proto>();
    let mut data = DataDescription::new();
    data.set_align(align as u64);
    {
        let mut buf = vec![0u8; size_of::<hl_obj_proto>() * fields.len()];
        for (pos, chunk) in buf.chunks_mut(size_of::<hl_obj_proto>()).enumerate() {
            let (_, findex, pindex) = fields[pos];
            let findex: c_int = findex as c_int;
            let pindex: c_int = pindex as c_int;
            chunk[offset_of!(hl_obj_proto, findex)
                ..offset_of!(hl_obj_proto, findex) + size_of::<c_int>()]
                .copy_from_slice(&findex.to_ne_bytes());
            chunk[offset_of!(hl_obj_proto, pindex)
                ..offset_of!(hl_obj_proto, pindex) + size_of::<c_int>()]
                .copy_from_slice(&pindex.to_ne_bytes());
        }
        data.define(buf.into_boxed_slice());
    }
    for (pos, (ustr, findex, pindex)) in fields.iter().enumerate() {
        write_data(
            m,
            &mut data,
            idxs.ustr[ustr],
            size_of::<*mut hl_obj_field>() * pos + offset_of!(hl_obj_proto, name),
        );
        idxs.hash_locations.entry(*ustr).or_default().push((
            id,
            size_of::<*mut hl_obj_proto>() * pos + offset_of!(hl_obj_proto, hashed_name),
        ));
    }
    m.define_data(id, &data)?;
    Ok(id)
}

fn build_type_virtual(
    m: &mut dyn Module,
    idxs: &mut Indexes,
    virt: &TypeVirtual,
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let fields_id = build_field_arr(m, idxs, &virt.fields)?;
    let align = align_of::<hl_type_virtual>();
    let mut data = DataDescription::new();
    data.set_align(align as u64);
    let mut buf = vec![0u8; size_of::<hl_type_virtual>()];
    let nfields: c_int = virt.fields.len() as c_int;
    buf[offset_of!(hl_type_virtual, nfields)
        ..offset_of!(hl_type_virtual, nfields) + size_of::<c_int>()]
        .copy_from_slice(&nfields.to_ne_bytes());
    data.define(buf.into_boxed_slice());
    write_data(m, &mut data, fields_id, offset_of!(hl_type_virtual, fields));
    m.define_data(id, &data)?;
    Ok(id)
}

fn build_type_obj(
    m: &mut dyn Module,
    idxs: &mut Indexes,
    TypeObj {
        name,
        super_,
        global,
        fields,
        protos,
        bindings,
    }: &TypeObj,
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let mut data = DataDescription::new();
    {
        let mut buf = vec![0u8; size_of::<hl_type_obj>()];
        let nfields: c_int = fields.len() as c_int;
        let nproto: c_int = protos.len() as c_int;
        let nbindings: c_int = bindings.len() as c_int;
        buf[offset_of!(hl_type_obj, nfields)
            ..offset_of!(hl_type_obj, nfields) + size_of::<c_int>()]
            .copy_from_slice(&nfields.to_ne_bytes());
        buf[offset_of!(hl_type_obj, nproto)..offset_of!(hl_type_obj, nproto) + size_of::<c_int>()]
            .copy_from_slice(&nproto.to_ne_bytes());
        buf[offset_of!(hl_type_obj, nbindings)
            ..offset_of!(hl_type_obj, nbindings) + size_of::<c_int>()]
            .copy_from_slice(&nbindings.to_ne_bytes());
        data.define(buf.into_boxed_slice());
    }
    write_data(m, &mut data, idxs.ustr[name], offset_of!(hl_type_obj, name));
    if let Some(super_) = super_ {
        write_data(
            m,
            &mut data,
            idxs.types[super_],
            offset_of!(hl_type_obj, super_),
        );
    }
    let fields_id = build_field_arr(m, idxs, fields).unwrap();
    write_data(m, &mut data, fields_id, offset_of!(hl_type_obj, fields));
    let proto_id = build_proto_arr(m, idxs, protos).unwrap();
    write_data(m, &mut data, proto_id, offset_of!(hl_type_obj, proto));
    let bindings_id = {
        let bindings_id = m.declare_anonymous_data(true, false).unwrap();
        let mut data = DataDescription::new();
        let mut buf = vec![0u8; size_of::<c_int>() * 2 * bindings.len()];
        for (pos, chunk) in buf.chunks_mut(size_of::<c_int>() * 2).enumerate() {
            let (fid, fidx) = bindings[pos];
            let fid = fid as c_int;
            let fidx = fidx as c_int;
            chunk[0..size_of::<c_int>()].copy_from_slice(&fid.to_ne_bytes());
            chunk[size_of::<c_int>()..].copy_from_slice(&fidx.to_ne_bytes());
        }
        data.define(buf.into_boxed_slice());
        m.define_data(bindings_id, &data);
        bindings_id
    };
    write_data(m, &mut data, bindings_id, offset_of!(hl_type_obj, bindings));
    if let Some(global) = global {
        write_data(m, &mut data, idxs.globals[global], offset_of!(hl_type_obj, global_value));
    }
    m.define_data(id, &data).unwrap();
    Ok(id)
}

fn build_enum_constructs(
    m: &mut dyn Module,
    idxs: &mut Indexes,
    constructs: &[(UStrIdx, Vec<TypeIdx>)],
) -> DataId {
    let id = m.declare_anonymous_data(true, false).unwrap();
    let mut data = DataDescription::new();
    {
        let mut buf = vec![0u8; size_of::<hl_enum_construct>() * constructs.len()];

        for (pos, chunk) in buf.chunks_mut(size_of::<hl_enum_construct>()).enumerate() {
            let nparams = constructs[pos].1.len() as c_int;
            chunk[offset_of!(hl_enum_construct, nparams)
                ..offset_of!(hl_enum_construct, nparams) + size_of::<c_int>()]
                .copy_from_slice(&nparams.to_ne_bytes());
        }

        data.define(buf.into_boxed_slice());
    }
    for (pos, (name, types)) in constructs.iter().enumerate() {
        write_data(
            m,
            &mut data,
            idxs.ustr[name],
            pos * size_of::<hl_enum_construct>() + offset_of!(hl_enum_construct, name),
        );
        let id = build_type_arr(m, idxs, &types).unwrap();
        write_data(
            m,
            &mut data,
            id,
            pos * size_of::<hl_enum_construct>() + offset_of!(hl_enum_construct, params),
        );
    }
    id
}

fn build_type_enum(
    m: &mut dyn Module,
    idxs: &mut Indexes,
    e: &TypeEnum,
) -> Result<DataId, ModuleError> {
    let id = m.declare_anonymous_data(true, false)?;
    let mut data = DataDescription::new();
    {
        let mut buf = vec![0u8; size_of::<hl_type_enum>()];
        let nconstructs = e.constructs.len() as c_int;
        buf[offset_of!(hl_type_enum, nconstructs)
            ..offset_of!(hl_type_enum, nconstructs) + size_of::<c_int>()]
            .copy_from_slice(&nconstructs.to_ne_bytes());
        data.define(buf.into_boxed_slice());
    }
    write_data(
        m,
        &mut data,
        idxs.ustr[&e.name],
        offset_of!(hl_type_enum, name),
    );
    let constructs_id = build_enum_constructs(m, idxs, &e.constructs);
    write_data(
        m,
        &mut data,
        constructs_id,
        offset_of!(hl_type_enum, constructs),
    );
    if let Some(global_value) = e.global_value {
        write_data(
            m,
            &mut data,
            idxs.globals[&global_value],
            offset_of!(hl_type_enum, global_value),
        );
    }
    Ok(id)
}

pub fn define_types(
    m: &mut dyn Module,
    code: &Code,
    idxs: &mut Indexes,
) -> Result<(), Box<dyn Error>> {
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
            HLType::Function(fun) => Some(build_type_fun(m, idxs, fun)?),
            HLType::Object(type_obj) => Some(build_type_obj(m, idxs, type_obj)?),
            HLType::Array => None,
            HLType::Type => None,
            HLType::Reference(type_idx) => Some(idxs.types[type_idx]),
            HLType::Virtual(virt) => Some(build_type_virtual(m, idxs, virt)?),
            HLType::Dynobj => None,
            HLType::Abstract(ustr_idx) => Some(idxs.ustr[ustr_idx]),
            HLType::Enum(type_enum) => Some(build_type_enum(m, idxs, type_enum)?),
            HLType::Null(type_idx) => Some(idxs.types[type_idx]),
            HLType::Method(fun) => Some(build_type_fun(m, &idxs, fun)?),
            HLType::Struct(type_obj) => Some(build_type_obj(m, idxs, type_obj)?),
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
        println!("type{}", pos);
        m.define_data(id, &data)?;
    }
    Ok(())
}

pub fn define_globals(m: &mut dyn Module, code: &Code, idxs: &Indexes) {
    for (gidx, id) in &idxs.globals {
        let mut data = DataDescription::new();
        data.define_zeroinit(8);
        m.define_data(*id, &data).unwrap();
    }
}
