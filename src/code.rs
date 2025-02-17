use std::ops::Index;
#[allow(unused)]
use std::{
    fs::File,
    io::{self, BufReader, ErrorKind, Read},
    path::Path,
};

use crate::opcode::OpCode;

pub struct Reader(BufReader<File>);
impl Reader {
    pub fn open_file(path: impl AsRef<Path>) -> io::Result<Reader> {
        let file = std::fs::OpenOptions::new().read(true).open(path)?;
        Ok(Self(BufReader::new(file)))
    }

    pub fn byte(&mut self) -> io::Result<u8> {
        let mut buf = [0u8];
        self.0.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn idx(&mut self) -> io::Result<isize> {
        let b = self.byte()? as i32;
        if b & 0x80 == 0 {
            return Ok((b & 0x7F) as isize);
        }

        if b & 0x40 == 0 {
            let v: isize = (self.byte()? as i32 | (b & 31) << 8) as isize;
            return Ok(if b & 0x20 == 0 { v } else { -v });
        }

        let c = self.byte()? as i32;
        let d = self.byte()? as i32;
        let e = self.byte()? as i32;
        let v: isize = (((b & 31) << 24) | (c << 16) | (d << 8) | e) as isize;
        Ok(if b & 0x20 == 0 { v } else { -v })
    }

    fn udx(&mut self) -> io::Result<usize> {
        let i = self.idx()?;
        if i.is_negative() {
            return Err(io::Error::new(ErrorKind::InvalidData, "negative index"));
        }
        Ok(i as usize)
    }

    fn int(&mut self) -> io::Result<i32> {
        let mut buf = [0u8; 4];
        self.0.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    fn f64(&mut self) -> io::Result<f64> {
        let mut buf = [0u8; 8];
        self.0.read_exact(&mut buf)?;
        Ok(f64::from_le_bytes(buf))
    }

    pub fn vec<T>(
        &mut self,
        count: usize,
        read_fn: impl Fn(&mut Reader) -> io::Result<T>,
    ) -> io::Result<Vec<T>> {
        let mut v = Vec::with_capacity(count);
        for _ in 0..count {
            v.push(read_fn(self)?)
        }
        Ok(v)
    }
}

fn read_strings(r: &mut Reader, nstrings: usize) -> io::Result<Vec<String>> {
    let size = r.int()? as usize;
    let mut buf = Vec::new();
    buf.resize(size, 0);
    r.0.read_exact(&mut buf)?;

    let mut cursor = std::io::Cursor::new(buf);

    let mut string_vec = Vec::with_capacity(nstrings);
    println!("string count: {}", nstrings);
    for _ in 0..nstrings {
        let sz = r.udx()?;
        // include null terminator
        let mut buf = vec![0u8; sz + 1];
        cursor.read_exact(&mut buf)?;
        let Ok(s) = String::from_utf8(buf) else {
            return Err(std::io::Error::new(
                ErrorKind::InvalidData,
                "invalid string",
            ));
        };
        string_vec.push(s);
    }

    Ok(string_vec)
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UStrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunIdx(pub usize);

fn ty(r: &mut Reader) -> io::Result<TypeIdx> {
    Ok(TypeIdx(r.idx()? as usize))
}

fn s(r: &mut Reader) -> io::Result<StrIdx> {
    Ok(StrIdx(r.idx()? as usize))
}

fn us(r: &mut Reader) -> io::Result<UStrIdx> {
    Ok(UStrIdx(r.idx()? as usize))
}

#[derive(Clone)]
pub struct TypeObj {
    name: UStrIdx,
    super_: isize,
    global: usize,
    fields: Vec<(UStrIdx, TypeIdx)>,
    protos: Vec<(UStrIdx, usize, isize)>,
    bindings: Vec<(usize, usize)>,
}

#[derive(Clone)]
pub enum HLType {
    Void,
    UInt8,
    UInt16,
    Int32,
    Int64,
    Float32,
    Float64,
    Boolean,
    Bytes,
    Dynamic,
    Function {
        args: Vec<TypeIdx>,
        ret: TypeIdx,
    },
    Object(TypeObj),
    Array,
    Type,
    Reference(TypeIdx),
    Virtual {
        fields: Vec<(UStrIdx, TypeIdx)>,
    },
    Dynobj,
    Abstract(UStrIdx),
    Enum {
        name: UStrIdx,
        global_value: usize,
        constructs: Vec<(UStrIdx, Vec<TypeIdx>)>,
    },
    Null(TypeIdx),
    Method {
        args: Vec<TypeIdx>,
        ret: TypeIdx,
    },
    Struct(TypeObj),
    Packed(TypeIdx),
    Guid,
}

impl HLType {
    pub fn type_kind(&self) -> crate::sys::hl_type_kind {
        use crate::sys::*;
        match self {
            Self::Void => hl_type_kind_HVOID,
            Self::UInt8 => hl_type_kind_HUI8,
            Self::UInt16 => hl_type_kind_HUI16,
            _ => todo!(),
        }
    }

    pub fn cranelift_type(&self) -> cranelift::prelude::Type {
        use cranelift::prelude::types;
        match self {
            Self::Void => types::INVALID,
            Self::UInt8 => types::I8,
            Self::UInt16 => types::I16,
            Self::Int32 => types::I32,
            Self::Int64 => types::I64,
            Self::Float32 => types::F32,
            Self::Float64 => types::F64,
            Self::Boolean => types::I32,
            Self::Bytes => types::I64,
            Self::Dynamic => types::I64,
            Self::Function { args: _, ret: _ } => types::I64,
            Self::Object(_) => types::I64,
            Self::Array => types::I64,
            Self::Type => types::I64,
            Self::Reference(_) => types::I64,
            Self::Virtual { fields: _ } => types::I64,
            Self::Dynobj => types::I64,
            Self::Abstract(_) => types::I64,
            Self::Enum {
                constructs: _,
                global_value: _,
                name: _,
            } => types::I64,
            Self::Null(_) => types::I64,
            Self::Method { args: _, ret: _ } => types::I64,
            Self::Struct(_) => types::I64,
            Self::Packed(_) => types::I64,
            Self::Guid => types::I64,
        }
    }
}

fn read_obj(r: &mut Reader) -> io::Result<TypeObj> {
    let name = us(r)?;
    let super_ = r.idx()?;
    let global = r.udx()?;
    let nfields = r.udx()?;
    let nproto = r.udx()?;
    let nbindings = r.udx()?;

    let fields = r.vec(nfields, |r| {
        let name = us(r)?;
        let ty = ty(r)?;
        Ok((name, ty))
    })?;

    let protos = r.vec(nproto, |r| {
        let name = us(r)?;
        let fidx = r.udx()?;
        let pidx = r.idx()?;
        Ok((name, fidx, pidx))
    })?;

    let bindings = r.vec(nbindings, |r| Ok((r.udx()?, r.udx()?)))?;

    Ok(TypeObj {
        name,
        super_,
        global,
        fields,
        protos,
        bindings,
    })
}

fn read_type(r: &mut Reader) -> io::Result<HLType> {
    let kind = r.byte()?;
    let t = match kind as u32 {
        crate::sys::hl_type_kind_HVOID => HLType::Void,
        crate::sys::hl_type_kind_HUI8 => HLType::UInt8,
        crate::sys::hl_type_kind_HUI16 => HLType::UInt16,
        crate::sys::hl_type_kind_HI32 => HLType::Int32,
        crate::sys::hl_type_kind_HI64 => HLType::Int64,
        crate::sys::hl_type_kind_HF32 => HLType::Float32,
        crate::sys::hl_type_kind_HF64 => HLType::Float64,
        crate::sys::hl_type_kind_HBOOL => HLType::Boolean,
        crate::sys::hl_type_kind_HBYTES => HLType::Bytes,
        crate::sys::hl_type_kind_HDYN => HLType::Dynamic,
        crate::sys::hl_type_kind_HFUN => {
            let nargs = r.byte()? as usize;
            let args = r.vec(nargs, ty)?;
            let ret = ty(r)?;
            HLType::Function { args, ret }
        }
        crate::sys::hl_type_kind_HOBJ => HLType::Object(read_obj(r)?),
        crate::sys::hl_type_kind_HARRAY => HLType::Array,
        crate::sys::hl_type_kind_HTYPE => HLType::Type,
        crate::sys::hl_type_kind_HREF => HLType::Reference(ty(r)?),
        crate::sys::hl_type_kind_HVIRTUAL => {
            let nfields = r.udx()?;
            let fields = r.vec(nfields, |r| {
                let name = us(r)?;
                let ty_ = ty(r)?;
                Ok((name, ty_))
            })?;
            HLType::Virtual { fields }
        }
        crate::sys::hl_type_kind_HDYNOBJ => HLType::Dynobj,
        crate::sys::hl_type_kind_HABSTRACT => HLType::Abstract(us(r)?),
        crate::sys::hl_type_kind_HENUM => {
            let name: UStrIdx = us(r)?;
            let global_value = r.udx()?;
            let nconstructs = r.udx()?;
            let constructs = r.vec(nconstructs, |r| {
                let name = us(r)?;
                let nparams = r.udx()?;
                let params = r.vec(nparams, ty)?;
                Ok((name, params))
            })?;
            HLType::Enum {
                name,
                global_value,
                constructs,
            }
        }
        crate::sys::hl_type_kind_HNULL => HLType::Null(ty(r)?),
        crate::sys::hl_type_kind_HMETHOD => {
            let nargs = r.byte()? as usize;
            let args = r.vec(nargs, ty)?;
            let ret = ty(r)?;
            HLType::Method { args, ret }
        }
        crate::sys::hl_type_kind_HSTRUCT => HLType::Struct(read_obj(r)?),
        crate::sys::hl_type_kind_HPACKED => HLType::Packed(ty(r)?),
        // crate::sys::hl_type_kind_HGUID => HLType::Guid,
        kind => {
            println!("{}", kind);
            return Err(io::Error::new(ErrorKind::InvalidData, "invalid type kind"));
        }
    };
    Ok(t)
}

pub struct HLFunction {
    pub ty: TypeIdx,
    pub idx: FunIdx,
    pub regs: Vec<TypeIdx>,
    pub opcodes: Vec<OpCode>,
}

pub struct Code {
    pub version: u8,
    pub flags: usize,
    pub ints: Vec<i32>,
    pub floats: Vec<f64>,
    pub strings: Vec<String>,
    pub bytes: Option<(Vec<u8>, Vec<usize>)>,
    pub debugfiles: Option<Vec<String>>,
    pub types: Vec<HLType>,
    pub globals: Vec<isize>,
    pub natives: Vec<(StrIdx, StrIdx, TypeIdx, FunIdx)>,
    pub functions: Vec<HLFunction>,
    pub constants: Vec<(usize, Vec<usize>)>,
    pub entrypoint: FunIdx,
}

impl Code {
    pub fn get_type(&self, idx: TypeIdx) -> &HLType {
        &self.types[idx.0]
    }
}

impl Index<TypeIdx> for Code {
    type Output = HLType;

    fn index(&self, idx: TypeIdx) -> &HLType {
        &self.types[idx.0]
    }
}

pub fn read_code(mut r: Reader) -> Result<Code, std::io::Error> {
    let mut buf = [0u8; 3];
    r.0.read_exact(&mut buf)?;
    if &buf != b"HLB" {
        return Err(std::io::Error::new(ErrorKind::InvalidData, "invalid magic"));
    }

    let version = r.byte()?;

    if version < 4 || version > 5 {
        return Err(io::Error::new(
            ErrorKind::InvalidData,
            format!("unsupported version, expected 4 or 5, got {version}"),
        ));
    }

    let flags = r.udx()?;

    let nints = r.udx()?;
    let nfloats = r.udx()?;
    let nstrings = r.udx()?;

    let nbytes = if version >= 5 { r.udx()? } else { 0 };
    let ntypes = r.udx()?;
    let nglobals = r.udx()?;
    let nnatives = r.udx()?;
    let nfunctions = r.udx()?;
    let nconstants = if version >= 4 { r.udx()? } else { 0 };
    let entrypoint = FunIdx(r.udx()?);

    let has_debug = flags & 1 == 1;

    let ints = r.vec(nints, Reader::int)?;
    let floats = r.vec(nfloats, Reader::f64)?;
    let strings = read_strings(&mut r, nstrings)?;

    let bytes = if version >= 5 {
        let size = r.int()? as usize;
        let mut bytes = Vec::new();
        bytes.resize(size, 0);
        r.0.read_exact(&mut bytes)?;
        let mut positions = Vec::with_capacity(nbytes);
        for _ in 0..nbytes {
            positions.push(r.udx()?)
        }
        Some((bytes, positions))
    } else {
        None
    };

    let debugfiles = if has_debug {
        let ndebugfiles = r.udx()?;
        let debufiles = read_strings(&mut r, ndebugfiles)?;
        Some(debufiles)
    } else {
        None
    };
    let types = r.vec(ntypes, read_type)?;
    let globals = r.vec(nglobals, Reader::idx)?;
    let natives = r.vec(nnatives, |r| {
        let lib = s(r)?;
        let name = s(r)?;
        let t = ty(r)?;
        let fidx = FunIdx(r.udx()?);
        Ok((lib, name, t, fidx))
    })?;
    let functions = r.vec(nfunctions, |r| {
        let ty_ = ty(r)?;
        let idx = FunIdx(r.udx()?);
        let nregs = r.udx()?;
        let nops = r.udx()?;
        let regs = r.vec(nregs, ty)?;
        let opcodes = r.vec(nops, crate::opcode::read_opcode)?;
        if has_debug {
            let mut i = 0;
            let mut debug = Vec::new();
            let mut curfile = -1;
            let mut curline = 0;
            let ndebugfiles = debugfiles.as_ref().unwrap().len();
            while i < nops {
                let mut c = r.byte()? as i32;
                if c & 1 > 0 {
                    c = c >> 1;
                    curfile = (c << 8) | r.byte()? as i32;
                    if curfile >= ndebugfiles as i32 {
                        return Err(io::Error::new(ErrorKind::InvalidData, "invalid debug file"));
                    }
                } else if c & 2 > 0 {
                    let delta = c >> 6;
                    let mut count = (c >> 2) & 15;
                    if i + (count as usize) > nops {
                        return Err(io::Error::new(ErrorKind::InvalidData, "outside range"));
                    }
                    while count > 0 {
                        count -= 1;
                        debug.push(curfile);
                        debug.push(curline);
                        i += 1;
                    }
                    curline += delta;
                } else if c & 4 > 0 {
                    curline += c >> 3;
                    debug.push(curfile);
                    debug.push(curline);
                    i += 1;
                } else {
                    let b2 = r.byte()? as i32;
                    let b3 = r.byte()? as i32;
                    curline = (c >> 3) | (b2 << 5) | (b3 << 13);
                    debug.push(curfile);
                    debug.push(curline);
                }
            }
            if version >= 3 {
                let nassigns = r.udx()?;
                for _ in 0..nassigns {
                    r.udx()?;
                    r.idx()?;
                }
            }
        }
        Ok(HLFunction {
            ty: ty_,
            idx,
            regs,
            opcodes,
        })
    })?;
    let constants = r.vec(nconstants, |r| {
        let global = r.udx()?;
        let nfields = r.udx()?;
        let fields = r.vec(nfields, Reader::udx)?;
        Ok((global, fields))
    })?;
    Ok(Code {
        version,
        flags,
        ints,
        floats,
        strings,
        bytes,
        debugfiles,
        types,
        globals,
        natives,
        functions,
        constants,
        entrypoint,
    })
}
