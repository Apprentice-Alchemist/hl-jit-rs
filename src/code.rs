use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, BufReader, ErrorKind, Read},
    ops::Index,
    path::Path,
};

use crate::opcode::{Idx, OpCode, Reg};

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
            let v: isize = (self.byte()? as i32 | ((b & 31) << 8)) as isize;
            return Ok(if b & 0x20 == 0 { v } else { -v });
        }

        let c = self.byte()? as i32;
        let d = self.byte()? as i32;
        let e = self.byte()? as i32;
        let v: isize = (((b & 31) << 24) | (c << 16) | (d << 8) | e) as isize;
        Ok(if b & 0x20 == 0 { v } else { -v })
    }

    pub fn udx(&mut self) -> io::Result<usize> {
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

    pub fn r<T>(&mut self) -> io::Result<T>
    where
        T: Readable,
    {
        T::r(self)
    }
}

fn read_strings(r: &mut Reader, nstrings: usize) -> io::Result<Vec<String>> {
    let size = r.int()? as usize;
    let mut buf = Vec::new();
    buf.resize(size, 0);
    r.0.read_exact(&mut buf)?;

    let mut cursor = std::io::Cursor::new(buf);

    let mut string_vec = Vec::with_capacity(nstrings);
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct UStrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct StrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FunIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct GlobalIdx(pub usize);

pub trait Readable {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized;
}

impl Readable for TypeIdx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.udx()?))
    }
}
impl Readable for UStrIdx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.udx()?))
    }
}
impl Readable for StrIdx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.udx()?))
    }
}
impl Readable for FunIdx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.udx()?))
    }
}
impl Readable for GlobalIdx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.udx()?))
    }
}

#[derive(Clone, Debug)]
pub struct TypeObj {
    pub name: UStrIdx,
    pub super_: Option<TypeIdx>,
    pub global: Option<GlobalIdx>,
    pub fields: Vec<(UStrIdx, TypeIdx)>,
    pub protos: Vec<(UStrIdx, FunIdx, Idx)>,
    pub bindings: Vec<(usize, usize)>,
}

#[derive(Clone, Debug)]
pub struct TypeFun {
    pub args: Vec<TypeIdx>,
    pub ret: TypeIdx,
}

#[derive(Clone, Debug)]
pub struct TypeEnum {
    pub name: UStrIdx,
    pub global_value: Option<GlobalIdx>,
    pub constructs: Vec<(UStrIdx, Vec<TypeIdx>)>,
}

#[derive(Clone, Debug)]
pub struct TypeVirtual {
    pub fields: Vec<(UStrIdx, TypeIdx)>,
}

#[derive(Clone, Debug)]
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
    Function(TypeFun),
    Object(TypeObj),
    Array,
    Type,
    Reference(TypeIdx),
    Virtual(TypeVirtual),
    Dynobj,
    Abstract(UStrIdx),
    Enum(TypeEnum),
    Null(TypeIdx),
    Method(TypeFun),
    Struct(TypeObj),
    Packed(TypeIdx),
    Guid,
}

impl HLType {
    pub fn as_u32(&self) -> u32 {
        match self {
            Self::Void => 0,
            Self::UInt8 => 1,
            Self::UInt16 => 2,
            Self::Int32 => 3,
            Self::Int64 => 4,
            Self::Float32 => 5,
            Self::Float64 => 6,
            Self::Boolean => 7,
            Self::Bytes => 8,
            Self::Dynamic => 9,
            Self::Function(_) => 10,
            Self::Object(_) => 11,
            Self::Array => 12,
            Self::Type => 13,
            Self::Reference(_) => 14,
            Self::Virtual(_) => 15,
            Self::Dynobj => 16,
            Self::Abstract(_) => 17,
            Self::Enum(_) => 18,
            Self::Null(_) => 19,
            Self::Method(_) => 20,
            Self::Struct(_) => 21,
            Self::Packed(_) => 22,
            Self::Guid => 23,
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float32 | Self::Float64)
    }

    pub fn cranelift_type(&self) -> cranelift::prelude::Type {
        use cranelift::prelude::types;
        match self {
            Self::Void => panic!("HVOID should not be used in CLIR"),
            Self::UInt8 => types::I8,
            Self::UInt16 => types::I16,
            Self::Int32 => types::I32,
            Self::Int64 => types::I64,
            Self::Float32 => types::F32,
            Self::Float64 => types::F64,
            Self::Boolean => types::I8,
            Self::Bytes => types::I64,
            Self::Dynamic => types::I64,
            Self::Function(_) => types::I64,
            Self::Object(_) => types::I64,
            Self::Array => types::I64,
            Self::Type => types::I64,
            Self::Reference(_) => types::I64,
            Self::Virtual(_) => types::I64,
            Self::Dynobj => types::I64,
            Self::Abstract(_) => types::I64,
            Self::Enum(_) => types::I64,
            Self::Null(_) => types::I64,
            Self::Method(_) => types::I64,
            Self::Struct(_) => types::I64,
            Self::Packed(_) => panic!("HPACKED should not be used in CLIR"),
            Self::Guid => types::I64,
        }
    }

    pub fn type_obj(&self) -> Option<&TypeObj> {
        match self {
            Self::Struct(obj) | Self::Object(obj) => Some(obj),
            _ => None,
        }
    }

    pub fn type_enum(&self) -> Option<&TypeEnum> {
        match self {
            Self::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn is_dynamic(&self) -> bool {
        match self {
            Self::Dynamic
            | Self::Function(_)
            | Self::Object(_)
            | Self::Array
            | Self::Virtual(_)
            | Self::Dynobj
            | Self::Enum(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Self::Bytes
            | Self::Dynamic
            | Self::Function(_)
            | Self::Object(_)
            | Self::Array
            | Self::Type
            | Self::Reference(_)
            | Self::Virtual(_)
            | Self::Dynobj
            | Self::Abstract(_)
            | Self::Enum(_)
            | Self::Null(_)
            | Self::Method(_)
            | Self::Struct(_)
            | Self::Packed(_)
            | Self::Guid => true,
            Self::Void
            | Self::UInt8
            | Self::UInt16
            | Self::Int32
            | Self::Int64
            | Self::Float32
            | Self::Float64
            | Self::Boolean => false,
        }
    }
}

fn read_obj(r: &mut Reader) -> io::Result<TypeObj> {
    let name = r.r()?;
    let super_ = {
        let val = r.idx()?;
        if val.is_positive() {
            Some(TypeIdx(val as usize))
        } else {
            None
        }
    };
    let global = r.udx()?;
    let global = if global == 0 {
        None
    } else {
        Some(GlobalIdx(global - 1))
    };
    let nfields = r.udx()?;
    let nproto = r.udx()?;
    let nbindings = r.udx()?;

    let fields = r.vec(nfields, |r| {
        let name = r.r()?;
        let ty = r.r()?;
        Ok((name, ty))
    })?;

    let protos = r.vec(nproto, |r| {
        let name = r.r()?;
        let fidx = r.r()?;
        let pidx = r.r()?;
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

fn read_type_fun(r: &mut Reader) -> io::Result<TypeFun> {
    let nargs = r.byte()? as usize;
    let args = r.vec(nargs, Reader::r)?;
    let ret = r.r()?;
    Ok(TypeFun { args, ret })
}

fn read_type_virtual(r: &mut Reader) -> io::Result<TypeVirtual> {
    let nfields = r.udx()?;
    let fields = r.vec(nfields, |r| {
        let name = r.r()?;
        let ty_ = r.r()?;
        Ok((name, ty_))
    })?;
    Ok(TypeVirtual { fields })
}

fn read_type_enum(r: &mut Reader) -> io::Result<TypeEnum> {
    let name: UStrIdx = r.r()?;
    let global_value = r.udx()?;
    let global_value = if global_value == 0 {
        None
    } else {
        Some(GlobalIdx(global_value - 1))
    };
    let nconstructs = r.udx()?;
    let constructs = r.vec(nconstructs, |r| {
        let name = r.r()?;
        let nparams = r.udx()?;
        let params = r.vec(nparams, Reader::r)?;
        Ok((name, params))
    })?;
    Ok(TypeEnum {
        name,
        global_value,
        constructs,
    })
}

fn read_type(r: &mut Reader) -> io::Result<HLType> {
    let kind = r.byte()?;
    let t = match kind as u32 {
        0 => HLType::Void,
        1 => HLType::UInt8,
        2 => HLType::UInt16,
        3 => HLType::Int32,
        4 => HLType::Int64,
        5 => HLType::Float32,
        6 => HLType::Float64,
        7 => HLType::Boolean,
        8 => HLType::Bytes,
        9 => HLType::Dynamic,
        10 => HLType::Function(read_type_fun(r)?),
        11 => HLType::Object(read_obj(r)?),
        12 => HLType::Array,
        13 => HLType::Type,
        14 => HLType::Reference(r.r()?),
        15 => HLType::Virtual(read_type_virtual(r)?),
        16 => HLType::Dynobj,
        17 => HLType::Abstract(r.r()?),
        18 => HLType::Enum(read_type_enum(r)?),
        19 => HLType::Null(r.r()?),
        20 => HLType::Method(read_type_fun(r)?),
        21 => HLType::Struct(read_obj(r)?),
        22 => HLType::Packed(r.r()?),
        23 => HLType::Guid,
        kind => {
            return Err(io::Error::new(ErrorKind::InvalidData, format!("invalid type kind: {kind}")));
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

impl Index<Reg> for HLFunction {
    type Output = TypeIdx;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.regs[index.0]
    }
}

pub struct Code {
    pub version: u8,
    pub flags: usize,
    pub ints: Vec<i32>,
    pub floats: Vec<f64>,
    pub strings: Vec<String>,
    pub bytes: Option<Vec<Box<[u8]>>>,
    pub debugfiles: Option<Vec<String>>,
    pub types: Vec<HLType>,
    pub globals: Vec<TypeIdx>,
    pub natives: Vec<(StrIdx, StrIdx, TypeIdx, FunIdx)>,
    pub functions: Vec<HLFunction>,
    pub constants: BTreeMap<GlobalIdx, Vec<usize>>,
    pub entrypoint: FunIdx,
}

impl Index<TypeIdx> for Code {
    type Output = HLType;

    fn index(&self, idx: TypeIdx) -> &HLType {
        &self.types[idx.0]
    }
}

impl Index<StrIdx> for Code {
    type Output = str;

    fn index(&self, idx: StrIdx) -> &Self::Output {
        &self.strings[idx.0]
    }
}
impl Index<GlobalIdx> for Code {
    type Output = TypeIdx;

    fn index(&self, idx: GlobalIdx) -> &Self::Output {
        &self.globals[idx.0]
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
    let entrypoint = r.r()?;

    let has_debug = flags & 1 == 1;

    let ints = r.vec(nints, Reader::int)?;
    let floats = r.vec(nfloats, Reader::f64)?;
    let strings = read_strings(&mut r, nstrings)?;

    let bytes = if version >= 5 {
        let size = r.int()? as usize;
        let mut bytes = Vec::new();
        bytes.resize(size, 0);
        r.0.read_exact(&mut bytes)?;
        let mut cursor = std::io::Cursor::new(buf);
        let mut bytes_vec = Vec::with_capacity(nbytes);
        for _ in 0..nbytes {
            let sz = r.udx()?;
            let mut buf = vec![0; sz];
            cursor.read_exact(&mut buf);
            bytes_vec.push(buf.into_boxed_slice());
        }
        Some(bytes_vec)
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
    let globals = r.vec(nglobals, Reader::r)?;
    let natives = r.vec(nnatives, |r| {
        let lib = r.r()?;
        let name = r.r()?;
        let t = r.r()?;
        let fidx = r.r()?;
        Ok((lib, name, t, fidx))
    })?;
    let functions = r.vec(nfunctions, |r| {
        let ty_ = r.r()?;
        let idx = r.r()?;
        let nregs = r.udx()?;
        let nops = r.udx()?;
        let regs = r.vec(nregs, Reader::r)?;
        let opcodes = r.vec(nops, crate::opcode::read_opcode)?;
        if let Some(ref debugfiles) = debugfiles {
            let mut i = 0;
            let mut debug = Vec::new();
            let mut curfile = -1;
            let mut curline = 0;
            let ndebugfiles = debugfiles.len();
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
                    i += 1;
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
        let global = r.r()?;
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
        constants: BTreeMap::from_iter(constants.into_iter()),
        entrypoint,
    })
}
