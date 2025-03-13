mod opcode;
mod reader;

use std::{
    collections::BTreeMap,
    io,
    ops::Index,
    path::Path,
};

use hl_code_derive::Readable;

pub use crate::opcode::{Idx, OpCode, Reg};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Readable)]
pub struct TypeIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Readable)]
pub struct UStrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Readable)]
pub struct StrIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Readable)]
pub struct FunIdx(pub usize);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Readable)]
pub struct GlobalIdx(pub usize);

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

#[derive(Clone, Debug, Readable)]
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

impl Code {
    pub fn from_file(path: impl AsRef<Path>) -> io::Result<Code> {
        reader::Reader::open_file(path)?.r()
    }
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
