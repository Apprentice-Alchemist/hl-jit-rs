use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, BufReader, ErrorKind, Read},
    path::Path,
};

use crate::{
    Code, FunIdx, GlobalIdx, HLFunction, HLType, StrIdx, TypeEnum, TypeFun, TypeIdx, TypeObj,
    TypeVirtual, UStrIdx,
};

pub struct Reader(BufReader<File>);
impl Reader {
    pub fn open_file(path: impl AsRef<Path>) -> io::Result<Reader> {
        let file = std::fs::OpenOptions::new().read(true).open(path)?;
        Ok(Self(BufReader::new(file)))
    }

    pub fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        self.0.read_exact(buf)
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

    pub fn int(&mut self) -> io::Result<i32> {
        let mut buf = [0u8; 4];
        self.0.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    pub fn f64(&mut self) -> io::Result<f64> {
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
impl Readable for TypeObj {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
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
}
impl Readable for TypeFun {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        let nargs = r.byte()? as usize;
        let args = r.vec(nargs, Reader::r)?;
        let ret = r.r()?;
        Ok(TypeFun { args, ret })
    }
}
impl Readable for TypeVirtual {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        let nfields = r.udx()?;
        let fields = r.vec(nfields, |r| {
            let name = r.r()?;
            let ty_ = r.r()?;
            Ok((name, ty_))
        })?;
        Ok(TypeVirtual { fields })
    }
}

impl Readable for TypeEnum {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
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
}

impl Readable for HLType {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
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
            10 => HLType::Function(r.r()?),
            11 => HLType::Object(r.r()?),
            12 => HLType::Array,
            13 => HLType::Type,
            14 => HLType::Reference(r.r()?),
            15 => HLType::Virtual(r.r()?),
            16 => HLType::Dynobj,
            17 => HLType::Abstract(r.r()?),
            18 => HLType::Enum(r.r()?),
            19 => HLType::Null(r.r()?),
            20 => HLType::Method(r.r()?),
            21 => HLType::Struct(r.r()?),
            22 => HLType::Packed(r.r()?),
            23 => HLType::Guid,
            kind => {
                return Err(io::Error::new(
                    ErrorKind::InvalidData,
                    format!("invalid type kind: {kind}"),
                ));
            }
        };
        Ok(t)
    }
}

fn read_strings(r: &mut Reader, nstrings: usize) -> io::Result<Vec<String>> {
    let size = r.int()? as usize;
    let mut buf = Vec::new();
    buf.resize(size, 0);
    r.read_exact(&mut buf)?;

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

impl Readable for Code {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        let mut buf = [0u8; 3];
        r.read_exact(&mut buf)?;
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
        let strings = read_strings(r, nstrings)?;

        let bytes = if version >= 5 {
            let size = r.int()? as usize;
            let mut bytes = Vec::new();
            bytes.resize(size, 0);
            r.read_exact(&mut bytes)?;
            let mut cursor = std::io::Cursor::new(buf);
            let mut bytes_vec = Vec::with_capacity(nbytes);
            for _ in 0..nbytes {
                let sz = r.udx()?;
                let mut buf = vec![0; sz];
                cursor.read_exact(&mut buf)?;
                bytes_vec.push(buf.into_boxed_slice());
            }
            Some(bytes_vec)
        } else {
            None
        };

        let debugfiles = if has_debug {
            let ndebugfiles = r.udx()?;
            let debufiles = read_strings(r, ndebugfiles)?;
            Some(debufiles)
        } else {
            None
        };
        let types = r.vec(ntypes, Reader::r)?;
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
            let opcodes = r.vec(nops, Reader::r)?;
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
                            return Err(io::Error::new(
                                ErrorKind::InvalidData,
                                "invalid debug file",
                            ));
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
}
