use std::{
    error::Error,
    fs::File,
    io::{self, BufRead, BufReader, ErrorKind, Read},
    path::Path,
};

mod opcode;
mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(improper_ctypes, reason = "triggered by bindgen generated u128")]

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

struct Reader(BufReader<File>);
impl Reader {
    fn open_file(path: impl AsRef<Path>) -> io::Result<Reader> {
        let file = std::fs::OpenOptions::new().read(true).open(path)?;
        Ok(Self(BufReader::new(file)))
    }

    fn byte(&mut self) -> io::Result<u8> {
        let mut buf = [0u8];
        self.0.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    fn idx(&mut self) -> io::Result<isize> {
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

    fn vec<T>(
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
    for i in 0..nstrings {
        let sz = r.udx()?;
        let mut buf = vec![0u8; sz];
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

type TypeIdx = isize;
type UStringIdx = i32;

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
    Function { args: Vec<TypeIdx>, ret: TypeIdx },
    Object,
    Array,
    Type,
    Reference,
    Virtual,
    Dynobj,
    Abstract,
    Enum,
    Null,
    Method { args: Vec<TypeIdx>, ret: TypeIdx },
    Struct,
    Packed,
    Guid,
}

fn read_obj(r: &mut Reader) -> io::Result<()> {
    let _name_idx = r.idx()?;
    let _super = r.idx()?;
    let _global = r.udx()?;
    let nfields = r.udx()?;
    let nproto = r.udx()?;
    let _nbindings = r.udx()?;

    let _fields = r.vec(nfields, |r| {
        let _name = r.idx()?;
        let _ty = r.idx()?;
        Ok(())
    })?;

    let _protos = r.vec(nproto, |r| {
        let _name = r.idx()?;
        let _fidx = r.udx()?;
        let _pidx = r.idx()?;
        Ok(())
    });

    let _bindings = r.vec(_nbindings, |r| Ok((r.udx()?, r.udx()?)));

    Ok(())
}

fn read_type(r: &mut Reader) -> io::Result<HLType> {
    let kind = r.byte()?;
    let t = match kind as u32 {
        sys::hl_type_kind_HVOID => HLType::Void,
        sys::hl_type_kind_HUI8 => HLType::UInt8,
        sys::hl_type_kind_HUI16 => HLType::UInt16,
        sys::hl_type_kind_HI32 => HLType::Int32,
        sys::hl_type_kind_HI64 => HLType::Int64,
        sys::hl_type_kind_HF32 => HLType::Float32,
        sys::hl_type_kind_HF64 => HLType::Float64,
        sys::hl_type_kind_HBOOL => HLType::Boolean,
        sys::hl_type_kind_HBYTES => HLType::Bytes,
        sys::hl_type_kind_HDYN => HLType::Dynamic,
        sys::hl_type_kind_HFUN => {
            let nargs = r.byte()? as usize;
            let args = r.vec(nargs, Reader::idx)?;
            let ret = r.idx()?;
            HLType::Function { args, ret }
        }
        sys::hl_type_kind_HOBJ => {
            read_obj(r)?;
            HLType::Object
        }
        sys::hl_type_kind_HARRAY => HLType::Array,
        sys::hl_type_kind_HTYPE => HLType::Type,
        sys::hl_type_kind_HREF => {
            let _ty = r.idx()?;
            HLType::Reference
        }
        sys::hl_type_kind_HVIRTUAL => {
            let nfields = r.udx()?;
            let fields = r.vec(nfields, |r| {
                let name = r.idx()?;
                let _ty = r.idx()?;
                Ok(())
            })?;
            HLType::Virtual
        }
        sys::hl_type_kind_HDYNOBJ => HLType::Dynobj,
        sys::hl_type_kind_HABSTRACT => {
            let _idx = r.idx()?;
            HLType::Abstract
        }
        sys::hl_type_kind_HENUM => {
            let _name: UStringIdx = r.idx()? as i32;
            let _global_value = r.udx()?;
            let nconstructs = r.udx()?;
            r.vec(nconstructs, |r| {
                let _name = r.idx()?;
                let nparams = r.udx()?;
                r.vec(nparams, Reader::idx)?;
                Ok(())
            })?;
            HLType::Enum
        }
        sys::hl_type_kind_HNULL => {
            let _ty_idx = r.idx()?;
            HLType::Null
        }
        sys::hl_type_kind_HMETHOD => {
            let nargs = r.byte()? as usize;
            let args = r.vec(nargs, Reader::idx)?;
            let ret = r.idx()?;
            HLType::Method { args, ret }
        }
        sys::hl_type_kind_HSTRUCT => {
            read_obj(r)?;
            HLType::Struct
        }
        sys::hl_type_kind_HPACKED => {
            let _ty_idx = r.idx()?;
            HLType::Packed
        }
        // sys::hl_type_kind_HGUID => HLType::Guid,
        kind => {
            println!("{}", kind);
            return Err(io::Error::new(ErrorKind::InvalidData, "invalid type kind"));
        }
    };
    Ok(t)
}

fn read_module(mut r: Reader) -> Result<(), std::io::Error> {
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
    let nfunctions = r.udx()?;
    let nconstants = if version >= 5 { r.udx()? } else { 0 };
    let entrypoint = r.udx()?;

    let has_debug = flags & 1 == 1;

    let ints = r.vec(nints, Reader::int)?;
    let floats = r.vec(nfloats, Reader::f64)?;

    let strings = read_strings(&mut r, nstrings)?;

    if version >= 5 {
        let size = r.int()? as usize;
        let mut bytes = Vec::new();
        bytes.resize(size, 0);
        r.0.read_exact(&mut bytes)?;
        let mut positions = Vec::with_capacity(nbytes);
        for _ in 0..nbytes {
            positions.push(r.udx()?)
        }
        // TODO: store bytes somewhere
    }

    if has_debug {
        let ndebugfiles = r.udx()?;
        let debufiles = read_strings(&mut r, ndebugfiles)?;
    
    }
    // let ntypes =
    let types = r.vec(ntypes, read_type)?;
    let globals = r.vec(nglobals, Reader::idx)?;
    let natives = r.vec(nnatives, |r| {
        let _lib = r.idx()?;
        let _name = r.idx()?;
        let _t = r.idx()?;
        let fidx = r.udx()?;
        Ok(())
    });
    let functions = r.vec(nfunctions, |r| {
        let _ty = r.idx()?;
        let _findex = r.udx()?;
        let nregs = r.udx()?;
        let nops = r.udx()?;
        let regs = r.vec(nregs, |r| {
            r.idx()
        });
        let opcodes = r.vec(nops, opcode::read_opcode)?;
        if has_debug {
            let mut i = 0;
            let mut debug = Vec::new();
            let mut curfile = -1;
            while i < nops {
                let c = r.byte()?;
                if c & 1 {
                    c = c >> 1;
                    curfile = (c << 8) | r.byte()?;
                    if curfile >= ndebufiles
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
        Ok(())
    });
    let constants = r.vec(nconstants, |r| {
        let _global = r.udx()?;
        let nfields = r.udx()?;
        let _fields = r.vec(nfields, Reader::udx)?;
        Ok(())
    })?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let reader = Reader::open_file("out.hl")?;
    read_module(reader)?;
    Ok(())
}
