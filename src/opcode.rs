use std::io;

use cranelift::prelude::{Variable,EntityRef};

use crate::Reader;
#[derive(Copy, Clone)]
pub struct Reg(usize);

impl Reg {
    pub fn var(self) -> Variable {
        Variable::new(self.0)
    }
}

impl crate::code::Readable for Reg {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Reg(r.idx()? as usize))
    }
}
#[derive(Copy, Clone)]
pub struct Idx(isize);

impl crate::code::Readable for Idx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.idx()?))
    }
}

pub fn read_opcode(r: &mut Reader) -> io::Result<OpCode> {
    use OpCode::*;
    let code = r.byte()?;
    let code = match code {
        0 => Mov {
            dst: r.r()?,
            src: r.r()?,
        },
        1 => Int {
            dst: r.r()?,
            idx: r.r()?,
        },
        2 => Float {
            dst: r.r()?,
            idx: r.r()?,
        },
        3 => Bool {
            dst: r.r()?,
            val: r.idx()? > 0,
        },
        4 => Bytes {
            dst: r.r()?,
            idx: r.r()?,
        },
        5 => String {
            dst: r.r()?,
            idx: r.r()?,
        },
        6 => Null { dst: r.r()? },
        7 => Add {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        8 => Sub {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        9 => Mul {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        10 => SDiv {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        11 => UDiv {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        12 => SMod {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        13 => UMod {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        14 => Shl {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        15 => SShr {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        16 => UShr {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        17 => And {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        18 => Or {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        19 => Xor {
            dst: r.r()?,
            a: r.r()?,
            b: r.r()?,
        },
        20 => Neg {
            dst: r.r()?,
            val: r.r()?,
        },
        21 => Not {
            dst: r.r()?,
            val: r.r()?,
        },
        22 => Incr { dst: r.r()? },
        23 => Decr { dst: r.r()? },
        24 => Call0 {
            dst: r.r()?,
            f: r.r()?,
        },
        25 => Call1 {
            dst: r.r()?,
            f: r.r()?,
            args: vec![r.r()?],
        },
        26 => Call2 {
            dst: r.r()?,
            f: r.r()?,
            args: vec![r.r()?, r.r()?],
        },
        27 => Call3 {
            dst: r.r()?,
            f: r.r()?,
            args: vec![r.r()?, r.r()?, r.r()?],
        },
        28 => Call4 {
            dst: r.r()?,
            f: r.r()?,
            args: vec![r.r()?, r.r()?, r.r()?, r.r()?],
        },
        29 => CallN {
            dst: r.r()?,
            f: r.r()?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, Reader::r)?
            },
        },
        30 => CallMethod {
            dst: r.r()?,
            fid: r.r()?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, Reader::r)?
            },
        },
        31 => CallThis {
            dst: r.r()?,
            fid: r.r()?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, Reader::r)?
            },
        },
        32 => CallClosure {
            dst: r.r()?,
            closure: r.r()?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, Reader::r)?
            },
        },
        33 => StaticClosure {
            dst: r.r()?,
            fid: r.r()?,
        },
        34 => InstanceClosure {
            dst: r.r()?,
            obj: r.r()?,
            idx: r.r()?,
        },
        35 => VirtualClosure {
            dst: r.r()?,
            obj: r.r()?,
            idx: r.r()?,
        },
        36 => GetGlobal {
            dst: r.r()?,
            idx: r.r()?,
        },
        37 => SetGlobal {
            idx: r.r()?,
            val: r.r()?,
        },
        38 => Field {
            dst: r.r()?,
            obj: r.r()?,
            fid: r.r()?,
        },
        39 => SetField {
            obj: r.r()?,
            fid: r.r()?,
            val: r.r()?,
        },
        40 => GetThis {
            dst: r.r()?,
            fid: r.r()?,
        },
        41 => SetThis {
            fid: r.r()?,
            val: r.r()?,
        },
        42 => DynGet {
            dst: r.r()?,
            obj: r.r()?,
            hashed_name: r.r()?,
        },
        43 => DynSet {
            obj: r.r()?,
            hashed_name: r.r()?,
            val: r.r()?,
        },
        44 => JTrue {
            val: r.r()?,
            offset: r.r()?,
        },
        45 => JFalse {
            val: r.r()?,
            offset: r.r()?,
        },
        46 => JNull {
            val: r.r()?,
            offset: r.r()?,
        },
        47 => JNotNull {
            val: r.r()?,
            offset: r.r()?,
        },
        48 => JSLt {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        49 => JSGte {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        50 => JSGt {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        51 => JSLte {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        52 => JULt {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        53 => JUGte {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        54 => JNotLt {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        55 => JNotGte {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        56 => JEq {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        57 => JNotEq {
            a: r.r()?,
            b: r.r()?,
            offset: r.r()?,
        },
        58 => JAlways { offset: r.r()? },
        59 => ToDyn {
            dst: r.r()?,
            val: r.r()?,
        },
        60 => ToSFloat {
            dst: r.r()?,
            val: r.r()?,
        },
        61 => ToUFloat {
            dst: r.r()?,
            val: r.r()?,
        },
        62 => ToInt {
            dst: r.r()?,
            val: r.r()?,
        },
        63 => SafeCast {
            dst: r.r()?,
            val: r.r()?,
        },
        64 => UnsafeCast {
            dst: r.r()?,
            val: r.r()?,
        },
        65 => ToVirtual {
            dst: r.r()?,
            val: r.r()?,
        },

        _ => return Err(io::Error::new(io::ErrorKind::InvalidData, "invalid opcode")),
    };
    Ok(code)
}

pub enum OpCode {
    Mov {
        dst: Reg,
        src: Reg,
    },
    Int {
        dst: Reg,
        idx: Idx,
    },
    Float {
        dst: Reg,
        idx: Idx,
    },
    Bool {
        dst: Reg,
        val: bool,
    },
    Bytes {
        dst: Reg,
        idx: Idx,
    },
    String {
        dst: Reg,
        idx: Idx,
    },
    Null {
        dst: Reg,
    },
    Add {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Sub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Mul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    SDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    UDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    SMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    UMod {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Shl {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    SShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    UShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    And {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Or {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Xor {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    Neg {
        dst: Reg,
        val: Reg,
    },
    Not {
        dst: Reg,
        val: Reg,
    },
    Incr {
        dst: Reg,
    },
    Decr {
        dst: Reg,
    },

    Call0 {
        dst: Reg,
        f: Reg,
    },
    Call1 {
        dst: Reg,
        f: Reg,
        args: Vec<Reg>,
    },
    Call2 {
        dst: Reg,
        f: Reg,
        args: Vec<Reg>,
    },
    Call3 {
        dst: Reg,
        f: Reg,
        args: Vec<Reg>,
    },
    Call4 {
        dst: Reg,
        f: Reg,
        args: Vec<Reg>,
    },
    CallN {
        dst: Reg,
        f: Reg,
        args: Vec<Reg>,
    },
    CallMethod {
        dst: Reg,
        fid: Idx,
        args: Vec<Reg>,
    },
    CallThis {
        dst: Reg,
        fid: Idx,
        args: Vec<Reg>,
    },
    CallClosure {
        dst: Reg,
        closure: Reg,
        args: Vec<Reg>,
    },

    StaticClosure {
        dst: Reg,
        fid: Idx,
    },
    InstanceClosure {
        dst: Reg,
        obj: Reg,
        idx: Idx,
    },
    VirtualClosure {
        dst: Reg,
        obj: Reg,
        idx: Idx,
    },
    GetGlobal {
        dst: Reg,
        idx: Idx,
    },
    SetGlobal {
        idx: Idx,
        val: Reg,
    },
    Field {
        dst: Reg,
        obj: Reg,
        fid: Idx,
    },
    SetField {
        obj: Reg,
        fid: Idx,
        val: Reg,
    },
    GetThis {
        dst: Reg,
        fid: Idx,
    },
    SetThis {
        fid: Idx,
        val: Reg,
    },
    DynGet {
        dst: Reg,
        obj: Reg,
        hashed_name: Idx,
    },
    DynSet {
        obj: Reg,
        hashed_name: Idx,
        val: Reg,
    },
    JTrue {
        val: Reg,
        offset: Idx,
    },
    JFalse {
        val: Reg,
        offset: Idx,
    },
    JNull {
        val: Reg,
        offset: Idx,
    },
    JNotNull {
        val: Reg,
        offset: Idx,
    },
    JSLt {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSGte {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSGt {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSLte {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JULt {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JUGte {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JNotLt {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JNotGte {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JEq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JNotEq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JAlways {
        offset: Idx,
    },

    ToDyn {
        dst: Reg,
        val: Reg,
    },
    ToSFloat {
        dst: Reg,
        val: Reg,
    },
    ToUFloat {
        dst: Reg,
        val: Reg,
    },
    ToInt {
        dst: Reg,
        val: Reg,
    },
    SafeCast {
        dst: Reg,
        val: Reg,
    },
    UnsafeCast {
        dst: Reg,
        val: Reg,
    },
    ToVirtual {
        dst: Reg,
        val: Reg,
    },

    Label,
    Ret(Reg),
    Throw(Reg),
    Rethrow(Reg),
    Switch {
        val: Reg,
        cases: Vec<Idx>,
        end: Idx,
    },
    NullCheck(Reg),
    Trap {
        dst: Reg,
        jump_off: Idx,
    },
    EndTrap {
        something: Idx,
    },
    GetI8 {
        dst: Reg,
        mem: Reg,
        val: Reg,
    },
    GetI16 {
        dst: Reg,
        mem: Reg,
        val: Reg,
    },
    GetMem {
        dst: Reg,
        mem: Reg,
        val: Reg,
    },
    GetArray {
        dst: Reg,
        mem: Reg,
        val: Reg,
    },
    OSetI8 {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    OSetI16 {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    OSetMem {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    OSetArray {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },

    New {
        dst: Reg,
    },
    ArraySize {
        dst: Reg,
        arr: Reg,
    },
    Type {
        dst: Reg,
        idx: Idx,
    },
    GetType {
        dst: Reg,
        val: Reg,
    },
    GetTid {
        dst: Reg,
        val: Reg,
    },
    Ref {
        dst: Reg,
        val: Reg,
    },
    Unref {
        dst: Reg,
        r: Reg,
    },
    Setref {
        r: Reg,
        val: Reg,
    },
    MakeEnum {
        dst: Reg,
        construct_idx: Idx,
        params: Vec<Reg>,
    },
    EnumAlloc {
        dst: Reg,
        idx: Idx,
    },
    OEnumIndex {
        dst: Reg,
        val: Reg,
    },
    EnumField {
        dst: Reg,
        obj: Reg,
        construct_idx: Idx,
        field_idx: Idx,
    },
    SetEnumField {
        obj: Reg,
        field_idx: Idx,
        val: Reg,
    },

    Assert,
    RefData {
        dst: Reg,
        r: Reg,
    },
    RefOffset {
        dst: Reg,
        r: Reg,
        off: Idx,
    },
    Nop,
    Prefetch {
        args: [Idx; 3],
    },
    Asm {
        args: [Idx; 3],
    },
}
