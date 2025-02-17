use std::io;

use crate::Reader;

pub struct Reg(i32);
pub struct Idx(isize);

fn reg(r: &mut Reader) -> io::Result<Reg> {
    Ok(Reg(r.idx()? as i32))
}

fn idx(r: &mut Reader) -> io::Result<Idx> {
    Ok(Idx(r.idx()?))
}

pub fn read_opcode(r: &mut Reader) -> io::Result<OpCode> {
    use OpCode::*;
    let code = r.byte()?;
    let code = match code {
        0 => Mov {
            dst: reg(r)?,
            src: reg(r)?,
        },
        1 => Int {
            dst: reg(r)?,
            idx: idx(r)?,
        },
        2 => Float {
            dst: reg(r)?,
            idx: idx(r)?,
        },
        3 => Bool {
            dst: reg(r)?,
            val: idx(r)?.0 > 0,
        },
        4 => Bytes {
            dst: reg(r)?,
            idx: idx(r)?,
        },
        5 => String {
            dst: reg(r)?,
            idx: idx(r)?,
        },
        6 => Null { dst: reg(r)? },
        7 => Add {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        8 => Sub {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        9 => Mul {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        10 => SDiv {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        11 => UDiv {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        12 => SMod {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        13 => UMod {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        14 => Shl {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        15 => SShr {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        16 => UShr {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        17 => And {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        18 => Or {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        19 => Xor {
            dst: reg(r)?,
            a: reg(r)?,
            b: reg(r)?,
        },
        20 => Neg {
            dst: reg(r)?,
            val: reg(r)?,
        },
        21 => Not {
            dst: reg(r)?,
            val: reg(r)?,
        },
        22 => Incr { dst: reg(r)? },
        23 => Decr { dst: reg(r)? },
        24 => Call0 {
            dst: reg(r)?,
            f: reg(r)?,
        },
        25 => Call1 {
            dst: reg(r)?,
            f: reg(r)?,
            args: vec![reg(r)?],
        },
        26 => Call2 {
            dst: reg(r)?,
            f: reg(r)?,
            args: vec![reg(r)?, reg(r)?],
        },
        27 => Call3 {
            dst: reg(r)?,
            f: reg(r)?,
            args: vec![reg(r)?, reg(r)?, reg(r)?],
        },
        28 => Call4 {
            dst: reg(r)?,
            f: reg(r)?,
            args: vec![reg(r)?, reg(r)?, reg(r)?, reg(r)?],
        },
        29 => CallN {
            dst: reg(r)?,
            f: reg(r)?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, reg)?
            },
        },
        30 => CallMethod {
            dst: reg(r)?,
            fid: idx(r)?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, reg)?
            },
        },
        31 => CallThis {
            dst: reg(r)?,
            fid: idx(r)?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, reg)?
            },
        },
        32 => CallClosure {
            dst: reg(r)?,
            closure: reg(r)?,
            args: {
                let count = r.byte()? as usize;
                r.vec(count, reg)?
            },
        },
        33 => StaticClosure {
            dst: reg(r)?,
            fid: idx(r)?,
        },
        34 => InstanceClosure {
            dst: reg(r)?,
            obj: reg(r)?,
            idx: idx(r)?,
        },
        35 => VirtualClosure {
            dst: reg(r)?,
            obj: reg(r)?,
            idx: idx(r)?,
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
        fid: Reg,
        val: Idx,
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
    JSLq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSGtq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSGq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JSLtq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JULq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JUGtq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JNotLq {
        a: Reg,
        b: Reg,
        offset: Idx,
    },
    JNotGtq {
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

    OToDyn {
        dst: Reg,
        val: Reg,
    },
    OToSFloat {
        dst: Reg,
        val: Reg,
    },
    OToUFloat {
        dst: Reg,
        val: Reg,
    },
    OToInt {
        dst: Reg,
        val: Reg,
    },
    OSafeCast {
        dst: Reg,
        val: Reg,
    },
    OUnsafeCast {
        dst: Reg,
        val: Reg,
    },
    OToVirtual {
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
