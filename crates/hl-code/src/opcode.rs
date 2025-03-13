use std::{io, ops::Deref};

use hl_code_derive::Readable;

use crate::{FunIdx, GlobalIdx, TypeIdx, UStrIdx, reader::Readable, reader::Reader};
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Reg(pub usize);

impl Readable for Reg {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Reg(r.idx()? as usize))
    }
}
#[derive(Copy, Clone, Debug)]
pub struct Idx(pub isize);

impl Readable for Idx {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(r.idx()?))
    }
}

#[derive(Debug)]
pub struct Args(Vec<Reg>);

impl Readable for Args {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        let count = r.byte()? as usize;
        Ok(Self(r.vec(count, Reader::r)?))
    }
}

#[derive(Debug)]
pub struct Cases(Vec<Idx>);

impl Readable for Cases {
    fn r(r: &mut Reader) -> io::Result<Self>
    where
        Self: Sized,
    {
        let count = r.udx()? as usize;
        Ok(Self(r.vec(count, Reader::r)?))
    }
}

impl Deref for Args {
    type Target = [Reg];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for Cases {
    type Target = [Idx];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[repr(u8)]
#[derive(Debug, Readable)]
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
        val: Idx,
    },
    Bytes {
        dst: Reg,
        idx: Idx,
    },
    String {
        dst: Reg,
        idx: UStrIdx,
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
        f: FunIdx,
    },
    Call1 {
        dst: Reg,
        f: FunIdx,
        args: [Reg; 1],
    },
    Call2 {
        dst: Reg,
        f: FunIdx,
        args: [Reg; 2],
    },
    Call3 {
        dst: Reg,
        f: FunIdx,
        args: [Reg; 3],
    },
    Call4 {
        dst: Reg,
        f: FunIdx,
        args: [Reg; 4],
    },
    CallN {
        dst: Reg,
        f: FunIdx,
        args: Args,
    },
    CallMethod {
        dst: Reg,
        fid: Idx,
        args: Args,
    },
    CallThis {
        dst: Reg,
        fid: Idx,
        args: Args,
    },
    CallClosure {
        dst: Reg,
        closure: Reg,
        args: Args,
    },

    StaticClosure {
        dst: Reg,
        fid: FunIdx,
    },
    InstanceClosure {
        dst: Reg,
        idx: FunIdx,
        obj: Reg,
    },
    VirtualClosure {
        dst: Reg,
        obj: Reg,
        idx: Idx,
    },
    GetGlobal {
        dst: Reg,
        idx: GlobalIdx,
    },
    SetGlobal {
        idx: GlobalIdx,
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
        str_idx: UStrIdx,
    },
    DynSet {
        obj: Reg,
        str_idx: UStrIdx,
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
        cases: Cases,
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
        offset: Reg,
    },
    GetI16 {
        dst: Reg,
        mem: Reg,
        offset: Reg,
    },
    GetMem {
        dst: Reg,
        mem: Reg,
        offset: Reg,
    },
    GetArray {
        dst: Reg,
        mem: Reg,
        offset: Reg,
    },
    SetI8 {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    SetI16 {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    SetMem {
        mem: Reg,
        offset: Reg,
        val: Reg,
    },
    SetArray {
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
        idx: TypeIdx,
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
        params: Args,
    },
    EnumAlloc {
        dst: Reg,
        idx: Idx,
    },
    EnumIndex {
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
        off: Reg,
    },
    Nop,
    Prefetch {
        args: [Idx; 3],
    },
    Asm {
        args: [Idx; 3],
    },
}
