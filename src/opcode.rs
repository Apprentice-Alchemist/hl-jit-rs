use std::io;

use crate::Reader;

pub type Reg = i32;

fn reg(r: &mut Reader) -> io::Result<i32> {
	Ok(r.idx()? as i32)
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
			idx: reg(r)?,
		},
		_ => {
			return Err(io::Error::new(io::ErrorKind::InvalidData, "invalid opcode"))
		} 
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
        idx: i32,
    },
    Float {
        dst: Reg,
        idx: i32,
    },
    Bool {
        dst: Reg,
        idx: i32,
    },
    Bytes {
        dst: Reg,
        idx: i32,
    },
    String {
        dst: Reg,
        idx: i32,
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
        f: Reg,
        args: Vec<Reg>,
    },
    Call2 {
        f: Reg,
        args: Vec<Reg>,
    },
    Call3 {
        f: Reg,
        args: Vec<Reg>,
    },
    Call4 {
        f: Reg,
        args: Vec<Reg>,
    },
    CallN {
        f: Reg,
        args: Vec<Reg>,
    },
    CallMethod {
        dst: Reg,
        obj: Reg,
        fid: i32,
        args: Vec<Reg>,
    },
    CallThis {
        dst: Reg,
        fid: i32,
        args: Vec<Reg>,
    },
    CallClosure {
        dst: Reg,
        closure: Reg,
        args: Vec<Reg>,
    },

    StaticClosure {
        dst: Reg,
        fid: Reg,
    },
    InstanceClosure {
        dst: Reg,
        obj: Reg,
        idx: i32,
    },
    VirtualClosure {
        dst: Reg,
        obj: Reg,
        idx: i32,
    },
    GetGlobal {
        dst: Reg,
        idx: i32,
    },
    SetGlobal {
        idx: i32,
        val: Reg,
    },
    Field {
        dst: Reg,
        obj: Reg,
        fid: i32,
    },
    SetField {
        obj: Reg,
        fid: Reg,
        val: i32,
    },
    GetThis {
        dst: Reg,
        fid: i32,
    },
    SetThis {
        fid: i32,
        val: Reg,
    },
    DynGet {
        dst: Reg,
        obj: Reg,
        hashed_name: i32,
    },
    DynSet {
        obj: Reg,
        hashed_name: i32,
        val: Reg,
    },
    JTrue {
        val: Reg,
        offset: i32,
    },
    JFalse {
        val: Reg,
        offset: i32,
    },
    JNull {
        val: Reg,
        offset: i32,
    },
    JNotNull {
        val: Reg,
        offset: i32,
    },
    JSLq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JSGtq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JSGq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JSLtq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JULq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JUGtq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JNotLq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JNotGtq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JEq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JNotEq {
        a: Reg,
        b: Reg,
        offset: i32,
    },
    JAlways {
        offset: i32,
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
        cases: Vec<i32>,
        end: i32,
    },
    NullCheck(Reg),
    Trap {
        dst: Reg,
        jump_off: i32,
    },
    EndTrap {
        something: i32,
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
        idx: i32,
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
        construct_idx: i32,
        params: Vec<Reg>,
    },
    EnumAlloc {
        dst: Reg,
        idx: i32,
    },
    OEnumIndex {
        dst: Reg,
        val: Reg,
    },
    EnumField {
        dst: Reg,
        obj: Reg,
        construct_idx: i32,
        field_idx: i32,
    },
    SetEnumField {
        obj: Reg,
        field_idx: i32,
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
        off: i32,
    },
    Nop,
    Prefetch {
        args: [i32; 3],
    },
    Asm {
        args: [i32; 3],
    },
}
