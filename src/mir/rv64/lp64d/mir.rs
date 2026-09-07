//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and/or sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
 */

use std::collections::HashMap;

use super::abi::Lp64dAbi;
use crate::air::tac::{self, AirStage, Tac, TacArena, TacId};
use crate::mir::MirGenerator as AbstractMirGenerator;
use crate::mir::abi::Abi;

use crate::types::{TypeRef, alignment_of, is_double_type, is_signed, size_of};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirId(pub usize);

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(unused)]
pub enum Register {
    Zero,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
    FT0,
    FT1,
    FT2,
    FT3,
    FS0,
    FS1,
    FA0,
    FA1,
    FA2,
    FA3,
    FA4,
    FA5,
    FA6,
    FA7,
    FT8,
    FT9,
    FT10,
    FT11,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Zero => write!(f, "zero"),
            Self::Ra => write!(f, "ra"),
            Self::Sp => write!(f, "sp"),
            Self::Gp => write!(f, "gp"),
            Self::Tp => write!(f, "tp"),
            Self::T0 => write!(f, "t0"),
            Self::T1 => write!(f, "t1"),
            Self::T2 => write!(f, "t2"),
            Self::S0 => write!(f, "s0"),
            Self::S1 => write!(f, "s1"),
            Self::A0 => write!(f, "a0"),
            Self::A1 => write!(f, "a1"),
            Self::A2 => write!(f, "a2"),
            Self::A3 => write!(f, "a3"),
            Self::A4 => write!(f, "a4"),
            Self::A5 => write!(f, "a5"),
            Self::A6 => write!(f, "a6"),
            Self::A7 => write!(f, "a7"),
            Self::S2 => write!(f, "s2"),
            Self::S3 => write!(f, "s3"),
            Self::S4 => write!(f, "s4"),
            Self::S5 => write!(f, "s5"),
            Self::S6 => write!(f, "s6"),
            Self::S7 => write!(f, "s7"),
            Self::S8 => write!(f, "s8"),
            Self::S9 => write!(f, "s9"),
            Self::S10 => write!(f, "s10"),
            Self::S11 => write!(f, "s11"),
            Self::T3 => write!(f, "t3"),
            Self::T4 => write!(f, "t4"),
            Self::T5 => write!(f, "t5"),
            Self::T6 => write!(f, "t6"),
            Self::FT0 => write!(f, "ft0"),
            Self::FT1 => write!(f, "ft1"),
            Self::FT2 => write!(f, "ft2"),
            Self::FT3 => write!(f, "ft3"),
            Self::FS0 => write!(f, "fs0"),
            Self::FS1 => write!(f, "fs1"),
            Self::FA0 => write!(f, "fa0"),
            Self::FA1 => write!(f, "fa1"),
            Self::FA2 => write!(f, "fa2"),
            Self::FA3 => write!(f, "fa3"),
            Self::FA4 => write!(f, "fa4"),
            Self::FA5 => write!(f, "fa5"),
            Self::FA6 => write!(f, "fa6"),
            Self::FA7 => write!(f, "fa7"),
            Self::FT8 => write!(f, "ft8"),
            Self::FT9 => write!(f, "ft9"),
            Self::FT10 => write!(f, "ft10"),
            Self::FT11 => write!(f, "ft11"),
        }
    }
}

impl Register {
    pub fn is_fp(self) -> bool {
        matches!(
            self,
            Self::FT0
                | Self::FT1
                | Self::FT2
                | Self::FT3
                | Self::FS0
                | Self::FS1
                | Self::FA0
                | Self::FA1
                | Self::FA2
                | Self::FA3
                | Self::FA4
                | Self::FA5
                | Self::FA6
                | Self::FA7
                | Self::FT8
                | Self::FT9
                | Self::FT10
                | Self::FT11
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(u64),
    Mem(MirId, i32),
    Reg(Register),
    Sym(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Ret,
    Lea(MirId, MirId),
    AddPtr(MirId, MirId, usize, MirId),
    Mov(MirId, MirId, usize),
    Movs(MirId, MirId),
    Movz(MirId, MirId),
    MovF(MirId, MirId),
    MovFToGp(MirId, MirId),
    Load(MirId, MirId, usize),
    Store(MirId, MirId, usize),
    Cvtsi2sd {
        src: MirId,
        dst: MirId,
        signed: bool,
        size: usize,
    },
    Cvttsd2si {
        src: MirId,
        dst: MirId,
        signed: bool,
        size: usize,
    },
    Neg(MirId, MirId, usize),
    Not(MirId, MirId, usize),
    Imul(MirId, MirId, MirId, usize),
    Idiv(MirId, MirId, MirId, usize, bool),
    Irem(MirId, MirId, MirId, usize, bool),
    Add(MirId, MirId, MirId, usize),
    Sub(MirId, MirId, MirId, usize),
    Shl(MirId, MirId, MirId, usize),
    Shr(MirId, MirId, MirId, usize),
    Sar(MirId, MirId, MirId, usize),
    And(MirId, MirId, MirId, usize),
    Or(MirId, MirId, MirId, usize),
    Xor(MirId, MirId, MirId, usize),
    FAddD(MirId, MirId, MirId),
    FSubD(MirId, MirId, MirId),
    FMulD(MirId, MirId, MirId),
    FDivD(MirId, MirId, MirId),
    CmpEq(MirId, MirId, MirId, usize),
    CmpNe(MirId, MirId, MirId, usize),
    CmpLt(MirId, MirId, MirId, usize, bool),
    CmpLe(MirId, MirId, MirId, usize, bool),
    CmpGt(MirId, MirId, MirId, usize, bool),
    CmpGe(MirId, MirId, MirId, usize, bool),
    FeqD(MirId, MirId, MirId),
    FneD(MirId, MirId, MirId),
    FltD(MirId, MirId, MirId),
    FleD(MirId, MirId, MirId),
    FgtD(MirId, MirId, MirId),
    FgeD(MirId, MirId, MirId),
    Jmp(MirId),
    Beqz(MirId, MirId),
    Bnez(MirId, MirId),
    FBeqz(MirId, MirId),
    FBnez(MirId, MirId),
    Call(MirId),
    PushBytes(usize),
    PopBytes(usize),
    Push(MirId, usize),
    PushF(MirId),
    Label(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Function {
        name: String,
        global: bool,
        stack: usize,
        mir: Vec<MirId>,
    },
    Data {
        name: String,
        global: bool,
        read_only: bool,
        inits: Vec<MirId>,
        alignment: usize,
        total_size: usize,
    },
    InitInteger {
        size: usize,
        value: u64,
    },
    InitDouble(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Mir {
    Op(Op),
    Operand(Operand, usize),
    Object(Object),
}

type VarMap = HashMap<String, MirId>;
type LabelMap = HashMap<usize, MirId>;

#[derive(Debug)]
pub struct MirArena {
    pub arena: Vec<Mir>,
    pub top_level: Vec<MirId>,
}

impl std::ops::Index<MirId> for MirArena {
    type Output = Mir;

    fn index(&self, id: MirId) -> &Mir {
        &self.arena[id.0]
    }
}

#[derive(Debug)]
pub struct MirStage {
    pub mir: MirArena,
}

pub struct MirGenerator {
    arena: Vec<Mir>,
    mir_ids: Vec<MirId>,
    var_map: VarMap,
    label_map: LabelMap,
    stack_top: i32,
    tac_arena: TacArena,
    abi: Lp64dAbi,
}

impl MirGenerator {
    fn label(&mut self, idx: usize) -> MirId {
        let len = self.arena.len();
        *self.label_map.entry(idx).or_insert_with(|| {
            let id = MirId(len);
            self.arena.push(Mir::Op(Op::Label(idx)));
            id
        })
    }

    fn operand_size(&self, operand: MirId) -> usize {
        match &self.arena[operand.0] {
            Mir::Operand(_, size) => *size,
            _ => unreachable!(),
        }
    }

    fn reg_for(&mut self, reg: Register, src: MirId) -> MirId {
        match &self.arena[src.0] {
            Mir::Operand(_, size) => {
                self.alloc(Mir::Operand(Operand::Reg(reg), *size))
            }
            _ => unreachable!(),
        }
    }

    fn mov(&mut self, src: MirId, dst: MirId) -> MirId {
        self.alloc(Mir::Op(Op::Mov(src, dst, self.operand_size(dst))))
    }

    fn stack_variable(&mut self, ty: &TypeRef, name: &str) -> MirId {
        if !self.var_map.contains_key(name) {
            let ty_size = size_of(ty);
            let size = ty_size as i32;
            let stack_align = self.abi.stack_alignment() as i32;
            let align = if ty_size >= self.abi.stack_alignment() {
                stack_align
            } else {
                alignment_of(ty) as i32
            };

            self.stack_top -= size;
            self.stack_top &= !(align - 1);
            let s0 = self.alloc(Mir::Operand(Operand::Reg(Register::S0), 8));
            let v0 = self
                .alloc(Mir::Operand(Operand::Mem(s0, self.stack_top), ty_size));
            self.var_map.insert(name.to_string(), v0);
        }
        self.var_map[name]
    }

    fn integer(&mut self, ty: &TypeRef, value: u64) -> MirId {
        self.alloc(Mir::Operand(Operand::Imm(value), size_of(ty)))
    }

    fn double(&mut self, value: f64) -> MirId {
        self.alloc(Mir::Operand(Operand::Imm(value.to_bits()), 8))
    }

    fn multiply(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::FMulD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v2);
            self.alloc(Mir::Op(Op::Imul(v0, v1, v2, size)))
        }
    }

    fn divide(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::FDivD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v2);
            let signed = is_signed(ty);
            self.alloc(Mir::Op(Op::Idiv(v0, v1, v2, size, signed)))
        }
    }

    fn modulo(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);
        let signed = is_signed(ty);
        self.alloc(Mir::Op(Op::Irem(v0, v1, v2, size, signed)))
    }

    fn add(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::FAddD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v2);
            self.alloc(Mir::Op(Op::Add(v0, v1, v2, size)))
        }
    }

    fn add_ptr(
        &mut self,
        lhs: TacId,
        rhs: TacId,
        scale: usize,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        self.alloc(Mir::Op(Op::AddPtr(v0, v1, scale, v2)))
    }

    fn subtract(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::FSubD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v2);
            self.alloc(Mir::Op(Op::Sub(v0, v1, v2, size)))
        }
    }

    fn shift_left(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);
        self.alloc(Mir::Op(Op::Shl(v0, v1, v2, size)))
    }

    fn shift_right(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);

        if is_signed(ty) {
            self.alloc(Mir::Op(Op::Sar(v0, v1, v2, size)))
        } else {
            self.alloc(Mir::Op(Op::Shr(v0, v1, v2, size)))
        }
    }

    fn and(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);
        self.alloc(Mir::Op(Op::And(v0, v1, v2, size)))
    }

    fn or(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);
        self.alloc(Mir::Op(Op::Or(v0, v1, v2, size)))
    }

    fn xor(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::Xor(v0, v1, v2, 8)))
        } else {
            let size = self.operand_size(v2);
            self.alloc(Mir::Op(Op::Xor(v0, v1, v2, size)))
        }
    }

    fn equal(
        &mut self,
        _ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FeqD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            self.alloc(Mir::Op(Op::CmpEq(v0, v1, v2, size)))
        }
    }

    fn not_eq(
        &mut self,
        _ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FneD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            self.alloc(Mir::Op(Op::CmpNe(v0, v1, v2, size)))
        }
    }

    fn less(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FltD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            let signed = is_signed(ty);
            self.alloc(Mir::Op(Op::CmpLt(v0, v1, v2, size, signed)))
        }
    }

    fn less_or_eq(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FleD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            let signed = is_signed(ty);
            self.alloc(Mir::Op(Op::CmpLe(v0, v1, v2, size, signed)))
        }
    }

    fn greater(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FgtD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            let signed = is_signed(ty);
            self.alloc(Mir::Op(Op::CmpGt(v0, v1, v2, size, signed)))
        }
    }

    fn greater_or_eq(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    ) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);

        if double {
            self.alloc(Mir::Op(Op::FgeD(v0, v1, v2)))
        } else {
            let size = self.operand_size(v0);
            let signed = is_signed(ty);
            self.alloc(Mir::Op(Op::CmpGe(v0, v1, v2, size, signed)))
        }
    }

    fn copy(&mut self, src: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(src);
        let v1 = self.expr(dst);
        self.mov(v0, v1)
    }

    fn copy_to_offset(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        dst: TacId,
        off: usize,
    ) -> MirId {
        let v0 = self.expr(lhs);

        let v1 = self.expr(dst);
        let (base, base_off) = match &self.arena[v1.0] {
            Mir::Operand(Operand::Mem(base, base_off), _) => (*base, *base_off),
            _ => unreachable!(),
        };

        let v2 = self.alloc(Mir::Operand(
            Operand::Mem(base, base_off + off as i32),
            size_of(ty),
        ));

        if is_double_type(ty) {
            self.alloc(Mir::Op(Op::MovF(v0, v2)))
        } else {
            self.mov(v0, v2)
        }
    }

    fn truncate(&mut self, lhs: TacId, dst: TacId) -> MirId {
        self.copy(lhs, dst)
    }

    fn int_to_double(&mut self, signed: bool, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        let size = self.operand_size(v0);
        self.alloc(Mir::Op(Op::Cvtsi2sd {
            src: v0,
            dst: v1,
            signed,
            size,
        }))
    }

    fn double_to_int(&mut self, ty: &TypeRef, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        self.alloc(Mir::Op(Op::Cvttsd2si {
            src: v0,
            dst: v1,
            signed: is_signed(ty),
            size: size_of(ty),
        }))
    }

    fn double_to_ulong(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        self.alloc(Mir::Op(Op::Cvttsd2si {
            src: v0,
            dst: v1,
            signed: false,
            size: size_of(ty),
        }))
    }

    fn jump(&mut self, label: TacId) -> MirId {
        let v0 = self.expr(label);
        self.alloc(Mir::Op(Op::Jmp(v0)))
    }

    fn jump_on_zero(&mut self, lhs: TacId, label: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(label);
        if double {
            self.alloc(Mir::Op(Op::FBeqz(v0, v1)))
        } else {
            self.alloc(Mir::Op(Op::Beqz(v0, v1)))
        }
    }

    fn jump_on_not_zero(&mut self, lhs: TacId, label: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(label);
        if double {
            self.alloc(Mir::Op(Op::FBnez(v0, v1)))
        } else {
            self.alloc(Mir::Op(Op::Bnez(v0, v1)))
        }
    }

    fn is_double(&self, tac: TacId) -> bool {
        match &self.tac_arena[tac] {
            Tac::Operand(tac::Operand::Double(_)) => true,
            Tac::Op(tac::Op::IntToDouble { .. }) => true,
            Tac::Op(tac::Op::Add { ty, .. })
            | Tac::Op(tac::Op::Sub { ty, .. })
            | Tac::Op(tac::Op::Mul { ty, .. })
            | Tac::Op(tac::Op::Div { ty, .. })
            | Tac::Op(tac::Op::Mod { ty, .. })
            | Tac::Op(tac::Op::Neg { ty, .. })
            | Tac::Op(tac::Op::Copy { ty, .. })
            | Tac::Op(tac::Op::Truncate { ty, .. })
            | Tac::Op(tac::Op::SignExt { ty, .. })
            | Tac::Op(tac::Op::ZeroExt { ty, .. })
            | Tac::Op(tac::Op::Equal { ty, .. })
            | Tac::Op(tac::Op::NotEq { ty, .. })
            | Tac::Op(tac::Op::Less { ty, .. })
            | Tac::Op(tac::Op::LessOrEq { ty, .. })
            | Tac::Op(tac::Op::Greater { ty, .. })
            | Tac::Op(tac::Op::GreaterOrEq { ty, .. })
            | Tac::Op(tac::Op::And { ty, .. })
            | Tac::Op(tac::Op::Or { ty, .. })
            | Tac::Op(tac::Op::Xor { ty, .. })
            | Tac::Op(tac::Op::Not { ty, .. }) => is_double_type(ty),
            Tac::Operand(tac::Operand::Pseudo(ty, _)) => is_double_type(ty),
            Tac::Object(tac::Object::Data(ty, _, _, _, _)) => {
                is_double_type(ty)
            }
            Tac::Operand(tac::Operand::Sym(ty, _)) => is_double_type(ty),
            _ => false,
        }
    }

    fn classify_args(
        &self,
        args: &[TacId],
    ) -> (
        Vec<(TacId, usize)>,
        Vec<(TacId, usize)>,
        Vec<(TacId, usize)>,
        Vec<TacId>,
    ) {
        let mut gp_reg_args: Vec<(TacId, usize)> = Vec::new();
        let mut fp_reg_args: Vec<(TacId, usize)> = Vec::new();
        let mut gp_double_args: Vec<(TacId, usize)> = Vec::new();
        let mut stack_args: Vec<TacId> = Vec::new();
        let mut gp_idx = 0usize;
        let mut fp_idx = 0usize;

        for arg in args {
            if self.is_double(*arg) {
                if fp_idx < self.abi.max_fp_arg_regs() {
                    fp_reg_args.push((*arg, fp_idx));
                    fp_idx += 1;
                } else if gp_idx < self.abi.max_gp_arg_regs() {
                    gp_double_args.push((*arg, gp_idx));
                    gp_idx += 1;
                } else {
                    stack_args.push(*arg);
                }
            } else if gp_idx < self.abi.max_gp_arg_regs() {
                gp_reg_args.push((*arg, gp_idx));
                gp_idx += 1;
            } else {
                stack_args.push(*arg);
            }
        }

        (gp_reg_args, fp_reg_args, gp_double_args, stack_args)
    }

    fn call(
        &mut self,
        ty: &TypeRef,
        func: TacId,
        args: &[TacId],
        dst: TacId,
    ) -> MirId {
        let arg_regs = [
            Register::A0,
            Register::A1,
            Register::A2,
            Register::A3,
            Register::A4,
            Register::A5,
            Register::A6,
            Register::A7,
        ];
        let fp_arg_regs = [
            Register::FA0,
            Register::FA1,
            Register::FA2,
            Register::FA3,
            Register::FA4,
            Register::FA5,
            Register::FA6,
            Register::FA7,
        ];

        let (gp_reg_args, fp_reg_args, gp_double_args, stack_args) =
            self.classify_args(args);

        let stack_align = self.abi.stack_alignment();
        let stack_padding =
            (stack_align - (stack_args.len() * 8 % stack_align)) % stack_align;

        if stack_padding != 0 {
            let v0 = self.alloc(Mir::Op(Op::PushBytes(stack_padding)));
            self.emit(v0);
        }

        for (arg, idx) in gp_reg_args.iter() {
            let v1 = self.expr(*arg);
            let v2 = self.reg_for(arg_regs[*idx], v1);
            let op_size = self.operand_size(v2);
            let v3 = self.alloc(Mir::Op(Op::Mov(v1, v2, op_size)));
            self.emit(v3);
        }

        for (arg, idx) in fp_reg_args.iter() {
            let v1 = self.expr(*arg);
            let v2 = self.reg_for(fp_arg_regs[*idx], v1);
            let v3 = self.alloc(Mir::Op(Op::MovF(v1, v2)));
            self.emit(v3);
        }

        for (arg, idx) in gp_double_args.iter() {
            let v1 = self.expr(*arg);
            let v2 = self.reg_for(arg_regs[*idx], v1);
            let v3 = self.alloc(Mir::Op(Op::MovFToGp(v1, v2)));
            self.emit(v3);
        }

        for arg in stack_args.iter().rev() {
            let v1 = self.expr(*arg);
            if self.is_double(*arg) {
                let v2 = self.alloc(Mir::Op(Op::PushF(v1)));
                self.emit(v2);
            } else {
                let v2 = self.alloc(Mir::Op(Op::Push(v1, 8)));
                self.emit(v2);
            }
        }

        let v1 = self.expr(func);
        let v2 = self.alloc(Mir::Op(Op::Call(v1)));
        self.emit(v2);

        let pop_bytes = 8 * stack_args.len() + stack_padding;

        if pop_bytes > 0 {
            let v3 = self.alloc(Mir::Op(Op::PopBytes(pop_bytes)));
            self.emit(v3);
        }

        let v4 = self.expr(dst);

        if is_double_type(ty) {
            let v5 = self.reg_for(Register::FA0, v4);
            self.alloc(Mir::Op(Op::MovF(v5, v4)))
        } else {
            let v5 = self.reg_for(Register::A0, v4);
            let op_size = self.operand_size(v4);
            self.alloc(Mir::Op(Op::Mov(v5, v4, op_size)))
        }
    }

    fn load(&mut self, ty: &TypeRef, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let size = size_of(ty);
        let v2 = self.alloc(Mir::Operand(Operand::Mem(v0, 0), size));
        self.alloc(Mir::Op(Op::Load(v2, v1, size)))
    }

    fn lea(&mut self, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        self.alloc(Mir::Op(Op::Lea(v0, v1)))
    }

    fn store(&mut self, ty: &TypeRef, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let size = size_of(ty);
        let v2 = self.alloc(Mir::Operand(Operand::Mem(v1, 0), size));

        self.alloc(Mir::Op(Op::Store(v0, v2, size)))
    }

    fn return_stmt(&mut self, ty: &TypeRef, lhs: TacId) -> MirId {
        let v0 = self.expr(lhs);

        if is_double_type(ty) {
            let v1 = self.reg_for(Register::FA0, v0);
            if v0 != v1 {
                let v2 = self.alloc(Mir::Op(Op::MovF(v0, v1)));
                self.emit(v2);
            }
        } else {
            let v1 = self.reg_for(Register::A0, v0);
            if v0 != v1 {
                let op_size = self.operand_size(v1);
                let v2 = self.alloc(Mir::Op(Op::Mov(v0, v1, op_size)));
                self.emit(v2);
            }
        }

        self.alloc(Mir::Op(Op::Ret))
    }

    #[allow(unused_variables)]
    fn expr(&mut self, node: TacId) -> MirId {
        let node_data = self.tac_arena[node].clone();
        match &node_data {
            Tac::Operand(tac::Operand::Integer { ty, value }) => {
                self.integer(ty, *value)
            }
            Tac::Operand(tac::Operand::Double(value)) => self.double(*value),
            Tac::Operand(tac::Operand::Pseudo(ty, name)) => {
                self.stack_variable(ty, name)
            }
            Tac::Op(tac::Op::Inv { src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let size = self.operand_size(v1);
                self.alloc(Mir::Op(Op::Not(v0, v1, size)))
            }
            Tac::Op(tac::Op::Neg { ty: _, src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let size = self.operand_size(v1);
                self.alloc(Mir::Op(Op::Neg(v0, v1, size)))
            }
            Tac::Op(tac::Op::Not { ty: _, src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let size = self.operand_size(v0);
                let zero = self.alloc(Mir::Operand(Operand::Imm(0), size));
                self.alloc(Mir::Op(Op::CmpEq(v0, zero, v1, size)))
            }
            Tac::Op(tac::Op::Mul { ty, lhs, rhs, dst }) => {
                self.multiply(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Div { ty, lhs, rhs, dst }) => {
                self.divide(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Mod { ty, lhs, rhs, dst }) => {
                self.modulo(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Add { ty, lhs, rhs, dst }) => {
                self.add(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Sub { ty, lhs, rhs, dst }) => {
                self.subtract(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::LeftShift { lhs, rhs, dst }) => {
                self.shift_left(*lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::RightShift { ty, lhs, rhs, dst }) => {
                self.shift_right(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::And {
                ty: _,
                lhs,
                rhs,
                dst,
            }) => self.and(*lhs, *rhs, *dst),
            Tac::Op(tac::Op::Or {
                ty: _,
                lhs,
                rhs,
                dst,
            }) => self.or(*lhs, *rhs, *dst),
            Tac::Op(tac::Op::Xor { ty, lhs, rhs, dst }) => {
                self.xor(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Less { ty, lhs, rhs, dst }) => {
                self.less(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::LessOrEq { ty, lhs, rhs, dst }) => {
                self.less_or_eq(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Greater { ty, lhs, rhs, dst }) => {
                self.greater(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::GreaterOrEq { ty, lhs, rhs, dst }) => {
                self.greater_or_eq(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Equal { ty, lhs, rhs, dst }) => {
                self.equal(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::NotEq { ty, lhs, rhs, dst }) => {
                self.not_eq(ty, *lhs, *rhs, *dst)
            }
            Tac::Op(tac::Op::Copy { ty: _, src, dst }) => self.copy(*src, *dst),
            Tac::Op(tac::Op::CopyToOffset { ty, src, dst, off }) => {
                self.copy_to_offset(ty, *src, *dst, *off)
            }
            Tac::Op(tac::Op::AddPtr {
                lhs,
                rhs,
                scale,
                dst,
            }) => self.add_ptr(*lhs, *rhs, *scale, *dst),
            Tac::Op(tac::Op::Truncate { ty: _, src, dst }) => {
                self.truncate(*src, *dst)
            }
            Tac::Op(tac::Op::SignExt { ty: _, src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let src_size = self.operand_size(v0);
                let dst_size = self.operand_size(v1);
                if dst_size > src_size {
                    self.alloc(Mir::Op(Op::Movs(v0, v1)))
                } else {
                    self.mov(v0, v1)
                }
            }
            Tac::Op(tac::Op::ZeroExt { ty: _, src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let src_size = self.operand_size(v0);
                let dst_size = self.operand_size(v1);
                if dst_size > src_size {
                    self.alloc(Mir::Op(Op::Movz(v0, v1)))
                } else {
                    self.mov(v0, v1)
                }
            }
            Tac::Op(tac::Op::DoubleToInt { ty, src, dst }) => {
                self.double_to_int(ty, *src, *dst)
            }
            Tac::Op(tac::Op::DoubleToUlong { ty, src, dst }) => {
                self.double_to_ulong(ty, *src, *dst)
            }
            Tac::Op(tac::Op::IntToDouble { src, dst }) => {
                let signed = match &self.tac_arena[*src] {
                    Tac::Operand(tac::Operand::Pseudo(ty, _)) => is_signed(ty),
                    Tac::Operand(tac::Operand::Integer { ty, .. }) => is_signed(ty),
                    _ => true,
                };
                self.int_to_double(signed, *src, *dst)
            }
            Tac::Op(tac::Op::Jump(label)) => self.jump(*label),
            Tac::Op(tac::Op::JumpOnZero { expr, label }) => {
                self.jump_on_zero(*expr, *label)
            }
            Tac::Op(tac::Op::JumpOnNotZero { expr, label }) => {
                self.jump_on_not_zero(*expr, *label)
            }
            Tac::Op(tac::Op::Label(idx)) => self.label(*idx),
            Tac::Operand(tac::Operand::Sym(ty, name)) => self
                .alloc(Mir::Operand(Operand::Sym(name.clone()), size_of(ty))),
            Tac::Op(tac::Op::Call {
                ty,
                func,
                args,
                dst,
            }) => self.call(ty, *func, args, *dst),
            Tac::Op(tac::Op::GetAddr { ty: _, src, dst }) => {
                self.lea(*src, *dst)
            }
            Tac::Op(tac::Op::Load { ty, src, dst }) => {
                self.load(ty, *src, *dst)
            }
            Tac::Op(tac::Op::Store { ty, src, dst }) => {
                self.store(ty, *src, *dst)
            }
            Tac::Op(tac::Op::Return(ty, expr)) => self.return_stmt(ty, *expr),
            _ => {
                unreachable!()
            }
        }
    }

    fn function(
        &mut self,
        name: &str,
        global: bool,
        params: &[TacId],
        body: &[TacId],
    ) {
        let saved_var_map = std::mem::take(&mut self.var_map);
        let saved_label_map = std::mem::take(&mut self.label_map);
        let saved_stack_top = self.stack_top;
        let saved_mir_len = self.mir_ids.len();

        self.stack_top = -112;

        let arg_regs = [
            Register::A0,
            Register::A1,
            Register::A2,
            Register::A3,
            Register::A4,
            Register::A5,
            Register::A6,
            Register::A7,
        ];
        let fp_arg_regs = [
            Register::FA0,
            Register::FA1,
            Register::FA2,
            Register::FA3,
            Register::FA4,
            Register::FA5,
            Register::FA6,
            Register::FA7,
        ];

        let (gp_reg_params, fp_reg_params, gp_double_params, stack_params) =
            self.classify_args(params);

        for (param, idx) in gp_reg_params.iter() {
            let v0 = self.expr(*param);
            let v1 = self.reg_for(arg_regs[*idx], v0);
            let op_size = self.operand_size(v0);
            let v2 = self.alloc(Mir::Op(Op::Mov(v1, v0, op_size)));
            self.emit(v2);
        }

        for (param, idx) in fp_reg_params.iter() {
            let v0 = self.expr(*param);
            let v1 = self.reg_for(fp_arg_regs[*idx], v0);
            let v2 = self.alloc(Mir::Op(Op::MovF(v1, v0)));
            self.emit(v2);
        }

        for (param, idx) in gp_double_params.iter() {
            let v0 = self.expr(*param);
            let v1 = self.reg_for(arg_regs[*idx], v0);
            let op_size = self.operand_size(v0);
            let v2 = self.alloc(Mir::Op(Op::Mov(v1, v0, op_size)));
            self.emit(v2);
        }

        let s0 = self.alloc(Mir::Operand(Operand::Reg(Register::S0), 8));
        let mut off = 0;

        for param in stack_params.iter() {
            let v0 = self.expr(*param);
            let v1 = self.alloc(Mir::Operand(
                Operand::Mem(s0, off),
                self.operand_size(v0),
            ));
            if self.is_double(*param) {
                let v2 = self.alloc(Mir::Op(Op::MovF(v1, v0)));
                self.emit(v2);
            } else {
                let v2 =
                    self.alloc(Mir::Op(Op::Mov(v1, v0, self.operand_size(v0))));
                self.emit(v2);
            }
            off += 8;
        }

        for op in body {
            self.lower_node(*op);
        }

        let stack_align = self.abi.stack_alignment();
        let stack_size = ((-self.stack_top) as usize + (stack_align - 1))
            & !(stack_align - 1);

        let body_mir: Vec<MirId> =
            self.mir_ids.drain(saved_mir_len..).collect();

        self.var_map = saved_var_map;
        self.label_map = saved_label_map;
        self.stack_top = saved_stack_top;

        let func = self.alloc(Mir::Object(Object::Function {
            name: name.to_owned(),
            global,
            stack: stack_size,
            mir: body_mir,
        }));
        self.emit(func);
    }

    fn static_init(&mut self, init: TacId) -> Option<MirId> {
        if let Tac::Object(tac::Object::StaticInit(_ty, expr)) =
            &self.tac_arena[init]
        {
            match &self.tac_arena[*expr] {
                Tac::Operand(tac::Operand::Integer { ty, value }) => {
                    Some(self.alloc(Mir::Object(Object::InitInteger {
                        size: size_of(ty),
                        value: *value,
                    })))
                }
                Tac::Operand(tac::Operand::Double(value)) => {
                    Some(self.alloc(Mir::Object(Object::InitDouble(*value))))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn data(
        &mut self,
        name: &str,
        global: bool,
        read_only: bool,
        init: TacId,
        ty: &TypeRef,
    ) {
        let (init_list, alignment) = match &mut self.tac_arena.arena[init.0] {
            Tac::Object(tac::Object::StaticInitList(list, align)) => {
                (std::mem::take(list), *align)
            }
            _ => return,
        };

        let mut inits: Vec<MirId> = Vec::new();
        for item in &init_list {
            if let Some(mir) = self.static_init(*item) {
                inits.push(mir);
            }
        }

        if !inits.is_empty() {
            let sv = self.alloc(Mir::Object(Object::Data {
                name: name.to_owned(),
                global,
                read_only,
                inits,
                alignment,
                total_size: size_of(ty),
            }));
            self.emit(sv);
        }
    }

    fn lower_node(&mut self, node: TacId) {
        let node_data = self.tac_arena[node].clone();
        match &node_data {
            Tac::Object(tac::Object::Function {
                name,
                global,
                params,
                tac,
            }) => {
                self.function(name, *global, params, tac);
            }
            Tac::Object(tac::Object::Data(
                ty,
                name,
                global,
                read_only,
                init,
            )) => {
                self.data(name, *global, *read_only, *init, ty);
            }
            Tac::Op(..) => {
                let expr = self.expr(node);
                self.emit(expr);
            }
            _ => {
                println!("Got {:#?}", node);
                unreachable!()
            }
        }
    }

    fn alloc(&mut self, mir: Mir) -> MirId {
        let id = MirId(self.arena.len());
        self.arena.push(mir);
        id
    }

    fn emit(&mut self, mir: MirId) {
        self.mir_ids.push(mir);
    }
}

impl AbstractMirGenerator for MirGenerator {
    type Mir = MirStage;

    fn new() -> Self {
        Self {
            arena: vec![],
            mir_ids: vec![],
            var_map: VarMap::new(),
            label_map: LabelMap::new(),
            stack_top: 0,
            tac_arena: TacArena {
                arena: vec![],
                top_level: vec![],
            },
            abi: Lp64dAbi,
        }
    }

    fn lower(&mut self, stage: AirStage) -> Self::Mir {
        let mut tac = stage.tac;
        let top_level = std::mem::take(&mut tac.top_level);
        self.tac_arena = tac;
        for &tac_id in &top_level {
            self.lower_node(tac_id);
        }

        MirStage {
            mir: MirArena {
                arena: std::mem::take(&mut self.arena),
                top_level: std::mem::take(&mut self.mir_ids),
            },
        }
    }
}
