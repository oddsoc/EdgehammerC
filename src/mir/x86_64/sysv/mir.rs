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

use super::abi::SysvAbi;
use crate::air::tac::{self, AirStage, Tac, TacArena, TacId};
use crate::mir::MirGenerator as AbstractMirGenerator;
use crate::mir::abi::Abi;

use crate::types::{TypeRef, alignment_of, is_double_type, is_signed, size_of};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirId(pub usize);

#[derive(Debug, PartialEq, Clone)]
pub enum CondCode {
    Eq,
    NotEq,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
    Above,
    AboveOrEq,
    Below,
    BelowOrEq,
    Parity,
    NoParity,
}

impl std::fmt::Display for CondCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "e"),
            Self::NotEq => write!(f, "ne"),
            Self::Less => write!(f, "l"),
            Self::LessOrEq => write!(f, "le"),
            Self::Greater => write!(f, "g"),
            Self::GreaterOrEq => write!(f, "ge"),
            Self::Above => write!(f, "a"),
            Self::AboveOrEq => write!(f, "ae"),
            Self::Below => write!(f, "b"),
            Self::BelowOrEq => write!(f, "be"),
            Self::Parity => write!(f, "p"),
            Self::NoParity => write!(f, "np"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(unused)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rax => write!(f, "rax"),
            Self::Rbx => write!(f, "rbx"),
            Self::Rcx => write!(f, "rcx"),
            Self::Rdx => write!(f, "rdx"),
            Self::Rsi => write!(f, "rsi"),
            Self::Rdi => write!(f, "rdi"),
            Self::Rsp => write!(f, "rsp"),
            Self::Rbp => write!(f, "rbp"),
            Self::R8 => write!(f, "r8"),
            Self::R9 => write!(f, "r9"),
            Self::R10 => write!(f, "r10"),
            Self::R11 => write!(f, "r11"),
            Self::R12 => write!(f, "r12"),
            Self::R13 => write!(f, "r13"),
            Self::R14 => write!(f, "r14"),
            Self::R15 => write!(f, "r15"),
            Self::Xmm0 => write!(f, "xmm0"),
            Self::Xmm1 => write!(f, "xmm1"),
            Self::Xmm2 => write!(f, "xmm2"),
            Self::Xmm3 => write!(f, "xmm3"),
            Self::Xmm4 => write!(f, "xmm4"),
            Self::Xmm5 => write!(f, "xmm5"),
            Self::Xmm6 => write!(f, "xmm6"),
            Self::Xmm7 => write!(f, "xmm7"),
            Self::Xmm8 => write!(f, "xmm8"),
            Self::Xmm9 => write!(f, "xmm9"),
            Self::Xmm10 => write!(f, "xmm10"),
            Self::Xmm11 => write!(f, "xmm11"),
            Self::Xmm12 => write!(f, "xmm12"),
            Self::Xmm13 => write!(f, "xmm13"),
            Self::Xmm14 => write!(f, "xmm14"),
            Self::Xmm15 => write!(f, "xmm15"),
        }
    }
}

impl Register {
    pub fn name(self, size: usize) -> &'static str {
        match (self, size) {
            (Self::Rax, 1) => "%al",
            (Self::Rax, 2) => "%ax",
            (Self::Rax, 4) => "%eax",
            (Self::Rax, 8) => "%rax",
            (Self::Rbx, 1) => "%bl",
            (Self::Rbx, 2) => "%bx",
            (Self::Rbx, 4) => "%ebx",
            (Self::Rbx, 8) => "%rbx",
            (Self::Rcx, 1) => "%cl",
            (Self::Rcx, 2) => "%cx",
            (Self::Rcx, 4) => "%ecx",
            (Self::Rcx, 8) => "%rcx",
            (Self::Rdx, 1) => "%dl",
            (Self::Rdx, 2) => "%dx",
            (Self::Rdx, 4) => "%edx",
            (Self::Rdx, 8) => "%rdx",
            (Self::Rsi, 1) => "%sil",
            (Self::Rsi, 2) => "%si",
            (Self::Rsi, 4) => "%esi",
            (Self::Rsi, 8) => "%rsi",
            (Self::Rdi, 1) => "%dil",
            (Self::Rdi, 2) => "%di",
            (Self::Rdi, 4) => "%edi",
            (Self::Rdi, 8) => "%rdi",
            (Self::Rsp, 1) => "%spl",
            (Self::Rsp, 2) => "%sp",
            (Self::Rsp, 4) => "%esp",
            (Self::Rsp, 8) => "%rsp",
            (Self::Rbp, 1) => "%bpl",
            (Self::Rbp, 2) => "%bp",
            (Self::Rbp, 4) => "%ebp",
            (Self::Rbp, 8) => "%rbp",
            (Self::R8, 1) => "%r8b",
            (Self::R8, 2) => "%r8w",
            (Self::R8, 4) => "%r8d",
            (Self::R8, 8) => "%r8",
            (Self::R9, 1) => "%r9b",
            (Self::R9, 2) => "%r9w",
            (Self::R9, 4) => "%r9d",
            (Self::R9, 8) => "%r9",
            (Self::R10, 1) => "%r10b",
            (Self::R10, 2) => "%r10w",
            (Self::R10, 4) => "%r10d",
            (Self::R10, 8) => "%r10",
            (Self::R11, 1) => "%r11b",
            (Self::R11, 2) => "%r11w",
            (Self::R11, 4) => "%r11d",
            (Self::R11, 8) => "%r11",
            (Self::R12, 1) => "%r12b",
            (Self::R12, 2) => "%r12w",
            (Self::R12, 4) => "%r12d",
            (Self::R12, 8) => "%r12",
            (Self::R13, 1) => "%r13b",
            (Self::R13, 2) => "%r13w",
            (Self::R13, 4) => "%r13d",
            (Self::R13, 8) => "%r13",
            (Self::R14, 1) => "%r14b",
            (Self::R14, 2) => "%r14w",
            (Self::R14, 4) => "%r14d",
            (Self::R14, 8) => "%r14",
            (Self::R15, 1) => "%r15b",
            (Self::R15, 2) => "%r15w",
            (Self::R15, 4) => "%r15d",
            (Self::R15, 8) => "%r15",
            (Self::Xmm0, _) => "%xmm0",
            (Self::Xmm1, _) => "%xmm1",
            (Self::Xmm2, _) => "%xmm2",
            (Self::Xmm3, _) => "%xmm3",
            (Self::Xmm4, _) => "%xmm4",
            (Self::Xmm5, _) => "%xmm5",
            (Self::Xmm6, _) => "%xmm6",
            (Self::Xmm7, _) => "%xmm7",
            (Self::Xmm8, _) => "%xmm8",
            (Self::Xmm9, _) => "%xmm9",
            (Self::Xmm10, _) => "%xmm10",
            (Self::Xmm11, _) => "%xmm11",
            (Self::Xmm12, _) => "%xmm12",
            (Self::Xmm13, _) => "%xmm13",
            (Self::Xmm14, _) => "%xmm14",
            (Self::Xmm15, _) => "%xmm15",
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Imm(u64),
    Mem(MirId, i32),
    Idx {
        base: MirId,
        index: MirId,
        scale: usize,
        off: i32,
    },
    Reg(Register),
    Sym(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Ret,
    Lea(MirId, MirId, usize),
    Mov(MirId, MirId, usize),
    Movsd(MirId, MirId),
    MovAbs(MirId, MirId, usize),
    Movsx(MirId, MirId, usize),
    Cvttsd2si(MirId, MirId, usize),
    Cvtsi2sd(MirId, MirId, usize),
    Not(MirId, usize),
    Neg(MirId, usize),
    Imul(MirId, MirId, usize),
    Mul(MirId, usize),
    Idiv(MirId, usize),
    Div(MirId, usize),
    Add(MirId, MirId, usize),
    Sub(MirId, MirId, usize),
    Shl(MirId, MirId, usize),
    Shr(MirId, MirId, usize),
    Sar(MirId, MirId, usize),
    And(MirId, MirId, usize),
    Or(MirId, MirId, usize),
    Xor(MirId, MirId, usize),
    Xorpd(MirId, MirId),
    Cmp(MirId, MirId, usize),
    Ucomisd(MirId, MirId),
    Push(MirId, usize),
    Call(MirId),
    Label(usize),
    PushBytes(usize),
    PopBytes(usize),
    Jmp(MirId),
    Jnz(MirId),
    Jcc { cond: CondCode, label: MirId },
    Setcc { cond: CondCode, dst: MirId },
    Test(MirId, MirId),
    Cqo(usize),
    Addsd(MirId, MirId),
    Subsd(MirId, MirId),
    Mulsd(MirId, MirId),
    Divsd(MirId, MirId),
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
    abi: SysvAbi,
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

    fn mov_to_xmm_regs(&mut self, src: MirId, dst: MirId) -> (MirId, MirId) {
        let xmm0 = self.alloc(Mir::Operand(Operand::Reg(Register::Xmm0), 8));
        let xmm1 = self.alloc(Mir::Operand(Operand::Reg(Register::Xmm1), 8));

        let v0 = self.alloc(Mir::Op(Op::Movsd(src, xmm0)));
        self.emit(v0);
        let v1 = self.alloc(Mir::Op(Op::Movsd(dst, xmm1)));
        self.emit(v1);

        (xmm0, xmm1)
    }

    fn op_res(&mut self, op: MirId, ans: MirId, dst: MirId) -> MirId {
        if ans != dst {
            self.emit(op);
            self.mov(ans, dst)
        } else {
            op
        }
    }

    fn xorpd(&mut self, lhs: MirId, rhs: MirId, dst: MirId) -> MirId {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(lhs, rhs);
        let v0 = self.alloc(Mir::Op(Op::Xorpd(xmm1, xmm0)));
        self.emit(v0);
        self.alloc(Mir::Op(Op::Movsd(xmm0, dst)))
    }

    fn cmp(&mut self, lhs: MirId, rhs: MirId) -> MirId {
        let size = self.operand_size(lhs);
        self.alloc(Mir::Op(Op::Cmp(lhs, rhs, size)))
    }

    fn comisd(&mut self, lhs: MirId, rhs: MirId) -> MirId {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(lhs, rhs);
        self.alloc(Mir::Op(Op::Ucomisd(xmm0, xmm1)))
    }

    fn mulsd(&mut self, lhs: MirId, rhs: MirId, dst: MirId) -> MirId {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(rhs, lhs);
        let v0 = self.alloc(Mir::Op(Op::Mulsd(xmm0, xmm1)));
        self.emit(v0);
        self.alloc(Mir::Op(Op::Movsd(xmm1, dst)))
    }

    fn divsd(&mut self, lhs: MirId, rhs: MirId, dst: MirId) -> MirId {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(rhs, lhs);
        let v0 = self.alloc(Mir::Op(Op::Divsd(xmm0, xmm1)));
        self.emit(v0);
        self.alloc(Mir::Op(Op::Movsd(xmm1, dst)))
    }

    fn idiv(&mut self, src: MirId, size: usize) -> MirId {
        let src = if is_immediate(&self.arena[src.0]) {
            let r11 = self.alloc(Mir::Operand(
                Operand::Reg(Register::R11),
                self.operand_size(src),
            ));
            let v0 = self.mov(src, r11);
            self.emit(v0);
            r11
        } else {
            src
        };

        self.alloc(Mir::Op(Op::Idiv(src, size)))
    }

    fn div(&mut self, src: MirId, size: usize) -> MirId {
        let src = if is_immediate(&self.arena[src.0]) {
            let r11 = self.alloc(Mir::Operand(
                Operand::Reg(Register::R11),
                self.operand_size(src),
            ));
            let v0 = self.mov(src, r11);
            self.emit(v0);
            r11
        } else {
            src
        };

        let edx = self.alloc(Mir::Operand(Operand::Reg(Register::Rdx), 4));
        let v1 = self.alloc(Mir::Op(Op::Xor(edx, edx, 4)));
        self.emit(v1);

        self.alloc(Mir::Op(Op::Div(src, size)))
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
            let rbp = self.alloc(Mir::Operand(Operand::Reg(Register::Rbp), 8));
            let v0 = self.alloc(Mir::Operand(
                Operand::Mem(rbp, self.stack_top),
                ty_size,
            ));
            self.var_map.insert(name.to_string(), v0);
        }
        self.var_map[name]
    }

    fn invert(&mut self, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        let size = self.operand_size(v1);

        let v2 = self.mov(v0, v1);
        self.emit(v2);
        self.alloc(Mir::Op(Op::Not(v1, size)))
    }

    fn negate(&mut self, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        let size = self.operand_size(v1);

        let v2 = self.mov(v0, v1);
        self.emit(v2);
        self.alloc(Mir::Op(Op::Neg(v1, size)))
    }

    fn not(&mut self, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        let v2 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v0)));
        let v3 = self.cmp(v2, v0);
        self.emit(v3);
        let v4 = self.mov(v2, v1);
        self.emit(v4);
        self.alloc(Mir::Op(Op::Setcc {
            cond: CondCode::Eq,
            dst: v1,
        }))
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
            self.mulsd(v0, v1, v2)
        } else {
            let size = self.operand_size(v2);
            let signed = is_signed(ty);

            if signed {
                let v3 = self.mov(v0, v2);
                self.emit(v3);
                self.alloc(Mir::Op(Op::Imul(v1, v2, size)))
            } else {
                let rax =
                    self.alloc(Mir::Operand(Operand::Reg(Register::Rax), size));
                let v3 = self.mov(v0, rax);
                self.emit(v3);

                let v4 = if is_immediate(&self.arena[v1.0]) {
                    let r11 = self.alloc(Mir::Operand(
                        Operand::Reg(Register::R11),
                        self.operand_size(v1),
                    ));
                    let v5 = self.mov(v1, r11);
                    self.emit(v5);
                    r11
                } else {
                    v1
                };

                let v6 = self.alloc(Mir::Op(Op::Mul(v4, size)));
                self.emit(v6);
                let rax =
                    self.alloc(Mir::Operand(Operand::Reg(Register::Rax), size));
                self.mov(rax, v2)
            }
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
            self.divsd(v0, v1, v2)
        } else {
            let v3 = self.reg_for(Register::Rax, v0);
            let v4 = self.mov(v0, v3);
            self.emit(v4);
            let signed = is_signed(ty);
            if signed {
                let v5 = self.alloc(Mir::Op(Op::Cqo(size_of(ty))));
                self.emit(v5);
            }

            if signed {
                let v6 = self.idiv(v1, size_of(ty));
                self.emit(v6);
            } else {
                let v6 = self.div(v1, size_of(ty));
                self.emit(v6);
            }

            let v7 = self.reg_for(Register::Rax, v2);
            self.mov(v7, v2)
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
        let v1 = self.reg_for(Register::Rax, v0);
        let v2 = self.mov(v0, v1);
        self.emit(v2);
        let signed = is_signed(ty);

        if signed {
            let v3 = self.alloc(Mir::Op(Op::Cqo(size_of(ty))));
            self.emit(v3);
        }

        let v4 = self.expr(rhs);
        if signed {
            let v5 = self.idiv(v4, size_of(ty));
            self.emit(v5);
        } else {
            let v5 = self.div(v4, size_of(ty));
            self.emit(v5);
        }
        let v6 = self.expr(dst);
        let v7 = self.reg_for(Register::Rdx, v6);
        self.mov(v7, v6)
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
            let (v3, v4) = self.mov_to_xmm_regs(v1, v0);
            let v5 = self.alloc(Mir::Op(Op::Addsd(v3, v4)));
            self.emit(v5);
            self.alloc(Mir::Op(Op::Movsd(v4, v2)))
        } else {
            let size = self.operand_size(v2);

            let v3 = self.mov(v0, v2);
            self.emit(v3);
            self.alloc(Mir::Op(Op::Add(v1, v2, size)))
        }
    }

    fn add_ptr(
        &mut self,
        lhs: TacId,
        rhs: TacId,
        mut scale: usize,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let rbx = self.alloc(Mir::Operand(Operand::Reg(Register::Rbx), 8));
        let rdx = self.alloc(Mir::Operand(Operand::Reg(Register::Rdx), 8));

        let v3 = self.mov(v0, rbx);
        self.emit(v3);
        let v4 = self.mov(v1, rdx);
        self.emit(v4);

        if !matches!(scale, 1 | 2 | 4 | 8) {
            let size = self.operand_size(v1);
            let rax =
                self.alloc(Mir::Operand(Operand::Reg(Register::Rax), size));
            let r11 =
                self.alloc(Mir::Operand(Operand::Reg(Register::R11), size));
            let v5 = self.mov(rdx, rax);
            self.emit(v5);
            let v6 = self.alloc(Mir::Operand(
                Operand::Imm(scale as u64),
                self.operand_size(v1),
            ));
            let v7 = self.mov(v6, r11);
            self.emit(v7);
            let v8 = self.alloc(Mir::Op(Op::Mul(r11, size)));
            self.emit(v8);
            let v9 = self.mov(rax, rdx);
            self.emit(v9);
            scale = 1;
        }

        let v10 = self.alloc(Mir::Operand(
            Operand::Idx {
                base: rbx,
                index: rdx,
                scale,
                off: 0,
            },
            8,
        ));
        let v11 = self.alloc(Mir::Op(Op::Lea(v10, rbx, 8)));

        self.op_res(v11, rbx, v2)
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
            let (v3, v4) = self.mov_to_xmm_regs(v1, v0);
            let v5 = self.alloc(Mir::Op(Op::Subsd(v3, v4)));
            self.emit(v5);
            self.alloc(Mir::Op(Op::Movsd(v4, v2)))
        } else {
            let size = self.operand_size(v2);

            let v3 = self.mov(v0, v2);
            self.emit(v3);
            self.alloc(Mir::Op(Op::Sub(v1, v2, size)))
        }
    }

    fn shift_left(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);

        let v3 = self.mov(v0, v2);
        self.emit(v3);
        self.alloc(Mir::Op(Op::Shl(v1, v2, size)))
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

        let v3 = self.mov(v0, v2);
        self.emit(v3);

        if is_signed(ty) {
            self.alloc(Mir::Op(Op::Sar(v1, v2, size)))
        } else {
            self.alloc(Mir::Op(Op::Shr(v1, v2, size)))
        }
    }

    fn and(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);

        let v3 = self.mov(v0, v2);
        self.emit(v3);
        self.alloc(Mir::Op(Op::And(v1, v2, size)))
    }

    fn or(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let v2 = self.expr(dst);
        let size = self.operand_size(v2);

        let v3 = self.mov(v0, v2);
        self.emit(v3);
        self.alloc(Mir::Op(Op::Or(v1, v2, size)))
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
            self.xorpd(v0, v1, v2)
        } else {
            let size = self.operand_size(v2);

            let v3 = self.mov(v0, v2);
            self.emit(v3);
            self.alloc(Mir::Op(Op::Xor(v1, v2, size)))
        }
    }

    fn equal(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        self.emit(v2);
        let v3 = self.expr(dst);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        self.check_cond(CondCode::Eq, np_check, v3)
    }

    fn not_eq(&mut self, lhs: TacId, rhs: TacId, dst: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        self.emit(v2);
        let v3 = self.expr(dst);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        self.check_neq_cond(v3, np_check)
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
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        let v3 = self.expr(dst);
        self.emit(v2);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        let cond = if is_signed(ty) {
            CondCode::Less
        } else {
            CondCode::Below
        };
        self.check_cond(cond, np_check, v3)
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
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        let v3 = self.expr(dst);
        self.emit(v2);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        let cond = if is_signed(ty) {
            CondCode::LessOrEq
        } else {
            CondCode::BelowOrEq
        };
        self.check_cond(cond, np_check, v3)
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
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        let v3 = self.expr(dst);
        self.emit(v2);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        let cond = if is_signed(ty) {
            CondCode::Greater
        } else {
            CondCode::Above
        };
        self.check_cond(cond, np_check, v3)
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
        let mut np_check = false;
        let v2 = if double {
            np_check = true;
            self.comisd(v1, v0)
        } else {
            self.cmp(v1, v0)
        };
        let v3 = self.expr(dst);
        self.emit(v2);
        let v4 =
            self.alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v3)));
        let v5 = self.mov(v4, v3);
        self.emit(v5);
        let cond = if is_signed(ty) {
            CondCode::GreaterOrEq
        } else {
            CondCode::AboveOrEq
        };
        self.check_cond(cond, np_check, v3)
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
        let base_off = match &self.arena[v1.0] {
            Mir::Operand(Operand::Mem(_, base_off), _) => *base_off,
            _ => unreachable!(),
        };

        let rbp = self.alloc(Mir::Operand(Operand::Reg(Register::Rbp), 8));
        let v2 = self.alloc(Mir::Operand(
            Operand::Mem(rbp, base_off + off as i32),
            size_of(ty),
        ));

        if is_double_type(ty) {
            let xmm0 =
                self.alloc(Mir::Operand(Operand::Reg(Register::Xmm0), 8));
            let v3 = self.alloc(Mir::Op(Op::Movsd(v0, xmm0)));
            self.emit(v3);
            self.alloc(Mir::Op(Op::Movsd(xmm0, v2)))
        } else {
            self.mov(v0, v2)
        }
    }

    fn truncate(&mut self, lhs: TacId, dst: TacId) -> MirId {
        self.copy(lhs, dst)
    }

    fn int_to_double(&mut self, signed: bool, lhs: TacId, dst: TacId) -> MirId {
        let mut v0 = self.expr(lhs);
        let v1 = self.expr(dst);
        let conv_size = if signed { self.operand_size(v0) } else { 8 };

        if !is_register(&self.arena[v0.0]) {
            let r10 = self
                .alloc(Mir::Operand(Operand::Reg(Register::R10), conv_size));
            let v2 = self.mov(v0, r10);
            self.emit(v2);
            v0 = r10;
        }

        let xmm0 = self.alloc(Mir::Operand(Operand::Reg(Register::Xmm0), 8));
        let v3 = self.alloc(Mir::Op(Op::Cvtsi2sd(v0, xmm0, conv_size)));
        self.emit(v3);
        self.alloc(Mir::Op(Op::Movsd(xmm0, v1)))
    }

    fn double_to_int(&mut self, ty: &TypeRef, lhs: TacId, dst: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);

        if !is_register(&self.arena[v1.0]) {
            let r10 = self
                .alloc(Mir::Operand(Operand::Reg(Register::R10), size_of(ty)));
            let v2 = self.alloc(Mir::Op(Op::Cvttsd2si(v0, r10, size_of(ty))));
            self.emit(v2);
            self.mov(r10, v1)
        } else {
            self.alloc(Mir::Op(Op::Cvttsd2si(v0, v1, size_of(ty))))
        }
    }

    fn double_to_ulong(
        &mut self,
        ty: &TypeRef,
        lhs: TacId,
        dst: TacId,
    ) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(dst);

        let r10 =
            self.alloc(Mir::Operand(Operand::Reg(Register::R10), size_of(ty)));

        let v2 = self.alloc(Mir::Op(Op::Cvttsd2si(v0, r10, 8)));
        self.emit(v2);
        self.mov(r10, v1)
    }

    fn jump(&mut self, lhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        self.alloc(Mir::Op(Op::Jmp(v0)))
    }

    fn check_neq_cond(&mut self, dst: MirId, np: bool) -> MirId {
        if np {
            let v0 = self.alloc(Mir::Op(Op::Setcc {
                cond: CondCode::NotEq,
                dst,
            }));
            self.emit(v0);
            let r10 = self.alloc(Mir::Operand(Operand::Reg(Register::R10), 1));
            let v1 = self.alloc(Mir::Op(Op::Setcc {
                cond: CondCode::Parity,
                dst: r10,
            }));
            self.emit(v1);
            self.alloc(Mir::Op(Op::Or(r10, dst, 1)))
        } else {
            self.alloc(Mir::Op(Op::Setcc {
                cond: CondCode::NotEq,
                dst,
            }))
        }
    }

    fn check_cond(&mut self, cond: CondCode, np: bool, dst: MirId) -> MirId {
        let v0 = self.alloc(Mir::Op(Op::Setcc { cond, dst }));

        if np {
            self.emit(v0);
            let r11 = self.alloc(Mir::Operand(Operand::Reg(Register::R11), 1));
            let v1 = self.alloc(Mir::Op(Op::Setcc {
                cond: CondCode::NoParity,
                dst: r11,
            }));
            self.emit(v1);
            self.alloc(Mir::Op(Op::And(r11, dst, 1)))
        } else {
            v0
        }
    }

    fn jump_on_zero(&mut self, lhs: TacId, rhs: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let mut np_check = false;

        if double {
            let xmm1 =
                self.alloc(Mir::Operand(Operand::Reg(Register::Xmm1), 8));
            let v2 = self.alloc(Mir::Op(Op::Xorpd(xmm1, xmm1)));
            self.emit(v2);
            let v3 = self.comisd(v0, xmm1);
            self.emit(v3);
            np_check = true;
        } else {
            let imm = self
                .alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v0)));
            let v2 = self.cmp(imm, v0);
            self.emit(v2);
        }

        let r10 = self.alloc(Mir::Operand(Operand::Reg(Register::R10), 1));
        let v4 = self.check_cond(CondCode::Eq, np_check, r10);
        self.emit(v4);
        let v5 = self.alloc(Mir::Op(Op::Test(r10, r10)));
        self.emit(v5);

        self.alloc(Mir::Op(Op::Jnz(v1)))
    }

    fn jump_on_not_zero(&mut self, lhs: TacId, rhs: TacId) -> MirId {
        let double = self.is_double(lhs);
        let v0 = self.expr(lhs);
        let mut np_check = false;
        let v1 = self.expr(rhs);

        if double {
            let xmm1 =
                self.alloc(Mir::Operand(Operand::Reg(Register::Xmm1), 8));
            let v2 = self.alloc(Mir::Op(Op::Xorpd(xmm1, xmm1)));
            self.emit(v2);
            let v3 = self.comisd(v0, xmm1);
            self.emit(v3);
            let v4 = self.alloc(Mir::Op(Op::Jcc {
                cond: CondCode::Parity,
                label: v1,
            }));
            self.emit(v4);
            np_check = true;
        } else {
            let imm = self
                .alloc(Mir::Operand(Operand::Imm(0), self.operand_size(v0)));
            let v2 = self.cmp(imm, v0);
            self.emit(v2);
        }

        let r10 = self.alloc(Mir::Operand(Operand::Reg(Register::R10), 1));
        let v5 = self.check_cond(CondCode::NotEq, np_check, r10);
        self.emit(v5);
        let v6 = self.alloc(Mir::Op(Op::Test(r10, r10)));
        self.emit(v6);

        self.alloc(Mir::Op(Op::Jnz(v1)))
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
    ) -> (Vec<TacId>, Vec<TacId>, Vec<TacId>) {
        let mut gp_reg_args: Vec<TacId> = Vec::new();
        let mut fp_reg_args: Vec<TacId> = Vec::new();
        let mut stack_args: Vec<TacId> = Vec::new();

        for arg in args {
            if self.is_double(*arg) {
                if fp_reg_args.len() < self.abi.max_fp_arg_regs() {
                    fp_reg_args.push(*arg);
                } else {
                    stack_args.push(*arg);
                }
            } else {
                if gp_reg_args.len() < self.abi.max_gp_arg_regs() {
                    gp_reg_args.push(*arg);
                } else {
                    stack_args.push(*arg);
                }
            }
        }

        (gp_reg_args, fp_reg_args, stack_args)
    }

    fn call(
        &mut self,
        ty: &TypeRef,
        func: TacId,
        args: &[TacId],
        dst: TacId,
    ) -> MirId {
        let arg_regs = [
            Register::Rdi,
            Register::Rsi,
            Register::Rdx,
            Register::Rcx,
            Register::R8,
            Register::R9,
        ];
        let fp_arg_regs = [
            Register::Xmm0,
            Register::Xmm1,
            Register::Xmm2,
            Register::Xmm3,
            Register::Xmm4,
            Register::Xmm5,
            Register::Xmm6,
            Register::Xmm7,
        ];

        let (gp_reg_args, fp_reg_args, stack_args) = self.classify_args(args);

        let stack_align = self.abi.stack_alignment();
        let stack_padding =
            (stack_align - (stack_args.len() * 8 % stack_align)) % stack_align;

        if stack_padding != 0 {
            let v0 = self.alloc(Mir::Op(Op::PushBytes(stack_padding)));
            self.emit(v0);
        }

        for (i, arg) in gp_reg_args.iter().enumerate() {
            let v1 = self.expr(*arg);
            let v2 = self.reg_for(arg_regs[i], v1);
            let op_size = self.operand_size(v2);
            let v3 = self.alloc(Mir::Op(Op::Mov(v1, v2, op_size)));
            self.emit(v3);
        }

        for (i, arg) in fp_reg_args.iter().enumerate() {
            let v1 = self.expr(*arg);
            let v2 = self.reg_for(fp_arg_regs[i], v1);
            let v3 = self.alloc(Mir::Op(Op::Movsd(v1, v2)));
            self.emit(v3);
        }

        for arg in stack_args.iter().rev() {
            let v1 = self.expr(*arg);
            if is_register(&self.arena[v1.0]) || is_immediate(&self.arena[v1.0])
            {
                let v2 = self.push(v1);
                self.emit(v2);
            } else {
                let v2 = self.reg_for(Register::Rax, v1);
                let op_size = self.operand_size(v2);
                let v3 = self.alloc(Mir::Op(Op::Mov(v1, v2, op_size)));
                self.emit(v3);
                let rax =
                    self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));
                let v4 = self.alloc(Mir::Op(Op::Push(rax, 8)));
                self.emit(v4);
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
            let v5 = self.reg_for(Register::Xmm0, v4);
            self.alloc(Mir::Op(Op::Movsd(v5, v4)))
        } else {
            let v5 = self.reg_for(Register::Rax, v4);
            let op_size = self.operand_size(v4);
            self.alloc(Mir::Op(Op::Mov(v5, v4, op_size)))
        }
    }

    fn push(&mut self, src: MirId) -> MirId {
        if is_large_immediate(&self.arena[src.0]) {
            let r11 = self.alloc(Mir::Operand(Operand::Reg(Register::R11), 8));
            let v0 = self.mov(src, r11);
            self.emit(v0);
            self.alloc(Mir::Op(Op::Push(r11, 8)))
        } else {
            self.alloc(Mir::Op(Op::Push(src, 8)))
        }
    }

    fn load(&mut self, ty: &TypeRef, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let rax = self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));

        let v2 = self.mov(v0, rax);
        self.emit(v2);

        let rax = self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));
        let v3 = self.alloc(Mir::Operand(Operand::Mem(rax, 0), size_of(ty)));

        self.mov(v3, v1)
    }

    fn lea(&mut self, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let rax = self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));

        let v2 = self.alloc(Mir::Op(Op::Lea(v0, rax, 8)));
        self.op_res(v2, rax, v1)
    }

    fn store(&mut self, ty: &TypeRef, lhs: TacId, rhs: TacId) -> MirId {
        let v0 = self.expr(lhs);
        let v1 = self.expr(rhs);
        let rax = self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));

        let v2 = self.mov(v1, rax);
        self.emit(v2);

        let rax = self.alloc(Mir::Operand(Operand::Reg(Register::Rax), 8));
        let v3 = self.alloc(Mir::Operand(Operand::Mem(rax, 0), size_of(ty)));

        let v4 = if is_register(&self.arena[v0.0]) {
            v0
        } else {
            let r11 = self.alloc(Mir::Operand(
                Operand::Reg(Register::R11),
                self.operand_size(v0),
            ));
            let v5 = self.mov(v0, r11);
            self.emit(v5);
            r11
        };

        self.mov(v4, v3)
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
            Tac::Op(tac::Op::Inv { src, dst }) => self.invert(*src, *dst),
            Tac::Op(tac::Op::Neg { ty: _, src, dst }) => {
                self.negate(*src, *dst)
            }
            Tac::Op(tac::Op::Not { ty: _, src, dst }) => self.not(*src, *dst),
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
            Tac::Op(tac::Op::Equal {
                ty: _,
                lhs,
                rhs,
                dst,
            }) => self.equal(*lhs, *rhs, *dst),
            Tac::Op(tac::Op::NotEq {
                ty: _,
                lhs,
                rhs,
                dst,
            }) => self.not_eq(*lhs, *rhs, *dst),
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
            Tac::Op(tac::Op::SignExt { ty, src, dst }) => {
                let v0 = self.expr(*src);
                let v1 = self.expr(*dst);
                let src_size = self.operand_size(v0);
                let dst_size = self.operand_size(v1);
                if dst_size > src_size {
                    let src_imm = is_immediate(&self.arena[v0.0]);
                    let v2 = self.reg_for(Register::R11, v1);
                    let v3 = if dst_size == 8 && src_imm {
                        self.alloc(Mir::Op(Op::MovAbs(v0, v2, dst_size)))
                    } else {
                        self.alloc(Mir::Op(Op::Movsx(v0, v2, dst_size)))
                    };
                    self.emit(v3);
                    let r11 = self
                        .alloc(Mir::Operand(Operand::Reg(Register::R11), 8));
                    self.mov(r11, v1)
                } else {
                    self.mov(v0, v1)
                }
            }
            Tac::Op(tac::Op::ZeroExt { ty: _, src, dst }) => {
                self.copy(*src, *dst)
            }
            Tac::Op(tac::Op::DoubleToInt { ty, src, dst }) => {
                self.double_to_int(ty, *src, *dst)
            }
            Tac::Op(tac::Op::DoubleToUlong { ty, src, dst }) => {
                self.double_to_ulong(ty, *src, *dst)
            }
            Tac::Op(tac::Op::IntToDouble { src, dst }) => {
                let signed = matches!(&self.tac_arena[*src], Tac::Operand(tac::Operand::Pseudo(ty, _)) if is_signed(ty));
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
            Tac::Op(tac::Op::GetAddr { ty, src, dst }) => self.lea(*src, *dst),
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

        self.stack_top = 0;

        let arg_regs = [
            Register::Rdi,
            Register::Rsi,
            Register::Rdx,
            Register::Rcx,
            Register::R8,
            Register::R9,
        ];
        let fp_arg_regs = [
            Register::Xmm0,
            Register::Xmm1,
            Register::Xmm2,
            Register::Xmm3,
            Register::Xmm4,
            Register::Xmm5,
            Register::Xmm6,
            Register::Xmm7,
        ];

        let (gp_reg_params, fp_reg_params, stack_params) =
            self.classify_args(params);

        for (i, param) in gp_reg_params.iter().enumerate() {
            let v0 = self.expr(*param);
            let v1 = self.reg_for(arg_regs[i], v0);
            let op_size = self.operand_size(v0);
            let v2 = self.alloc(Mir::Op(Op::Mov(v1, v0, op_size)));
            self.emit(v2);
        }

        for (i, param) in fp_reg_params.iter().enumerate() {
            let v0 = self.expr(*param);
            let v1 = self.reg_for(fp_arg_regs[i], v0);
            let v2 = self.alloc(Mir::Op(Op::Movsd(v1, v0)));
            self.emit(v2);
        }

        let mut off = 16;

        for param in stack_params.iter() {
            let v0 = self.expr(*param);
            let rbp = self.alloc(Mir::Operand(Operand::Reg(Register::Rbp), 8));
            let v1 = self.alloc(Mir::Operand(
                Operand::Mem(rbp, off),
                self.operand_size(v0),
            ));

            let v2 = self.reg_for(Register::R10, v1);
            let v3 = self.mov(v1, v2);
            self.emit(v3);
            let v4 = self.mov(v2, v0);
            self.emit(v4);
            off += 8;
        }

        for op in body {
            self.lower_node(*op);
        }

        let stack_align = self.abi.stack_alignment();
        let stack_size = ((-self.stack_top) as usize + (stack_align - 1))
            & !(stack_align - 1);

        let mut body_mir: Vec<MirId> =
            self.mir_ids.drain(saved_mir_len..).collect();
        self.fixup_function_body(&mut body_mir);

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

    fn return_stmt(&mut self, ty: &TypeRef, lhs: TacId) -> MirId {
        let v0 = self.expr(lhs);

        if is_double_type(ty) {
            let v1 = self.reg_for(Register::Xmm0, v0);
            if v0 != v1 {
                let v2 = self.alloc(Mir::Op(Op::Movsd(v0, v1)));
                self.emit(v2);
            }
        } else {
            let v1 = self.reg_for(Register::Rax, v0);
            if v0 != v1 {
                let op_size = self.operand_size(v1);
                let v2 = self.alloc(Mir::Op(Op::Mov(v0, v1, op_size)));
                self.emit(v2);
            }
        }

        self.alloc(Mir::Op(Op::Ret))
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

    fn mov_fixup(
        &mut self,
        result: &mut Vec<MirId>,
        src: MirId,
        dst: MirId,
        size: usize,
    ) {
        let dst_is_mem = is_memory(&self.arena[dst.0]);
        let src_is_large_imm = is_large_immediate(&self.arena[src.0]);
        let src_size = self.operand_size(src);
        let dst_size = size;

        let v0 = if src_is_large_imm && src_size > dst_size {
            match &self.arena[src.0] {
                Mir::Operand(Operand::Imm(val), _) => {
                    let mask = if dst_size >= 8 {
                        !0u64
                    } else {
                        (1u64 << (dst_size * 8)) - 1
                    };
                    self.alloc(Mir::Operand(Operand::Imm(val & mask), dst_size))
                }
                _ => unreachable!(),
            }
        } else {
            src
        };

        let v0_is_large_imm = is_large_immediate(&self.arena[v0.0]);
        let v0_is_mem = is_memory(&self.arena[v0.0]);

        let v1 = if (v0_is_mem || v0_is_large_imm) && dst_is_mem {
            let load_size = src_size.min(dst_size);
            let r11 = self
                .alloc(Mir::Operand(Operand::Reg(Register::R11), load_size));
            result.push(self.alloc(Mir::Op(Op::Mov(v0, r11, load_size))));
            r11
        } else {
            v0
        };

        let v1_size = self.operand_size(v1);

        if dst_size > v1_size {
            let v1_imm = is_immediate(&self.arena[v1.0]);

            let v2 = self.reg_for(Register::R11, dst);

            if dst_size == 8 && v1_imm {
                result.push(self.alloc(Mir::Op(Op::MovAbs(v1, v2, dst_size))));
            } else {
                let r11d =
                    self.alloc(Mir::Operand(Operand::Reg(Register::R11), 4));
                result.push(self.alloc(Mir::Op(Op::Mov(v1, r11d, v1_size))));
            }

            let r11 = self.alloc(Mir::Operand(Operand::Reg(Register::R11), 8));
            result.push(self.alloc(Mir::Op(Op::Mov(r11, dst, dst_size))));
        } else {
            result.push(self.alloc(Mir::Op(Op::Mov(v1, dst, dst_size))));
        }
    }

    fn shift_fixup(
        &mut self,
        result: &mut Vec<MirId>,
        src: MirId,
        dst: MirId,
        size: usize,
        variant: fn(MirId, MirId, usize) -> Op,
    ) {
        let src_is_imm = is_immediate(&self.arena[src.0]);
        let src_is_reg = is_register(&self.arena[src.0]);

        let v0 = if !src_is_imm && !src_is_reg {
            let cl = self.alloc(Mir::Operand(Operand::Reg(Register::Rcx), 1));
            result.push(self.alloc(Mir::Op(Op::Mov(src, cl, 1))));
            cl
        } else if src_is_reg && self.operand_size(src) == 1 {
            src
        } else if src_is_reg {
            let cl = self.alloc(Mir::Operand(Operand::Reg(Register::Rcx), 1));
            result.push(self.alloc(Mir::Op(Op::Mov(src, cl, 1))));
            cl
        } else {
            src
        };

        let id = MirId(self.arena.len());
        self.arena.push(Mir::Op(variant(v0, dst, size)));
        result.push(id);
    }

    fn imul_fixup(
        &mut self,
        result: &mut Vec<MirId>,
        src: MirId,
        dst: MirId,
        size: usize,
    ) {
        let dst_is_mem = is_memory(&self.arena[dst.0]);
        let src_is_imm = is_immediate(&self.arena[src.0]);

        let v0 = if dst_is_mem {
            let v1 = self.reg_for(Register::R10, dst);
            let load_size = self.operand_size(dst);
            result.push(self.alloc(Mir::Op(Op::Mov(dst, v1, load_size))));
            v1
        } else {
            dst
        };

        let v2 = if src_is_imm {
            let v3 = self.reg_for(Register::R11, src);
            let load_size = self.operand_size(src);
            result.push(self.alloc(Mir::Op(Op::Mov(src, v3, load_size))));
            v3
        } else {
            src
        };

        let id = MirId(self.arena.len());
        self.arena.push(Mir::Op(Op::Imul(v2, v0, size)));
        result.push(id);

        if dst_is_mem && v0 != dst {
            result.push(self.alloc(Mir::Op(Op::Mov(v0, dst, size))));
        }
    }

    fn alu_fixup(
        &mut self,
        result: &mut Vec<MirId>,
        src: MirId,
        dst: MirId,
        size: usize,
        variant: fn(MirId, MirId, usize) -> Op,
    ) {
        let dst_is_imm = is_immediate(&self.arena[dst.0]);

        let v0 = if dst_is_imm {
            let v1 = self.reg_for(Register::R10, dst);
            let load_size = self.operand_size(dst);
            result.push(self.alloc(Mir::Op(Op::Mov(dst, v1, load_size))));
            v1
        } else {
            dst
        };

        let src_is_mem = is_memory(&self.arena[src.0]);
        let src_is_large_imm = is_large_immediate(&self.arena[src.0]);

        let v2 = if (src_is_mem && is_memory(&self.arena[v0.0]))
            || src_is_large_imm
        {
            let v3 = self.reg_for(Register::R11, src);
            let load_size = self.operand_size(src);
            result.push(self.alloc(Mir::Op(Op::Mov(src, v3, load_size))));
            v3
        } else {
            src
        };

        let id = MirId(self.arena.len());
        self.arena.push(Mir::Op(variant(v2, v0, size)));
        result.push(id);
    }

    fn fixup_function_body(&mut self, body: &mut Vec<MirId>) {
        let prev_mir = std::mem::take(body);
        for mir_id in prev_mir {
            let mir = &self.arena[mir_id.0];
            match mir {
                Mir::Op(Op::Mov(src, dst, size)) => {
                    self.mov_fixup(body, *src, *dst, *size);
                }
                Mir::Op(Op::Add(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::Add);
                }
                Mir::Op(Op::Sub(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::Sub);
                }
                Mir::Op(Op::And(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::And);
                }
                Mir::Op(Op::Or(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::Or);
                }
                Mir::Op(Op::Xor(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::Xor);
                }
                Mir::Op(Op::Imul(src, dst, size)) => {
                    self.imul_fixup(body, *src, *dst, *size);
                }
                Mir::Op(Op::Cmp(src, dst, size)) => {
                    self.alu_fixup(body, *src, *dst, *size, Op::Cmp);
                }
                Mir::Op(Op::Shl(src, dst, size)) => {
                    self.shift_fixup(body, *src, *dst, *size, Op::Shl);
                }
                Mir::Op(Op::Shr(src, dst, size)) => {
                    self.shift_fixup(body, *src, *dst, *size, Op::Shr);
                }
                Mir::Op(Op::Sar(src, dst, size)) => {
                    self.shift_fixup(body, *src, *dst, *size, Op::Sar);
                }
                _ => {
                    body.push(mir_id);
                }
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

    #[allow(dead_code)]
    fn out(&mut self) -> Vec<MirId> {
        std::mem::take(&mut self.mir_ids)
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
            abi: SysvAbi,
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

fn is_immediate(mir: &Mir) -> bool {
    matches!(mir, Mir::Operand(Operand::Imm(_), _))
}

fn is_large_immediate(mir: &Mir) -> bool {
    match mir {
        Mir::Operand(Operand::Imm(val), size) => {
            if *size > 4 {
                return true;
            }

            if *val > i32::MAX as u64 {
                return true;
            }

            false
        }
        _ => false,
    }
}

fn is_memory(mir: &Mir) -> bool {
    matches!(
        mir,
        Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _)
            | Mir::Operand(Operand::Idx { .. }, _)
    )
}

fn is_register(mir: &Mir) -> bool {
    matches!(mir, Mir::Operand(Operand::Reg(_), _))
}
