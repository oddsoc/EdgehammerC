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

use std::fmt::Write as FmtWrite;
use std::path::Path;

use crate::asm::syntax::pad_inst;
use crate::mir::rv64::lp64d::mir::{
    Mir, MirArena, MirId, MirStage, Object, Op, Operand, Register,
};

const GP_SCRATCH: [Register; 18] = [
    Register::T0,
    Register::T1,
    Register::T2,
    Register::T3,
    Register::T4,
    Register::T5,
    Register::T6,
    Register::S1,
    Register::S2,
    Register::S3,
    Register::S4,
    Register::S5,
    Register::S6,
    Register::S7,
    Register::S8,
    Register::S9,
    Register::S10,
    Register::S11,
];

const FP_SCRATCH: [Register; 8] = [
    Register::FT0,
    Register::FT1,
    Register::FT2,
    Register::FT3,
    Register::FT8,
    Register::FT9,
    Register::FT10,
    Register::FT11,
];

fn operand_size(arena: &MirArena, id: MirId) -> usize {
    match &arena[id] {
        Mir::Operand(_, size) => *size,
        _ => unreachable!(),
    }
}

fn is_init_zero(arena: &MirArena, init: MirId) -> bool {
    match &arena[init] {
        Mir::Object(Object::InitInteger { value, .. }) => *value == 0,
        Mir::Object(Object::InitDouble(v)) => v.to_bits() == 0,
        _ => false,
    }
}

fn fits_signed_12(val: u64) -> bool {
    let v = val as i64;
    v >= -2048 && v <= 2047
}

fn is_pow2(n: usize) -> bool {
    n != 0 && (n & (n - 1)) == 0
}

fn collect_pinned(arena: &MirArena, ids: &[MirId]) -> Vec<Register> {
    let mut out = Vec::new();
    for id in ids {
        pin_operand(arena, *id, &mut out);
    }
    out
}

fn pin_operand(arena: &MirArena, id: MirId, out: &mut Vec<Register>) {
    match &arena[id] {
        Mir::Operand(Operand::Reg(r), _) => {
            if !out.contains(r) {
                out.push(*r);
            }
        }
        Mir::Operand(Operand::Mem(base, _), _) => {
            pin_operand(arena, *base, out);
        }
        _ => {}
    }
}

struct Emitter<'a> {
    out: String,
    arena: &'a MirArena,
    gp: Vec<Register>,
    fp: Vec<Register>,
}

impl<'a> Emitter<'a> {
    fn new(arena: &'a MirArena) -> Self {
        Self {
            out: String::new(),
            arena,
            gp: Vec::new(),
            fp: Vec::new(),
        }
    }

    fn begin(&mut self, pinned: Vec<Register>) {
        self.gp = GP_SCRATCH
            .iter()
            .copied()
            .filter(|r| !pinned.contains(r))
            .collect();
        self.fp = FP_SCRATCH
            .iter()
            .copied()
            .filter(|r| !pinned.contains(r))
            .collect();
    }

    fn alloc_gp(&mut self) -> Register {
        match self.gp.pop() {
            Some(r) => r,
            None => unreachable!("no gp scratch available"),
        }
    }

    fn alloc_fp(&mut self) -> Register {
        match self.fp.pop() {
            Some(r) => r,
            None => unreachable!("no fp scratch available"),
        }
    }

    fn inst_line(&mut self, s: &str) {
        self.out.push_str(&pad_inst(s));
        self.out.push('\n');
    }

    fn line(&mut self, s: &str) {
        self.inst_line(s);
    }

    fn line_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        let mut s = String::new();
        let _ = s.write_fmt(args);
        self.inst_line(&s);
    }

    fn li(&mut self, reg: Register, val: u64) {
        if fits_signed_12(val) {
            self.line_fmt(format_args!("addi {}, zero, {}", reg, val as i64));
        } else {
            self.line_fmt(format_args!("li {}, {}", reg, val as i64));
        }
    }

    fn label_str(arena: &MirArena, id: MirId) -> String {
        match &arena[id] {
            Mir::Op(Op::Label(idx)) => format!(".L{}", idx),
            _ => unreachable!("label expected"),
        }
    }

    fn data_ptr(&mut self, id: MirId) -> Register {
        match &self.arena[id] {
            Mir::Operand(Operand::Sym(name), _) => {
                let t = self.alloc_gp();
                self.line_fmt(format_args!("la {}, {}", t, name));
                t
            }
            Mir::Operand(Operand::Mem(base, off), _) => {
                let base_reg = match &self.arena[*base] {
                    Mir::Operand(Operand::Reg(r), _) => *r,
                    _ => self.gp(*base),
                };
                let t = self.alloc_gp();
                if *off == 0 {
                    t
                } else if (*off as i64) >= -2048 && (*off as i64) <= 2047 {
                    self.line_fmt(format_args!(
                        "addi {}, {}, {}",
                        t, base_reg, off
                    ));
                    t
                } else {
                    let tmp = self.alloc_gp();
                    self.li(tmp, *off as u64);
                    self.line_fmt(format_args!(
                        "add {}, {}, {}",
                        t, base_reg, tmp
                    ));
                    t
                }
            }
            _ => unreachable!("cannot take address of operand"),
        }
    }

    fn s0_offset(&self, id: MirId) -> Option<i32> {
        match &self.arena[id] {
            Mir::Operand(Operand::Mem(base, off), _) => {
                match &self.arena[*base] {
                    Mir::Operand(Operand::Reg(r), _) if *r == Register::S0 => {
                        if fits_signed_12(*off as u64) {
                            Some(*off)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn mem_ref(&mut self, id: MirId) -> String {
        match self.s0_offset(id) {
            Some(off) => format!("{}(s0)", off),
            None => {
                let p = self.data_ptr(id);
                format!("0({})", p)
            }
        }
    }

    fn op_load(&mut self, reg: Register, size: usize, id: MirId) {
        let m = match size {
            1 => "lb",
            2 => "lh",
            4 => "lw",
            8 => "ld",
            _ => unreachable!("unsupported load size {}", size),
        };
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("{} {}, {}", m, reg, mr));
    }

    fn op_load_zero(&mut self, reg: Register, size: usize, id: MirId) {
        let m = match size {
            1 => "lbu",
            2 => "lhu",
            4 => "lwu",
            8 => "ld",
            _ => unreachable!("unsupported load size {}", size),
        };
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("{} {}, {}", m, reg, mr));
    }

    fn op_sign_load(&mut self, reg: Register, size: usize, id: MirId) {
        let m = match size {
            1 => "lb",
            2 => "lh",
            4 => "lw",
            8 => "ld",
            _ => unreachable!("unsupported load size {}", size),
        };
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("{} {}, {}", m, reg, mr));
    }

    fn op_store(&mut self, reg: Register, size: usize, id: MirId) {
        let m = match size {
            1 => "sb",
            2 => "sh",
            4 => "sw",
            8 => "sd",
            _ => unreachable!("unsupported store size {}", size),
        };
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("{} {}, {}", m, reg, mr));
    }

    fn op_fld(&mut self, reg: Register, id: MirId) {
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("fld {}, {}", reg, mr));
    }

    fn op_fsd(&mut self, reg: Register, id: MirId) {
        let mr = self.mem_ref(id);
        self.line_fmt(format_args!("fsd {}, {}", reg, mr));
    }

    fn gp(&mut self, id: MirId) -> Register {
        match &self.arena[id] {
            Mir::Operand(Operand::Reg(reg), _) if !reg.is_fp() => *reg,
            Mir::Operand(Operand::Reg(_), _) => {
                unreachable!("fp register in integer context")
            }
            Mir::Operand(Operand::Imm(val), _) => {
                let t = self.alloc_gp();
                self.li(t, *val);
                t
            }
            Mir::Operand(Operand::Mem(_, _), size)
            | Mir::Operand(Operand::Sym(_), size) => {
                let t = self.alloc_gp();
                self.op_load(t, *size, id);
                t
            }
            _ => unreachable!(),
        }
    }

    fn fp(&mut self, id: MirId) -> Register {
        match &self.arena[id] {
            Mir::Operand(Operand::Reg(reg), _) if reg.is_fp() => *reg,
            Mir::Operand(Operand::Imm(val), _) => {
                let d = self.alloc_fp();
                if *val == 0 {
                    self.line_fmt(format_args!("fmv.d.x {}, zero", d));
                } else {
                    let t = self.alloc_gp();
                    self.li(t, *val);
                    self.line_fmt(format_args!("fmv.d.x {}, {}", d, t));
                }
                d
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                let d = self.alloc_fp();
                self.op_fld(d, id);
                d
            }
            _ => unreachable!(),
        }
    }

    fn load(&mut self, dst: Register, size: usize, base: Register) {
        match size {
            1 => {
                self.line_fmt(format_args!("lb {}, 0({})", dst, base));
            }
            2 => {
                self.line_fmt(format_args!("lh {}, 0({})", dst, base));
            }
            4 => {
                self.line_fmt(format_args!("lw {}, 0({})", dst, base));
            }
            8 => {
                self.line_fmt(format_args!("ld {}, 0({})", dst, base));
            }
            _ => unreachable!(),
        }
    }

    fn store(&mut self, src: Register, size: usize, base: Register) {
        match size {
            1 => {
                self.line_fmt(format_args!("sb {}, 0({})", src, base));
            }
            2 => {
                self.line_fmt(format_args!("sh {}, 0({})", src, base));
            }
            4 => {
                self.line_fmt(format_args!("sw {}, 0({})", src, base));
            }
            8 => {
                self.line_fmt(format_args!("sd {}, 0({})", src, base));
            }
            _ => unreachable!(),
        }
    }

    fn dst_reg(&mut self, dst: MirId) -> Register {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(r), _) => *r,
            _ => self.alloc_gp(),
        }
    }

    fn dst_reg_fp(&mut self, dst: MirId) -> Register {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(r), _) => *r,
            _ => self.alloc_fp(),
        }
    }

    fn store_result(&mut self, src: Register, dst: MirId) {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(reg), _) => {
                if *reg != src {
                    self.line_fmt(format_args!("mv {}, {}", reg, src));
                }
            }
            Mir::Operand(Operand::Mem(_, _), size)
            | Mir::Operand(Operand::Sym(_), size) => {
                self.op_store(src, *size, dst);
            }
            _ => unreachable!(),
        }
    }

    fn store_fp_result(&mut self, src: Register, dst: MirId) {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(reg), _) => {
                if *reg != src {
                    self.line_fmt(format_args!("fmv.d {}, {}", reg, src));
                }
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                self.op_fsd(src, dst);
            }
            _ => unreachable!(),
        }
    }

    fn mov_into_reg(&mut self, d: Register, src: MirId) {
        match &self.arena[src] {
            Mir::Operand(Operand::Reg(reg), _) => {
                if *reg != d {
                    self.line_fmt(format_args!("mv {}, {}", d, reg));
                }
            }
            Mir::Operand(Operand::Imm(val), _) => {
                self.li(d, *val);
            }
            Mir::Operand(Operand::Mem(_, _), size)
            | Mir::Operand(Operand::Sym(_), size) => {
                self.op_load(d, *size, src);
            }
            _ => unreachable!(),
        }
    }

    fn mov_to_mem(&mut self, src: MirId, dst: MirId, size: usize) {
        match &self.arena[src] {
            Mir::Operand(Operand::Imm(val), _) => {
                let t = self.alloc_gp();
                self.li(t, *val);
                self.op_store(t, size, dst);
            }
            Mir::Operand(Operand::Reg(reg), _) => {
                self.op_store(*reg, size, dst);
            }
            _ => {
                let t = self.gp(src);
                self.op_store(t, size, dst);
            }
        }
    }

    fn mov(&mut self, src: MirId, dst: MirId, size: usize) {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(_), _) => {
                let d = self.dst_reg(dst);
                self.mov_into_reg(d, src);
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                self.mov_to_mem(src, dst, size);
            }
            _ => unreachable!(),
        }
    }

    fn movs(&mut self, src: MirId, dst: MirId) {
        let src_size = operand_size(self.arena, src);
        let dst_size = operand_size(self.arena, dst);
        let d = self.dst_reg(dst);
        match &self.arena[src] {
            Mir::Operand(Operand::Imm(val), _) => {
                self.li(d, *val);
                if src_size < 8 {
                    self.sign_extend(d, src_size, d);
                }
            }
            Mir::Operand(Operand::Reg(reg), _) => {
                if src_size < 8 {
                    self.sign_extend(d, src_size, *reg);
                } else {
                    self.mov_into_reg(d, src);
                }
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                self.op_sign_load(d, src_size, src);
            }
            _ => unreachable!(),
        }
        debug_assert!(dst_size >= src_size);
        self.store_result(d, dst);
    }

    fn movz(&mut self, src: MirId, dst: MirId) {
        let src_size = operand_size(self.arena, src);
        let d = self.dst_reg(dst);
        match &self.arena[src] {
            Mir::Operand(Operand::Imm(val), _) => {
                self.li(d, *val);
                if src_size < 8 {
                    self.zero_extend(d, src_size, d);
                }
            }
            Mir::Operand(Operand::Reg(reg), _) => {
                if src_size < 8 {
                    self.zero_extend(d, src_size, *reg);
                } else {
                    self.mov_into_reg(d, src);
                }
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                self.op_load_zero(d, src_size, src);
            }
            _ => unreachable!(),
        }
        self.store_result(d, dst);
    }

    fn sign_extend(&mut self, into: Register, src_size: usize, src: Register) {
        let shift = (8 - src_size) * 8;
        if into == src {
            self.line_fmt(format_args!("slli {}, {}, {}", into, src, shift));
            self.line_fmt(format_args!("srai {}, {}, {}", into, into, shift));
        } else {
            let t = self.alloc_gp();
            self.line_fmt(format_args!("slli {}, {}, {}", t, src, shift));
            self.line_fmt(format_args!("srai {}, {}, {}", into, t, shift));
        }
    }

    fn zero_extend(&mut self, into: Register, src_size: usize, src: Register) {
        let shift = (8 - src_size) * 8;
        if into == src {
            self.line_fmt(format_args!("slli {}, {}, {}", into, src, shift));
            self.line_fmt(format_args!("srli {}, {}, {}", into, into, shift));
        } else {
            let t = self.alloc_gp();
            self.line_fmt(format_args!("slli {}, {}, {}", t, src, shift));
            self.line_fmt(format_args!("srli {}, {}, {}", into, t, shift));
        }
    }

    fn movf(&mut self, src: MirId, dst: MirId) {
        match &self.arena[dst] {
            Mir::Operand(Operand::Reg(reg), _) => {
                let f = self.fp(src);
                if *reg != f {
                    self.line_fmt(format_args!("fmv.d {}, {}", reg, f));
                }
            }
            Mir::Operand(Operand::Mem(_, _), _)
            | Mir::Operand(Operand::Sym(_), _) => {
                let f = self.fp(src);
                self.op_fsd(f, dst);
            }
            _ => unreachable!(),
        }
    }

    fn deref_ptr(&mut self, id: MirId) -> Register {
        // Assemble the effective address of a memory operand whose base is a
        // memory-backed pseudo holding the pointer value.
        match &self.arena[id] {
            Mir::Operand(Operand::Mem(base, off), _) => {
                let a = self.alloc_gp();
                match self.s0_offset(*base) {
                    Some(boff) => {
                        self.line_fmt(format_args!("ld {}, {}(s0)", a, boff));
                    }
                    None => {
                        let p = self.data_ptr(*base);
                        self.line_fmt(format_args!("ld {}, 0({})", a, p));
                    }
                }
                if *off != 0 {
                    if fits_signed_12(*off as u64) {
                        self.line_fmt(format_args!(
                            "addi {}, {}, {}",
                            a, a, *off
                        ));
                    } else {
                        let t = self.alloc_gp();
                        self.li(t, *off as u64);
                        self.line_fmt(format_args!("add {}, {}, {}", a, a, t));
                    }
                }
                a
            }
            Mir::Operand(Operand::Sym(_), _) => self.data_ptr(id),
            _ => unreachable!(),
        }
    }

    fn load_indirect(&mut self, src: MirId, dst: MirId, size: usize) {
        let a = self.deref_ptr(src);
        let d = self.dst_reg(dst);
        self.load(d, size, a);
        self.store_result(d, dst);
    }

    fn store_indirect(&mut self, src: MirId, dst: MirId, size: usize) {
        let a = self.deref_ptr(dst);
        let t = match &self.arena[src] {
            Mir::Operand(Operand::Imm(val), _) => {
                let t = self.alloc_gp();
                self.li(t, *val);
                t
            }
            Mir::Operand(Operand::Reg(reg), _) => *reg,
            _ => self.gp(src),
        };
        self.store(t, size, a);
    }

    fn binop(
        &mut self,
        size: usize,
        mnemonic: &str,
        mnemonic_w: Option<&str>,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
    ) {
        let l = self.gp(lhs);
        let r = self.gp(rhs);
        let d = self.dst_reg(dst);
        match size {
            4 => {
                let m = mnemonic_w.unwrap_or(mnemonic);
                self.line_fmt(format_args!("{} {}, {}, {}", m, d, l, r));
            }
            _ => {
                self.line_fmt(format_args!("{} {}, {}, {}", mnemonic, d, l, r));
            }
        }
        self.store_result(d, dst);
    }

    fn emit_mul(&mut self, lhs: MirId, rhs: MirId, dst: MirId, size: usize) {
        self.binop(size, "mul", Some("mulw"), lhs, rhs, dst);
    }

    fn emit_div(
        &mut self,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        size: usize,
        signed: bool,
    ) {
        let (m, w) = if signed {
            ("div", "divw")
        } else {
            ("divu", "divuw")
        };
        self.binop(size, m, Some(w), lhs, rhs, dst);
    }

    fn emit_rem(
        &mut self,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        size: usize,
        signed: bool,
    ) {
        let (m, w) = if signed {
            ("rem", "remw")
        } else {
            ("remu", "remuw")
        };
        self.binop(size, m, Some(w), lhs, rhs, dst);
    }

    fn emit_shift(
        &mut self,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        size: usize,
        kind: ShiftKind,
    ) {
        let v = self.gp(lhs);
        let d = self.dst_reg(dst);

        let imm_m64 = match kind {
            ShiftKind::Shl => "slli",
            ShiftKind::Shr => "srli",
            ShiftKind::Sar => "srai",
        };
        let imm_m32 = match kind {
            ShiftKind::Shl => "slliw",
            ShiftKind::Shr => "srliw",
            ShiftKind::Sar => "sraiw",
        };
        let reg_m = match kind {
            ShiftKind::Shl => ("sllw", "sll"),
            ShiftKind::Shr => ("srlw", "srl"),
            ShiftKind::Sar => ("sraw", "sra"),
        };

        let value = if size < 4 {
            let t = self.alloc_gp();
            match kind {
                ShiftKind::Shr => self.zero_extend(t, size, v),
                ShiftKind::Sar => self.sign_extend(t, size, v),
                ShiftKind::Shl => self.zero_extend(t, size, v),
            }
            t
        } else {
            v
        };

        {
            let m = if size == 4 { imm_m32 } else { imm_m64 };
            let (reg_m32, reg_m64) = reg_m;
            match &self.arena[rhs] {
                Mir::Operand(Operand::Imm(amt), _) => {
                    self.line_fmt(format_args!(
                        "{} {}, {}, {}",
                        m, d, value, *amt
                    ));
                }
                _ => {
                    let a = self.gp(rhs);
                    let m = if size == 4 { reg_m32 } else { reg_m64 };
                    self.line_fmt(format_args!(
                        "{} {}, {}, {}",
                        m, d, value, a
                    ));
                }
            }
        }
        self.store_result(d, dst);
    }

    fn cmp_src(&mut self, id: MirId, signed: bool, size: usize) -> Register {
        let t = self.gp(id);
        if size < 8 {
            if signed {
                self.sign_extend(t, size, t);
            } else {
                self.zero_extend(t, size, t);
            }
        }
        t
    }

    fn emit_cmp(
        &mut self,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        size: usize,
        signed: bool,
        variant: CmpKind,
    ) {
        let l = self.cmp_src(lhs, signed, size);
        let r = self.cmp_src(rhs, signed, size);
        let d = self.dst_reg(dst);
        match variant {
            CmpKind::Eq => {
                self.line_fmt(format_args!("xor {}, {}, {}", d, l, r));
                self.line_fmt(format_args!("seqz {}, {}", d, d));
            }
            CmpKind::Ne => {
                self.line_fmt(format_args!("xor {}, {}, {}", d, l, r));
                self.line_fmt(format_args!("snez {}, {}", d, d));
            }
            CmpKind::Lt => {
                let m = if signed { "slt" } else { "sltu" };
                self.line_fmt(format_args!("{} {}, {}, {}", m, d, l, r));
            }
            CmpKind::Le => {
                let m = if signed { "slt" } else { "sltu" };
                self.line_fmt(format_args!("{} {}, {}, {}", m, d, r, l));
                self.line_fmt(format_args!("xori {}, {}, 1", d, d));
            }
            CmpKind::Gt => {
                let m = if signed { "slt" } else { "sltu" };
                self.line_fmt(format_args!("{} {}, {}, {}", m, d, r, l));
            }
            CmpKind::Ge => {
                let m = if signed { "slt" } else { "sltu" };
                self.line_fmt(format_args!("{} {}, {}, {}", m, d, l, r));
                self.line_fmt(format_args!("xori {}, {}, 1", d, d));
            }
        }
        self.store_result(d, dst);
    }

    fn emit_fcmp(
        &mut self,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        variant: FpCmpKind,
    ) {
        let l = self.fp(lhs);
        let r = self.fp(rhs);
        let d = self.dst_reg(dst);
        match variant {
            FpCmpKind::Eq => {
                self.line_fmt(format_args!("feq.d {}, {}, {}", d, l, r));
            }
            FpCmpKind::Ne => {
                self.line_fmt(format_args!("feq.d {}, {}, {}", d, l, r));
                self.line_fmt(format_args!("xori {}, {}, 1", d, d));
            }
            FpCmpKind::Lt => {
                self.line_fmt(format_args!("flt.d {}, {}, {}", d, l, r));
            }
            FpCmpKind::Le => {
                self.line_fmt(format_args!("fle.d {}, {}, {}", d, l, r));
            }
            FpCmpKind::Gt => {
                self.line_fmt(format_args!("flt.d {}, {}, {}", d, r, l));
            }
            FpCmpKind::Ge => {
                self.line_fmt(format_args!("fle.d {}, {}, {}", d, r, l));
            }
        }
        self.store_result(d, dst);
    }

    fn emit_fbin(
        op: &str,
        lhs: MirId,
        rhs: MirId,
        dst: MirId,
        e: &mut Emitter,
    ) {
        let l = e.fp(lhs);
        let r = e.fp(rhs);
        let d = e.dst_reg_fp(dst);
        e.line_fmt(format_args!("{} {}, {}, {}", op, d, l, r));
        e.store_fp_result(d, dst);
    }

    fn ret(&mut self) {
        self.line("ld s11, -104(s0)");
        self.line("ld s10, -96(s0)");
        self.line("ld s9, -88(s0)");
        self.line("ld s8, -80(s0)");
        self.line("ld s7, -72(s0)");
        self.line("ld s6, -64(s0)");
        self.line("ld s5, -56(s0)");
        self.line("ld s4, -48(s0)");
        self.line("ld s3, -40(s0)");
        self.line("ld s2, -32(s0)");
        self.line("ld s1, -24(s0)");
        self.line("ld ra, -8(s0)");
        self.line("addi sp, s0, 0");
        self.line("ld s0, -16(sp)");
        self.line("ret");
    }

    fn push(&mut self, src: MirId) {
        let v = self.gp(src);
        self.line("addi sp, sp, -8");
        self.line_fmt(format_args!("sd {}, 0(sp)", v));
    }

    fn pushf(&mut self, src: MirId) {
        let v = self.fp(src);
        self.line("addi sp, sp, -8");
        self.line_fmt(format_args!("fsd {}, 0(sp)", v));
    }

    fn addptr(&mut self, lhs: MirId, rhs: MirId, scale: usize, dst: MirId) {
        let l = self.gp(lhs);
        let mut idx = self.gp(rhs);
        if scale != 1 {
            let t = self.alloc_gp();
            if is_pow2(scale) {
                let shift = scale.trailing_zeros();
                self.line_fmt(format_args!("slli {}, {}, {}", t, idx, shift));
            } else {
                let k = self.alloc_gp();
                self.li(k, scale as u64);
                self.line_fmt(format_args!("mul {}, {}, {}", t, idx, k));
            }
            idx = t;
        }
        let d = self.dst_reg(dst);
        self.line_fmt(format_args!("add {}, {}, {}", d, l, idx));
        self.store_result(d, dst);
    }

    fn emit_op(&mut self, op: &Op) {
        match op {
            Op::Ret => {
                let pinned = Vec::new();
                self.begin(pinned);
                self.ret();
            }
            Op::Lea(src, dst) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                let p = self.data_ptr(*src);
                self.store_result(p, *dst);
            }
            Op::AddPtr(lhs, rhs, scale, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.addptr(*lhs, *rhs, *scale, *dst);
            }
            Op::Mov(src, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.mov(*src, *dst, *size);
            }
            Op::Movs(src, dst) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.movs(*src, *dst);
            }
            Op::Movz(src, dst) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.movz(*src, *dst);
            }
            Op::MovF(src, dst) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.movf(*src, *dst);
            }
            Op::MovFToGp(src, dst) => {
                let pinned = collect_pinned(self.arena, &[*src]);
                self.begin(pinned);
                let f = self.fp(*src);
                let gp = match &self.arena[*dst] {
                    Mir::Operand(Operand::Reg(reg), _) => *reg,
                    _ => unreachable!(),
                };
                self.line_fmt(format_args!("fmv.x.d {}, {}", gp, f));
            }
            Op::Load(src, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.load_indirect(*src, *dst, *size);
            }
            Op::Store(src, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                self.store_indirect(*src, *dst, *size);
            }
            Op::Cvtsi2sd {
                src,
                dst,
                signed,
                size,
            } => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                let v = self.gp(*src);
                let d = self.dst_reg_fp(*dst);
                let m = match (signed, size) {
                    (true, 8) => "fcvt.d.l",
                    (false, 8) => "fcvt.d.lu",
                    (_, _) if *signed => "fcvt.d.w",
                    _ => "fcvt.d.wu",
                };
                self.line_fmt(format_args!("{} {}, {}", m, d, v));
                self.store_fp_result(d, *dst);
            }
            Op::Cvttsd2si {
                src,
                dst,
                signed,
                size,
            } => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                let v = self.fp(*src);
                let d = self.dst_reg(*dst);
                let m = match (signed, size) {
                    (true, 8) => "fcvt.l.d",
                    (false, 8) => "fcvt.lu.d",
                    (_, _) if *signed => "fcvt.w.d",
                    _ => "fcvt.wu.d",
                };
                self.line_fmt(format_args!("{} {}, {}, rtz", m, d, v));
                self.store_result(d, *dst);
            }
            Op::Neg(src, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                let v = self.gp(*src);
                let d = self.dst_reg(*dst);
                match size {
                    4 => {
                        self.line_fmt(format_args!("negw {}, {}", d, v));
                    }
                    _ => {
                        self.line_fmt(format_args!("neg {}, {}", d, v));
                    }
                }
                self.store_result(d, *dst);
            }
            Op::Not(src, dst, _size) => {
                let pinned = collect_pinned(self.arena, &[*src, *dst]);
                self.begin(pinned);
                let v = self.gp(*src);
                let d = self.dst_reg(*dst);
                self.line_fmt(format_args!("xori {}, {}, -1", d, v));
                self.store_result(d, *dst);
            }
            Op::Imul(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_mul(*lhs, *rhs, *dst, *size);
            }
            Op::Idiv(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_div(*lhs, *rhs, *dst, *size, *signed);
            }
            Op::Irem(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_rem(*lhs, *rhs, *dst, *size, *signed);
            }
            Op::Add(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.binop(*size, "add", Some("addw"), *lhs, *rhs, *dst);
            }
            Op::Sub(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.binop(*size, "sub", Some("subw"), *lhs, *rhs, *dst);
            }
            Op::Shl(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_shift(*lhs, *rhs, *dst, *size, ShiftKind::Shl);
            }
            Op::Shr(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_shift(*lhs, *rhs, *dst, *size, ShiftKind::Shr);
            }
            Op::Sar(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_shift(*lhs, *rhs, *dst, *size, ShiftKind::Sar);
            }
            Op::And(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.binop(*size, "and", None, *lhs, *rhs, *dst);
            }
            Op::Or(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.binop(*size, "or", None, *lhs, *rhs, *dst);
            }
            Op::Xor(lhs, rhs, dst, size) => {
                let fp = matches!(
                    &self.arena[*lhs],
                    Mir::Operand(Operand::Reg(r), _) if r.is_fp()
                );
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                if fp || *size == 8 {
                    let l = self.fp(*lhs);
                    let r = self.fp(*rhs);
                    let lt = self.alloc_gp();
                    let rt = self.alloc_gp();
                    self.line_fmt(format_args!("fmv.x.d {}, {}", lt, l));
                    self.line_fmt(format_args!("fmv.x.d {}, {}", rt, r));
                    let d = self.dst_reg(*dst);
                    self.line_fmt(format_args!("xor {}, {}, {}", d, lt, rt));
                    let dst_is_gp = match &self.arena[*dst] {
                        Mir::Operand(Operand::Reg(r), _) => !r.is_fp(),
                        _ => false,
                    };
                    if dst_is_gp {
                        // xor result written into a gp register
                        self.store_result(d, *dst);
                    } else {
                        let fd = self.alloc_fp();
                        self.line_fmt(format_args!("fmv.d.x {}, {}", fd, d));
                        self.op_fsd(fd, *dst);
                    }
                } else {
                    self.binop(*size, "xor", None, *lhs, *rhs, *dst);
                }
            }
            Op::FAddD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                Self::emit_fbin("fadd.d", *lhs, *rhs, *dst, self);
            }
            Op::FSubD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                Self::emit_fbin("fsub.d", *lhs, *rhs, *dst, self);
            }
            Op::FMulD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                Self::emit_fbin("fmul.d", *lhs, *rhs, *dst, self);
            }
            Op::FDivD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                Self::emit_fbin("fdiv.d", *lhs, *rhs, *dst, self);
            }
            Op::CmpEq(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, true, CmpKind::Eq);
            }
            Op::CmpNe(lhs, rhs, dst, size) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, true, CmpKind::Ne);
            }
            Op::CmpLt(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, *signed, CmpKind::Lt);
            }
            Op::CmpLe(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, *signed, CmpKind::Le);
            }
            Op::CmpGt(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, *signed, CmpKind::Gt);
            }
            Op::CmpGe(lhs, rhs, dst, size, signed) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_cmp(*lhs, *rhs, *dst, *size, *signed, CmpKind::Ge);
            }
            Op::FeqD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Eq);
            }
            Op::FneD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Ne);
            }
            Op::FltD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Lt);
            }
            Op::FleD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Le);
            }
            Op::FgtD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Gt);
            }
            Op::FgeD(lhs, rhs, dst) => {
                let pinned = collect_pinned(self.arena, &[*lhs, *rhs, *dst]);
                self.begin(pinned);
                self.emit_fcmp(*lhs, *rhs, *dst, FpCmpKind::Ge);
            }
            Op::Jmp(label) => {
                let pinned = Vec::new();
                self.begin(pinned);
                let l = Self::label_str(self.arena, *label);
                self.line_fmt(format_args!("j {}", l));
            }
            Op::Beqz(opnd, label) => {
                let pinned = collect_pinned(self.arena, &[*opnd]);
                self.begin(pinned);
                let v = self.gp(*opnd);
                let l = Self::label_str(self.arena, *label);
                self.line_fmt(format_args!("beqz {}, {}", v, l));
            }
            Op::Bnez(opnd, label) => {
                let pinned = collect_pinned(self.arena, &[*opnd]);
                self.begin(pinned);
                let v = self.gp(*opnd);
                let l = Self::label_str(self.arena, *label);
                self.line_fmt(format_args!("bnez {}, {}", v, l));
            }
            Op::FBeqz(opnd, label) => {
                let pinned = collect_pinned(self.arena, &[*opnd]);
                self.begin(pinned);
                let v = self.fp(*opnd);
                let z = self.alloc_fp();
                let t = self.alloc_gp();
                let l = Self::label_str(self.arena, *label);
                self.line_fmt(format_args!("fmv.d.x {}, zero", z));
                self.line_fmt(format_args!("feq.d {}, {}, {}", t, v, z));
                self.line_fmt(format_args!("bnez {}, {}", t, l));
            }
            Op::FBnez(opnd, label) => {
                let pinned = collect_pinned(self.arena, &[*opnd]);
                self.begin(pinned);
                let v = self.fp(*opnd);
                let z = self.alloc_fp();
                let t = self.alloc_gp();
                let l = Self::label_str(self.arena, *label);
                self.line_fmt(format_args!("fmv.d.x {}, zero", z));
                self.line_fmt(format_args!("feq.d {}, {}, {}", t, v, z));
                self.line_fmt(format_args!("beqz {}, {}", t, l));
            }
            Op::Call(func) => {
                let pinned = collect_pinned(self.arena, &[*func]);
                self.begin(pinned);
                match &self.arena[*func] {
                    Mir::Operand(Operand::Sym(name), _) => {
                        self.line_fmt(format_args!("call {}", name));
                    }
                    _ => {
                        let v = self.gp(*func);
                        self.line_fmt(format_args!("jalr ra, 0({})", v));
                    }
                }
            }
            Op::PushBytes(n) => {
                let pinned = Vec::new();
                self.begin(pinned);
                self.line_fmt(format_args!("addi sp, sp, -{}", n));
            }
            Op::PopBytes(n) => {
                let pinned = Vec::new();
                self.begin(pinned);
                self.line_fmt(format_args!("addi sp, sp, {}", n));
            }
            Op::Push(src, _) => {
                let pinned = collect_pinned(self.arena, &[*src]);
                self.begin(pinned);
                self.push(*src);
            }
            Op::PushF(src) => {
                let pinned = collect_pinned(self.arena, &[*src]);
                self.begin(pinned);
                self.pushf(*src);
            }
            Op::Label(idx) => {
                self.out.push_str(&format!(".L{}:\n", idx));
            }
        }
    }
}

enum ShiftKind {
    Shl,
    Shr,
    Sar,
}

enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

enum FpCmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

fn fmt_init_value(out: &mut String, arena: &MirArena, init: MirId) -> usize {
    match &arena[init] {
        Mir::Object(Object::InitDouble(value)) => {
            out.push_str(&format!("\t.quad 0x{:016x}\n", value.to_bits()));
            8
        }
        Mir::Object(Object::InitInteger { size, value }) => {
            match size {
                1 => {
                    out.push_str(&format!("\t.byte {}\n", *value as u8));
                }
                2 => {
                    out.push_str(&format!("\t.half {}\n", *value as u16));
                }
                4 => {
                    out.push_str(&format!("\t.long {}\n", *value as u32));
                }
                8 => {
                    out.push_str(&format!("\t.quad {}\n", *value));
                }
                _ => unreachable!(),
            }
            *size
        }
        _ => unreachable!(),
    }
}

fn emit_data(arena: &MirArena, obj: &Object) -> String {
    let mut out = String::new();
    match obj {
        Object::Data {
            name,
            global,
            read_only,
            inits,
            alignment,
            total_size,
        } => {
            if *global {
                out.push_str(&format!("\t.globl {}\n", name));
            }

            if *read_only {
                out.push_str("\t.section .rodata\n");
                out.push_str(&format!("\t.balign {}\n", alignment));
                out.push_str(&format!("{}:\n", name));

                let mut emitted_bytes = 0usize;
                for init in inits {
                    emitted_bytes += fmt_init_value(&mut out, arena, *init);
                }

                let trailing_zeros = total_size - emitted_bytes;
                if trailing_zeros > 0 {
                    out.push_str(&format!("\t.zero {}\n", trailing_zeros));
                }
            } else if inits.iter().all(|init| is_init_zero(arena, *init)) {
                out.push_str("\t.bss\n");
                out.push_str(&format!("\t.balign {}\n", alignment));
                out.push_str(&format!("{}:\n", name));
                out.push_str(&format!("\t.zero {}\n", total_size));
            } else {
                out.push_str("\t.data\n");
                out.push_str(&format!("\t.balign {}\n", alignment));
                out.push_str(&format!("{}:\n", name));

                let last_nonzero =
                    inits.iter().rposition(|init| !is_init_zero(arena, *init));
                let emit_up_to = last_nonzero.map(|i| i + 1).unwrap_or(0);
                let mut emitted_bytes = 0usize;

                for init in &inits[..emit_up_to] {
                    emitted_bytes += fmt_init_value(&mut out, arena, *init);
                }

                let trailing_zeros = total_size - emitted_bytes;
                if trailing_zeros > 0 {
                    out.push_str(&format!("\t.zero {}\n", trailing_zeros));
                }
            }
        }
        _ => unreachable!(),
    }
    out
}

pub fn emit(filepath: &str, stage: &MirStage) {
    let mut out = String::new();
    let path = Path::new(filepath);
    let filename = path.file_stem().unwrap().to_str().unwrap();

    out.push_str(&format!("\t.file \"{}.c\"\n", filename));
    out.push_str("\t.text\n\n");

    for instr in &stage.mir.top_level {
        match &stage.mir[*instr] {
            Mir::Object(Object::Function {
                name,
                global,
                stack,
                mir,
            }) => {
                out.push_str("\t.text\n");
                if *global {
                    out.push_str(&format!("\t.globl {}\n", name));
                }
                out.push_str(&format!("\t.type {}, @function\n", name));
                out.push_str(&format!("{}:\n", name));

                if *stack <= 2047 {
                    out.push_str(&pad_inst(&format!("addi sp, sp, -{}", stack)));
                    out.push('\n');
                } else {
                    out.push_str(&pad_inst(&format!("li t0, {}", stack)));
                    out.push('\n');
                    out.push_str("                \tsub sp, sp, t0\n");
                    out.push('\n');
                }

                for (reg, off) in [
                    ("ra", stack - 8),
                    ("s0", stack - 16),
                    ("s1", stack - 24),
                    ("s2", stack - 32),
                    ("s3", stack - 40),
                    ("s4", stack - 48),
                    ("s5", stack - 56),
                    ("s6", stack - 64),
                    ("s7", stack - 72),
                    ("s8", stack - 80),
                    ("s9", stack - 88),
                    ("s10", stack - 96),
                    ("s11", stack - 104),
                ] {
                    if (off as i64) >= -2048 && off <= 2047 {
                        out.push_str(&pad_inst(&format!("sd {}, {}(sp)", reg, off)));
                        out.push('\n');
                    } else {
                        out.push_str(&pad_inst(&format!("li t0, {}", off)));
                        out.push('\n');
                        out.push_str("                \tadd t0, sp, t0\n");
                        out.push('\n');
                        out.push_str(&pad_inst(&format!("sd {}, 0(t0)", reg)));
                        out.push('\n');
                    }
                }

                if *stack <= 2047 {
                    out.push_str(&pad_inst(&format!("addi s0, sp, {}", stack)));
                    out.push('\n');
                } else {
                    out.push_str(&pad_inst(&format!("li t0, {}", stack)));
                    out.push('\n');
                    out.push_str("                \tadd s0, sp, t0\n");
                    out.push('\n');
                }

                                let mut emitter = Emitter::new(&stage.mir);
                for instr in mir {
                    match &stage.mir[*instr] {
                        Mir::Op(op) => emitter.emit_op(op),
                        _ => unreachable!(),
                    }
                }
                out.push_str(&emitter.out);
                out.push('\n');
            }
            _ => {
                let emitter = Emitter::new(&stage.mir);
                match &stage.mir[*instr] {
                    Mir::Object(Object::Data { .. }) => {
                        let obj = stage.mir[*instr].clone();
                        if let Mir::Object(obj) = obj {
                            out.push_str(&emit_data(&stage.mir, &obj));
                        }
                        out.push('\n');
                        out.push('\n');
                    }
                    Mir::Op(op) => {
                        let mut e = emitter;
                        e.emit_op(&op);
                        out.push_str(&e.out);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    out.push_str("\t.ident\t\"asjcc 0.1.0\"\n");
    out.push_str("\t.section .note.GNU-stack,\"\",@progbits\n");

    std::fs::write(filepath, out).unwrap();
}
