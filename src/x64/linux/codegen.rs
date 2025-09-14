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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::codegen::CodeGenerator as AbstractCodeGenerator;
use crate::ir::tac::{Tac, TacRef};
use crate::types::{TypeRef, is_double_type, is_signed, size_of};

macro_rules! new_node {
    ($variant:ident) => {
        Rc::new(RefCell::new(Code::$variant))
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(RefCell::new(Code::$variant( $($args),* )))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(RefCell::new(Code::$variant {
            $($field: $value),*
        }))
    };
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Code {
    Function {
        name: String,
        global: bool,
        stack: usize,
        code: CodeVec,
    },
    StaticVar {
        name: String,
        global: bool,
        init: CodeRef,
    },
    InitInteger {
        signed: bool,
        size: usize,
        value: u64,
    },
    InitDouble(f64),
    Ret,
    Mov(CodeRef, CodeRef, usize),
    Movsd(CodeRef, CodeRef),
    MovAbs(CodeRef, CodeRef, usize),
    MovSignExt(CodeRef, CodeRef, usize),
    Cvttsd2si(CodeRef, CodeRef, usize),
    Cvtsi2sd(CodeRef, CodeRef, usize),
    Not(CodeRef, usize),
    Neg(CodeRef, usize),
    IMul(CodeRef, CodeRef, usize),
    Mul(CodeRef, usize),
    IDiv(CodeRef, usize),
    Div(CodeRef, usize),
    Add(CodeRef, CodeRef, usize),
    Sub(CodeRef, CodeRef, usize),
    Shl(CodeRef, CodeRef, usize),
    Shr(CodeRef, CodeRef, usize),
    Sar(CodeRef, CodeRef, usize),
    And(CodeRef, CodeRef, usize),
    Or(CodeRef, CodeRef, usize),
    Xor(CodeRef, CodeRef, usize),
    Xorpd(CodeRef, CodeRef),
    Cmp(CodeRef, CodeRef, usize),
    Comisd(CodeRef, CodeRef),
    Push(CodeRef, usize),
    Call(CodeRef),
    Imm {
        val: u64,
        signed: bool,
        size: usize,
    },
    Var {
        off: i32,
        signed: bool,
        size: usize,
    },
    Reg {
        reg: Register,
        signed: bool,
        size: usize,
    },
    Data {
        name: String,
        signed: bool,
        size: usize,
    },
    Label(usize),
    FunctionRef(String, bool),
    PushBytes(usize),
    PopBytes(usize),
    Jmp(CodeRef),
    JmpNotZero(CodeRef),
    JmpCC {
        cond: CondCode,
        label: CodeRef,
    },
    SetCC {
        cond: CondCode,
        dst: CodeRef,
    },
    Test(CodeRef, CodeRef),
    Cdq(usize),
    RoData {
        name: String,
        bits: u64,
    },
    Addsd(CodeRef, CodeRef),
    Subsd(CodeRef, CodeRef),
    Mulsd(CodeRef, CodeRef),
    Divsd(CodeRef, CodeRef),
}

type CodeRef = Rc<RefCell<Code>>;
type CodeVec = Vec<CodeRef>;
type VarMap = HashMap<usize, CodeRef>;
type LabelMap = HashMap<usize, CodeRef>;

pub struct CodeGenerator {
    code_vec: CodeVec,
    var_map: VarMap,
    label_map: LabelMap,
}

impl CodeGenerator {
    fn label(&mut self, idx: usize) -> CodeRef {
        self.label_map
            .entry(idx)
            .or_insert_with(|| new_node!(Label(idx)))
            .clone()
    }

    fn imm64_as_imm32(imm: &CodeRef) -> CodeRef {
        if let Code::Imm {
            val,
            signed,
            size: _,
        } = *imm.borrow()
        {
            new_node!(Imm {
                val: val as i32 as u64,
                signed: signed,
                size: 4
            })
        } else {
            unreachable!();
        }
    }

    fn mov_imm<'a>(
        &mut self,
        imm: &'a CodeRef,
        dst: &'a CodeRef,
        tmp: &'a CodeRef,
    ) -> CodeRef {
        // >32 bit immediate would truncate so load into a register
        if Self::operand_size(dst.clone()) < 8 {
            // assembler might complain about truncation to 32 bit
            // register so we truncate it here before emitting it.
            let imm = Self::imm64_as_imm32(imm);
            self.direct_mov(imm, tmp.clone())
        } else {
            self.direct_mov(imm.clone(), tmp.clone())
        }
    }

    fn lhs_rhs_fixup<'a>(
        &mut self,
        mut src: &'a CodeRef,
        mut dst: &'a CodeRef,
        tmp: &'a CodeRef,
        forced_src: Option<&'a CodeRef>,
        forced_dst: Option<&'a CodeRef>,
    ) -> (CodeRef, CodeRef) {
        if let Some(new_src) = forced_src {
            let mov = self.direct_mov(src.clone(), new_src.clone());
            self.emit(mov);
            src = new_src;
        }

        if let Some(new_dst) = forced_dst {
            let mov = self.direct_mov(dst.clone(), new_dst.clone());
            self.emit(mov);
            dst = new_dst;
        }

        assert!(!is_immediate(dst));

        if is_large_immediate(src) && forced_src.is_none() {
            let mov = self.mov_imm(src, dst, tmp);
            self.emit(mov);
            src = tmp;
        } else if is_mem_addr(src) && is_mem_addr(dst) {
            assert!(forced_src.is_none());

            // src and dst cannot both be memory addresses

            let mov = self.direct_mov(src.clone(), tmp.clone());
            self.emit(mov);
            src = tmp;
        } else if is_immediate(src) && is_mem_addr(dst) {
            assert!(forced_src.is_none());

            // over-zealous for some ops but keeps things simple

            let mov = self.direct_mov(src.clone(), tmp.clone());
            self.emit(mov);
            src = tmp;
        }

        (src.clone(), dst.clone())
    }

    fn operand_size(operand: CodeRef) -> usize {
        match &*operand.borrow() {
            Code::Reg { size, .. }
            | Code::Imm { size, .. }
            | Code::Var { size, .. }
            | Code::Data { size, .. } => *size,
            _ => {
                unreachable!();
            }
        }
    }

    fn reg_for(reg: Register, src: CodeRef) -> CodeRef {
        match &*src.borrow() {
            Code::Reg { size, signed, .. }
            | Code::Imm { size, signed, .. }
            | Code::Var { size, signed, .. }
            | Code::Data { size, signed, .. } => {
                new_node!(Reg {
                    reg: reg,
                    signed: *signed,
                    size: *size
                })
            }
            _ => unreachable!(),
        }
    }

    fn direct_mov(&mut self, src: CodeRef, dst: CodeRef) -> CodeRef {
        let src_imm = matches!(*src.borrow(), Code::Imm { .. });

        match &*src.borrow() {
            Code::Reg {
                size: src_size,
                signed: src_signed,
                ..
            }
            | Code::Imm {
                size: src_size,
                signed: src_signed,
                ..
            }
            | Code::Var {
                size: src_size,
                signed: src_signed,
                ..
            }
            | Code::Data {
                size: src_size,
                signed: src_signed,
                ..
            } => match &*dst.borrow() {
                Code::Reg {
                    size: dst_size,
                    signed: _dst_signed,
                    ..
                }
                | Code::Imm {
                    size: dst_size,
                    signed: _dst_signed,
                    ..
                }
                | Code::Var {
                    size: dst_size,
                    signed: _dst_signed,
                    ..
                }
                | Code::Data {
                    size: dst_size,
                    signed: _dst_signed,
                    ..
                } => {
                    if *dst_size > *src_size {
                        if *dst_size == 8 && src_imm {
                            new_node!(MovAbs(
                                src.clone(),
                                dst.clone(),
                                *dst_size
                            ))
                        } else if *src_signed {
                            new_node!(MovSignExt(
                                src.clone(),
                                dst.clone(),
                                *dst_size
                            ))
                        } else {
                            self.mov_zero_ext(&src, &dst)
                        }
                    } else {
                        new_node!(Mov(src.clone(), dst.clone(), *dst_size))
                    }
                }
                _ => unreachable!(),
            },
            _ => {
                println!("src: {:?}\ndst: {:?}", src, dst);

                unreachable!()
            }
        }
    }

    fn mov(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        self.direct_mov(lhs.clone(), rhs.clone())
    }

    fn mov_to_xmm_regs(
        &mut self,
        src: &CodeRef,
        dst: &CodeRef,
    ) -> (CodeRef, CodeRef) {
        let xmm0 = new_node!(Reg {
            reg: Register::Xmm0,
            signed: false,
            size: 8
        });
        let xmm1 = new_node!(Reg {
            reg: Register::Xmm1,
            signed: false,
            size: 8
        });
        let mut mov = new_node!(Movsd(src.clone(), xmm0.clone()));
        self.emit(mov);
        mov = new_node!(Movsd(dst.clone(), xmm1.clone()));
        self.emit(mov);

        (xmm0, xmm1)
    }

    fn add_op(
        &mut self,
        ty: &TypeRef,
        src: &mut CodeRef,
        dst: &CodeRef,
    ) -> CodeRef {
        if is_double_type(ty) {
            let (xmm0, xmm1) = self.mov_to_xmm_regs(src, dst);
            let addsd = new_node!(Addsd(xmm0.clone(), xmm1.clone()));
            self.emit(addsd);
            new_node!(Movsd(xmm1.clone(), dst.clone()))
        } else {
            let tmp = Self::reg_for(Register::R10, dst.clone());
            let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);
            new_node!(Add(lhs, rhs, Self::operand_size(dst.clone())))
        }
    }

    fn sub_op(
        &mut self,
        ty: &TypeRef,
        src: &mut CodeRef,
        dst: &CodeRef,
    ) -> CodeRef {
        if is_double_type(ty) {
            let (xmm0, xmm1) = self.mov_to_xmm_regs(src, dst);
            let subsd = new_node!(Subsd(xmm0.clone(), xmm1.clone()));
            self.emit(subsd);
            new_node!(Movsd(xmm1.clone(), dst.clone()))
        } else {
            let tmp = Self::reg_for(Register::R10, dst.clone());

            let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

            new_node!(Sub(lhs, rhs, Self::operand_size(dst.clone())))
        }
    }

    fn shl_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());
        let forced_src = new_node!(Reg {
            reg: Register::Rcx,
            signed: false,
            size: 1
        });

        let (lhs, rhs) =
            self.lhs_rhs_fixup(src, dst, &tmp, Some(&forced_src), None);

        new_node!(Shl(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn sar_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());
        let forced_src = new_node!(Reg {
            reg: Register::Rcx,
            signed: false,
            size: 1
        });

        let (lhs, rhs) =
            self.lhs_rhs_fixup(src, dst, &tmp, Some(&forced_src), None);

        new_node!(Sar(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn shr_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());
        let forced_src = new_node!(Reg {
            reg: Register::Rcx,
            signed: false,
            size: 1
        });

        let (lhs, rhs) =
            self.lhs_rhs_fixup(src, dst, &tmp, Some(&forced_src), None);

        new_node!(Shr(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn and_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        new_node!(And(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn or_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        new_node!(Or(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn xor_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        new_node!(Xor(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn xorpd_op(
        &mut self,
        lhs: &mut CodeRef,
        rhs: &CodeRef,
        dst: &CodeRef,
    ) -> CodeRef {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(lhs, rhs);
        let xor = new_node!(Xorpd(xmm1.clone(), xmm0.clone()));
        self.emit(xor);
        new_node!(Movsd(xmm0, dst.clone()))
    }

    fn cmp(&mut self, lhs: &mut CodeRef, rhs: &CodeRef) -> CodeRef {
        let size = Self::operand_size(lhs.clone());
        let tmpl = Self::reg_for(Register::R10, rhs.clone());
        let tmpr = Self::reg_for(Register::R11, rhs.clone());
        let mut mov = self.direct_mov(lhs.clone(), tmpl.clone());
        self.emit(mov);
        mov = self.direct_mov(rhs.clone(), tmpr.clone());
        self.emit(mov);
        new_node!(Cmp(tmpl, tmpr, size))
    }

    fn comisd(&mut self, lhs: &mut CodeRef, rhs: &CodeRef) -> CodeRef {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(lhs, rhs);
        new_node!(Comisd(xmm0, xmm1))
    }

    fn imul(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let size = Self::operand_size(dst.clone());
        let tmp = Self::reg_for(Register::R10, dst.clone());
        let forced_dst = Self::reg_for(Register::R11, dst.clone());

        let (lhs, rhs) =
            self.lhs_rhs_fixup(src, dst, &tmp, None, Some(&forced_dst));

        let mul = new_node!(IMul(lhs, rhs.clone(), size));
        self.emit(mul);
        self.direct_mov(rhs.clone(), dst.clone())
    }

    fn mul(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let size = Self::operand_size(dst.clone());
        let tmp = Self::reg_for(Register::R10, dst.clone());
        let forced_dst = Self::reg_for(Register::Rax, dst.clone());

        let (lhs, rhs) =
            self.lhs_rhs_fixup(src, dst, &tmp, None, Some(&forced_dst));
        let tmp2 = Self::reg_for(Register::R11, src.clone());
        let mov = self.direct_mov(lhs.clone(), tmp2.clone());
        self.emit(mov);

        let mul = new_node!(Mul(tmp2, size));
        self.emit(mul);
        self.direct_mov(rhs.clone(), dst.clone())
    }

    fn mulsd(
        &mut self,
        lhs: &mut CodeRef,
        rhs: &CodeRef,
        dst: &CodeRef,
    ) -> CodeRef {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(rhs, lhs);
        let mulsd = new_node!(Mulsd(xmm0.clone(), xmm1.clone()));
        self.emit(mulsd);
        new_node!(Movsd(xmm1.clone(), dst.clone()))
    }

    fn divsd(
        &mut self,
        lhs: &mut CodeRef,
        rhs: &CodeRef,
        dst: &CodeRef,
    ) -> CodeRef {
        let (xmm0, xmm1) = self.mov_to_xmm_regs(rhs, lhs);
        let divsd = new_node!(Divsd(xmm0.clone(), xmm1.clone()));
        self.emit(divsd);
        new_node!(Movsd(xmm1.clone(), dst.clone()))
    }

    fn idiv(&mut self, src: CodeRef, size: usize) -> CodeRef {
        if is_immediate(&src) {
            let tmp = new_node!(Reg {
                reg: Register::R10,
                signed: true,
                size: size
            });

            let mov = self.direct_mov(src.clone(), tmp.clone());
            self.emit(mov);
            new_node!(IDiv(tmp.clone(), size))
        } else {
            new_node!(IDiv(src.clone(), size))
        }
    }

    fn div(&mut self, src: CodeRef, size: usize) -> CodeRef {
        if is_immediate(&src) {
            let tmp = new_node!(Reg {
                reg: Register::R10,
                signed: false,
                size: size
            });

            let mov = self.direct_mov(src.clone(), tmp.clone());
            self.emit(mov);

            let edx = new_node!(Reg {
                reg: Register::Rdx,
                signed: false,
                size: 4
            });

            let xor = new_node!(Xor(edx.clone(), edx.clone(), 4));
            self.emit(xor);

            new_node!(Div(tmp.clone(), size))
        } else {
            let edx = new_node!(Reg {
                reg: Register::Rdx,
                signed: false,
                size: 4
            });
            let xor = new_node!(Xor(edx.clone(), edx.clone(), 4));
            self.emit(xor);

            new_node!(Div(src.clone(), size))
        }
    }

    fn local_variable(&mut self, ty: &TypeRef, pos: usize) -> CodeRef {
        let ty_val = ty.borrow();
        self.var_map
            .entry(pos)
            .or_insert_with(|| {
                new_node!(Var {
                    off: pos as i32,
                    signed: is_signed(ty),
                    size: ty_val.size
                })
            })
            .clone()
    }

    fn invert(&mut self, _ty: &TypeRef, src: &TacRef, dst: &TacRef) -> CodeRef {
        let src = self.expr(src.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut src.clone(), &dst);
        self.emit(mov);
        new_node!(Not(dst.clone(), Self::operand_size(dst.clone())))
    }

    fn negate(&mut self, _ty: &TypeRef, src: &TacRef, dst: &TacRef) -> CodeRef {
        let src = self.expr(src.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut src.clone(), &dst);
        self.emit(mov);
        new_node!(Neg(dst.clone(), Self::operand_size(dst.clone())))
    }

    fn not(&mut self, _ty: &TypeRef, src: &TacRef, dst: &TacRef) -> CodeRef {
        let x = self.expr(src.clone());
        let imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(x.clone())
        });
        let cmp = self.cmp(&mut imm.clone(), &x);
        self.emit(cmp);
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut imm.clone(), &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::Eq,
            dst: dst.clone(),
        })
    }

    fn integer(&mut self, ty: &TypeRef, value: u64) -> CodeRef {
        let imm = new_node!(Imm {
            val: value as u64,
            signed: is_signed(ty),
            size: size_of(ty)
        });
        imm
    }

    fn double(&mut self, value: f64) -> CodeRef {
        let imm = new_node!(Imm {
            val: value.to_bits(),
            signed: true,
            size: 8
        });
        imm
    }

    fn multiply(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let mut y = self.expr(rhs.clone());
        let dst = self.expr(dst.clone());

        if is_double_type(ty) {
            self.mulsd(&mut x, &y, &dst)
        } else if is_signed(ty) {
            let mov = self.mov(&mut x, &dst);
            self.emit(mov);
            self.imul(&mut y, &dst)
        } else {
            let mov = self.mov(&mut x, &dst);
            self.emit(mov);
            self.mul(&mut y, &dst)
        }
    }

    fn divide(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let dst = self.expr(dst.clone());

        if is_double_type(ty) {
            self.divsd(&mut x, &y, &dst)
        } else {
            let reg = Self::reg_for(Register::Rax, x.clone());
            let mov = self.mov(&mut x, &reg);
            self.emit(mov);
            let signed = is_signed(ty);
            if signed {
                let cdq = new_node!(Cdq(size_of(ty)));
                self.emit(cdq);
            }

            if signed {
                let idiv = self.idiv(y, size_of(ty));
                self.emit(idiv);
            } else {
                let div = self.div(y, size_of(ty));
                self.emit(div);
            }

            self.mov(&mut Self::reg_for(Register::Rax, dst.clone()), &dst)
        }
    }

    fn modulo(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let reg = Self::reg_for(Register::Rax, x.clone());
        let mov = self.mov(&mut x, &reg);
        self.emit(mov);
        let signed = is_signed(ty);

        if signed {
            let cdq = new_node!(Cdq(size_of(ty)));
            self.emit(cdq);
        }

        let y = self.expr(rhs.clone());
        if signed {
            let idiv = self.idiv(y, size_of(ty));
            self.emit(idiv);
        } else {
            let div = self.div(y, size_of(ty));
            self.emit(div);
        }
        let dst = self.expr(dst.clone());
        self.mov(&mut Self::reg_for(Register::Rdx, dst.clone()), &dst)
    }

    fn add(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        self.add_op(ty, &mut y, &dst)
    }

    fn subtract(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        self.sub_op(ty, &mut y, &dst)
    }

    fn shift_left(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        self.shl_op(&mut y, &dst)
    }

    fn shift_right(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        if is_signed(ty) {
            self.sar_op(&mut y, &dst)
        } else {
            self.shr_op(&mut y, &dst)
        }
    }

    fn and(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        self.and_op(&mut y, &dst)
    }

    fn or(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let dst = self.expr(dst.clone());
        let mov = self.mov(&mut x, &dst);
        self.emit(mov);
        let mut y = self.expr(rhs.clone());
        self.or_op(&mut y, &dst)
    }

    fn xor(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut x = self.expr(lhs.clone());
        let mut y = self.expr(rhs.clone());
        let dst = self.expr(dst.clone());

        if is_double_type(ty) {
            self.xorpd_op(&mut x, &y, &dst)
        } else {
            let mov = self.mov(&mut x, &dst);
            self.emit(mov);
            self.xor_op(&mut y, &dst)
        }
    }

    fn equal(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        self.emit(cmp);
        let dst = self.expr(dst.clone());
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);

        self.check_cond(CondCode::Eq, np_check, &dst.clone())
    }

    fn not_eq(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        self.emit(cmp);
        let dst = self.expr(dst.clone());
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        self.check_neq_cond(&dst.clone(), np_check)
    }

    fn less(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);

        let cond = if is_signed(ty) {
            CondCode::Less
        } else {
            CondCode::Below
        };

        self.check_cond(cond, np_check, &dst.clone())
    }

    fn less_or_eq(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);

        let cond = if is_signed(ty) {
            CondCode::LessOrEq
        } else {
            CondCode::BelowOrEq
        };

        self.check_cond(cond, np_check, &dst.clone())
    }

    fn greater(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);

        let cond = if is_signed(ty) {
            CondCode::Greater
        } else {
            CondCode::Above
        };

        self.check_cond(cond, np_check, &dst.clone())
    }

    fn greater_or_eq(
        &mut self,
        ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let mut np_check = false;
        let cmp = if Self::is_double(lhs) {
            np_check = true;
            self.comisd(&mut y.clone(), &x)
        } else {
            self.cmp(&mut y.clone(), &x)
        };
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);

        let cond = if is_signed(ty) {
            CondCode::GreaterOrEq
        } else {
            CondCode::AboveOrEq
        };

        self.check_cond(cond, np_check, &dst.clone())
    }

    fn copy(&mut self, _ty: &TypeRef, src: &TacRef, dst: &TacRef) -> CodeRef {
        let mut src = self.expr(src.clone());
        let dst = self.expr(dst.clone());
        self.mov(&mut src, &dst)
    }

    fn truncate(
        &mut self,
        ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        self.copy(ty, src, dst)
    }

    fn mov_zero_ext(&mut self, from: &CodeRef, to: &CodeRef) -> CodeRef {
        let reg4 = new_node!(Reg {
            reg: Register::R11,
            signed: false,
            size: 4
        });

        let mov = self.direct_mov(from.clone(), reg4);
        self.emit(mov);

        let mut reg8 = new_node!(Reg {
            reg: Register::R11,
            signed: false,
            size: 8
        });

        self.mov(&mut reg8, &to)
    }

    fn int_to_double(
        &mut self,
        ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let mut src = self.expr(src.clone());
        let dst = self.expr(dst.clone());

        if !is_register(&src) {
            let tmp = new_node!(Reg {
                reg: Register::R10,
                signed: true,
                size: size_of(ty)
            });
            let mov = self.mov(&mut src, &tmp);
            self.emit(mov);
            src = tmp;
        }

        let xmm = new_node!(Reg {
            reg: Register::Xmm0,
            signed: true,
            size: size_of(ty),
        });

        let cvt = new_node!(Cvtsi2sd(src.clone(), xmm.clone(), 8));
        self.emit(cvt);
        let mov = new_node!(Movsd(xmm, dst.clone()));

        mov
    }

    fn double_to_int(
        &mut self,
        ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let src = self.expr(src.clone());
        let dst = self.expr(dst.clone());

        if !is_register(&dst) {
            let tmp = new_node!(Reg {
                reg: Register::R10,
                signed: true,
                size: size_of(ty)
            });

            let cvt =
                new_node!(Cvttsd2si(src.clone(), tmp.clone(), size_of(ty)));
            self.emit(cvt);
            self.direct_mov(tmp, dst)
        } else {
            new_node!(Cvttsd2si(src.clone(), dst.clone(), size_of(ty)))
        }
    }

    fn double_to_ulong(
        &mut self,
        ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let src = self.expr(src.clone());
        let dst = self.expr(dst.clone());

        let tmp = new_node!(Reg {
            reg: Register::R10,
            signed: false,
            size: 8
        });
        let out = new_node!(Reg {
            reg: Register::R10,
            signed: false,
            size: size_of(ty)
        });

        let cvt = new_node!(Cvttsd2si(src.clone(), tmp.clone(), 8));
        self.emit(cvt);
        let mov = self.direct_mov(out, dst.clone());
        mov
    }

    fn jump(&mut self, label: &TacRef) -> CodeRef {
        let label = self.expr(label.clone());
        new_node!(Jmp(label))
    }

    fn check_neq_cond(&mut self, dst: &CodeRef, np: bool) -> CodeRef {
        if np {
            self.emit(new_node!(SetCC {
                cond: CondCode::NotEq,
                dst: dst.clone(),
            }));
            let tmp_unord = new_node!(Reg {
                reg: Register::R10,
                signed: false,
                size: 1
            });
            self.emit(new_node!(SetCC {
                cond: CondCode::Parity,
                dst: tmp_unord.clone(),
            }));
            new_node!(Or(tmp_unord, dst.clone(), 1))
        } else {
            new_node!(SetCC {
                cond: CondCode::NotEq,
                dst: dst.clone(),
            })
        }
    }

    fn check_cond(
        &mut self,
        cond: CondCode,
        np: bool,
        dst: &CodeRef,
    ) -> CodeRef {
        let setcc = new_node!(SetCC {
            cond: cond.clone(),
            dst: dst.clone(),
        });

        if np {
            self.emit(setcc);
            let tmp = new_node!(Reg {
                reg: Register::R11,
                signed: false,
                size: 1
            });
            self.emit(new_node!(SetCC {
                cond: CondCode::NoParity,
                dst: tmp.clone(),
            }));
            new_node!(And(tmp.clone(), dst.clone(), 1))
        } else {
            setcc
        }
    }

    fn jump_on_zero(
        &mut self,
        _ty: &TypeRef,
        expr: &TacRef,
        label: &TacRef,
    ) -> CodeRef {
        let mut src = self.expr(expr.clone());
        let label = self.expr(label.clone());
        let mut np_check = false;

        if Self::is_double(expr) {
            let xmm1 = new_node!(Reg {
                reg: Register::Xmm1,
                signed: false,
                size: 8
            });
            let xor = new_node!(Xorpd(xmm1.clone(), xmm1.clone()));
            self.emit(xor);
            let cmp = self.comisd(&mut src, &xmm1);
            self.emit(cmp);
            np_check = true;
        } else {
            let imm = new_node!(Imm {
                val: 0,
                signed: true,
                size: Self::operand_size(src.clone())
            });
            let cmp = self.cmp(&mut imm.clone(), &src);
            self.emit(cmp);
        }

        let res = new_node!(Reg {
            reg: Register::R10,
            signed: false,
            size: 1
        });

        let check = self.check_cond(CondCode::Eq, np_check, &res);
        self.emit(check);
        self.emit(new_node!(Test(res.clone(), res.clone())));

        new_node!(JmpNotZero(label))
    }

    fn jump_on_not_zero(
        &mut self,
        _ty: &TypeRef,
        expr: &TacRef,
        label: &TacRef,
    ) -> CodeRef {
        let mut src = self.expr(expr.clone());
        let mut np_check = false;
        let label = self.expr(label.clone());

        if Self::is_double(expr) {
            let xmm1 = new_node!(Reg {
                reg: Register::Xmm1,
                signed: false,
                size: 8
            });
            let xor = new_node!(Xorpd(xmm1.clone(), xmm1.clone()));
            self.emit(xor);
            let cmp = self.comisd(&mut src, &xmm1);
            self.emit(cmp);
            self.emit(new_node!(JmpCC {
                cond: CondCode::Parity,
                label: label.clone()
            }));
            np_check = true;
        } else {
            let imm = new_node!(Imm {
                val: 0,
                signed: true,
                size: Self::operand_size(src.clone())
            });
            let cmp = self.cmp(&mut imm.clone(), &src);
            self.emit(cmp);
        }

        let res = new_node!(Reg {
            reg: Register::R10,
            signed: false,
            size: 1
        });

        let check = self.check_cond(CondCode::NotEq, np_check, &res);
        self.emit(check);
        self.emit(new_node!(Test(res.clone(), res.clone())));

        new_node!(JmpNotZero(label))
    }

    fn is_double(tac: &TacRef) -> bool {
        match tac.as_ref() {
            Tac::Double(_) => true,
            Tac::IntToDouble { .. } => true,
            Tac::Add { ty, .. }
            | Tac::Sub { ty, .. }
            | Tac::Mul { ty, .. }
            | Tac::Div { ty, .. }
            | Tac::Mod { ty, .. }
            | Tac::Neg { ty, .. }
            | Tac::Copy { ty, .. }
            | Tac::Truncate { ty, .. }
            | Tac::SignExt { ty, .. }
            | Tac::ZeroExt { ty, .. }
            | Tac::Equal { ty, .. }
            | Tac::NotEq { ty, .. }
            | Tac::Less { ty, .. }
            | Tac::LessOrEq { ty, .. }
            | Tac::Greater { ty, .. }
            | Tac::GreaterOrEq { ty, .. }
            | Tac::And { ty, .. }
            | Tac::Or { ty, .. }
            | Tac::Xor { ty, .. }
            | Tac::Not { ty, .. } => is_double_type(ty),
            Tac::Var(ty, _) => is_double_type(ty),
            Tac::StaticVar(ty, _, _, _) => is_double_type(ty),
            Tac::StaticVarRef(ty, _) => is_double_type(ty),
            _ => false,
        }
    }

    fn classify_args(
        args: &[TacRef],
    ) -> (Vec<TacRef>, Vec<TacRef>, Vec<TacRef>) {
        let mut gp_reg_args: Vec<TacRef> = Vec::new();
        let mut fp_reg_args: Vec<TacRef> = Vec::new();
        let mut stack_args: Vec<TacRef> = Vec::new();

        for arg in args {
            if Self::is_double(arg) {
                if fp_reg_args.len() < 8 {
                    fp_reg_args.push(arg.clone());
                } else {
                    stack_args.push(arg.clone());
                }
            } else {
                if gp_reg_args.len() < 6 {
                    gp_reg_args.push(arg.clone());
                } else {
                    stack_args.push(arg.clone());
                }
            }
        }

        (gp_reg_args, fp_reg_args, stack_args)
    }

    fn call(
        &mut self,
        ty: &TypeRef,
        func: &TacRef,
        args: &[TacRef],
        dst: &TacRef,
    ) -> CodeRef {
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

        let (gp_reg_args, fp_reg_args, stack_args) = Self::classify_args(args);

        let stack_padding = if (stack_args.len() & 1) != 0 { 8 } else { 0 };

        if stack_padding != 0 {
            self.emit(new_node!(PushBytes(stack_padding)));
        }

        for (i, arg) in gp_reg_args.iter().enumerate() {
            let reg: &Register = &arg_regs[i];
            let src = self.expr(arg.clone());
            let dst = Self::reg_for(reg.clone(), src.clone());
            self.emit(new_node!(Mov(
                src.clone(),
                dst.clone(),
                Self::operand_size(dst.clone())
            )));
        }

        for (i, arg) in fp_reg_args.iter().enumerate() {
            let reg: &Register = &fp_arg_regs[i];
            let src = self.expr(arg.clone());
            let dst = Self::reg_for(reg.clone(), src.clone());
            self.emit(new_node!(Movsd(src.clone(), dst.clone(),)));
        }

        for arg in stack_args.iter().rev() {
            let src = self.expr(arg.clone());
            if is_register(&src) || is_immediate(&src) {
                let p = self.push(&src);
                self.emit(p);
            } else {
                let dst = Self::reg_for(Register::Rax, src.clone());
                self.emit(new_node!(Mov(
                    src.clone(),
                    dst.clone(),
                    Self::operand_size(dst.clone())
                )));
                self.emit(new_node!(Push(
                    new_node!(Reg {
                        reg: Register::Rax,
                        signed: false,
                        size: 8
                    }),
                    8
                )));
            }
        }

        let callee = self.expr(func.clone());
        let call = new_node!(Call(callee));

        self.emit(call);

        let pop_bytes = 8 * stack_args.len() + stack_padding;

        if pop_bytes > 0 {
            self.emit(new_node!(PopBytes(pop_bytes)));
        }

        let res = self.expr(dst.clone());

        if is_double_type(ty) {
            let mov = new_node!(Movsd(
                Self::reg_for(Register::Xmm0, res.clone()),
                res.clone()
            ));
            mov
        } else {
            let mov = new_node!(Mov(
                Self::reg_for(Register::Rax, res.clone()),
                res.clone(),
                Self::operand_size(res.clone())
            ));
            mov
        }
    }

    fn push(&mut self, src: &CodeRef) -> CodeRef {
        if is_large_immediate(src) {
            let tmp = new_node!(Reg {
                reg: Register::R11,
                signed: false,
                size: 8
            });
            let mov = self.direct_mov(src.clone(), tmp.clone());
            self.emit(mov);
            new_node!(Push(tmp.clone(), 8))
        } else {
            new_node!(Push(src.clone(), 8))
        }
    }

    #[allow(unused_variables)]
    fn expr(&mut self, node: TacRef) -> CodeRef {
        match &*node {
            Tac::Integer { ty, value } => self.integer(ty, *value),
            Tac::Double(value) => self.double(*value),
            Tac::Var(ty, off) => self.local_variable(ty, *off),
            Tac::Inv { ty, src, dst } => self.invert(ty, src, dst),
            Tac::Neg { ty, src, dst } => self.negate(ty, src, dst),
            Tac::Not { ty, src, dst } => self.not(ty, src, dst),
            Tac::Mul { ty, lhs, rhs, dst } => self.multiply(ty, lhs, rhs, dst),
            Tac::Div { ty, lhs, rhs, dst } => self.divide(ty, lhs, rhs, dst),
            Tac::Mod { ty, lhs, rhs, dst } => self.modulo(ty, lhs, rhs, dst),
            Tac::Add { ty, lhs, rhs, dst } => self.add(ty, lhs, rhs, dst),
            Tac::Sub { ty, lhs, rhs, dst } => self.subtract(ty, lhs, rhs, dst),
            Tac::LeftShift { ty, lhs, rhs, dst } => {
                self.shift_left(ty, lhs, rhs, dst)
            }
            Tac::RightShift { ty, lhs, rhs, dst } => {
                self.shift_right(ty, lhs, rhs, dst)
            }
            Tac::And { ty, lhs, rhs, dst } => self.and(ty, lhs, rhs, dst),
            Tac::Or { ty, lhs, rhs, dst } => self.or(ty, lhs, rhs, dst),
            Tac::Xor { ty, lhs, rhs, dst } => self.xor(ty, lhs, rhs, dst),
            Tac::Less { ty, lhs, rhs, dst } => self.less(ty, lhs, rhs, dst),
            Tac::LessOrEq { ty, lhs, rhs, dst } => {
                self.less_or_eq(ty, lhs, rhs, dst)
            }
            Tac::Greater { ty, lhs, rhs, dst } => {
                self.greater(ty, lhs, rhs, dst)
            }
            Tac::GreaterOrEq { ty, lhs, rhs, dst } => {
                self.greater_or_eq(ty, lhs, rhs, dst)
            }
            Tac::Equal { ty, lhs, rhs, dst } => self.equal(ty, lhs, rhs, dst),
            Tac::NotEq { ty, lhs, rhs, dst } => self.not_eq(ty, lhs, rhs, dst),
            Tac::Copy { ty, src, dst } => self.copy(ty, src, dst),
            Tac::Truncate { ty, src, dst } => self.truncate(ty, src, dst),
            Tac::SignExt { ty, src, dst } => self.copy(ty, src, dst),
            Tac::ZeroExt { ty, src, dst } => self.copy(ty, src, dst),
            Tac::DoubleToInt { ty, src, dst } => {
                self.double_to_int(ty, src, dst)
            }
            Tac::DoubleToUlong { ty, src, dst } => {
                self.double_to_ulong(ty, src, dst)
            }
            Tac::IntToDouble { ty, src, dst } => {
                self.int_to_double(ty, src, dst)
            }
            Tac::Jump(label) => self.jump(label),
            Tac::JumpOnZero { ty, expr, label } => {
                self.jump_on_zero(ty, expr, label)
            }
            Tac::JumpOnNotZero { ty, expr, label } => {
                self.jump_on_not_zero(ty, expr, label)
            }
            Tac::Label(idx) => self.label(*idx),
            Tac::FunctionRef(name, defined) => {
                new_node!(FunctionRef(name.clone(), *defined))
            }
            Tac::StaticVarRef(ty, name) => {
                new_node!(Data {
                    name: name.clone(),
                    signed: is_signed(&ty),
                    size: size_of(&ty)
                })
            }
            Tac::Call {
                ty,
                func,
                args,
                dst,
            } => self.call(ty, func, args, dst),
            Tac::RoData(ty, name, bits) => {
                new_node!(RoData {
                    name: name.clone(),
                    bits: *bits,
                })
            }
            _ => {
                println!("Did not expect: {:#?}", node);
                unreachable!()
            }
        }
    }

    fn function(
        &mut self,
        name: &String,
        global: bool,
        params: &[TacRef],
        body: &[TacRef],
        depth: usize,
    ) {
        let mut codegen = CodeGenerator::new();

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
            Self::classify_args(params);

        for (i, param) in gp_reg_params.iter().enumerate() {
            let reg = &arg_regs[i];
            let var = self.expr(param.clone());
            codegen.emit(new_node!(Mov(
                Self::reg_for(reg.clone(), var.clone()),
                var.clone(),
                Self::operand_size(var.clone())
            )));
        }

        for (i, param) in fp_reg_params.iter().enumerate() {
            let reg = &fp_arg_regs[i];
            let var = self.expr(param.clone());
            codegen.emit(new_node!(Movsd(
                Self::reg_for(reg.clone(), var.clone()),
                var.clone(),
            )));
        }

        let mut off = 16;

        for param in stack_params.iter() {
            let par = codegen.expr(param.clone());
            let mut from = new_node!(Var {
                off: -(off),
                signed: false,
                size: Self::operand_size(par.clone())
            });

            let to = Self::reg_for(Register::R10, from.clone());
            let mut mov = codegen.mov(&mut from, &to);
            codegen.emit(mov);
            from = to.clone();
            mov = codegen.mov(&mut from, &par);
            codegen.emit(mov);

            off += 8;
        }

        for op in body {
            codegen.stmt_or_decl(op.clone());
        }

        self.emit(new_node!(Function {
            name: name.clone(),
            global: global,
            stack: (depth + 15) & !15,
            code: codegen.out(),
        }));
    }

    fn static_variable(&mut self, name: &String, global: bool, init: &TacRef) {
        if let Tac::StaticInitializer(_ty, expr) = &*init.as_ref() {
            match expr.as_ref() {
                Tac::Integer { ty, value } => {
                    let initializer = new_node!(InitInteger {
                        signed: is_signed(ty),
                        size: size_of(ty),
                        value: *value
                    });
                    self.emit(new_node!(StaticVar {
                        name: name.clone(),
                        global: global,
                        init: initializer
                    }));
                }
                Tac::Double(value) => {
                    let initializer = new_node!(InitDouble(*value));
                    self.emit(new_node!(StaticVar {
                        name: name.clone(),
                        global: global,
                        init: initializer
                    }));
                }
                _ => {}
            }
        }
    }

    fn return_stmt(&mut self, ty: &TypeRef, expr: &TacRef) {
        let src = self.expr(expr.clone());
        let dst: CodeRef;

        if is_double_type(ty) {
            dst = Self::reg_for(Register::Xmm0, src.clone());

            if src != dst {
                self.code_vec
                    .push(new_node!(Movsd(src.clone(), dst.clone())));
            }
        } else {
            dst = Self::reg_for(Register::Rax, src.clone());

            if src != dst {
                self.code_vec.push(new_node!(Mov(
                    src.clone(),
                    dst.clone(),
                    Self::operand_size(dst.clone())
                )));
            }
        }

        self.emit(new_node!(Ret));
    }

    fn stmt_or_decl(&mut self, node: TacRef) {
        match &*node {
            Tac::Function {
                name,
                global,
                params,
                code,
                depth,
            } => {
                self.function(name, *global, params, code, *depth);
            }
            Tac::StaticVar(_ty, name, global, init) => {
                self.static_variable(name, *global, init);
            }
            Tac::Inv { .. }
            | Tac::Neg { .. }
            | Tac::Not { .. }
            | Tac::Mul { .. }
            | Tac::Div { .. }
            | Tac::Mod { .. }
            | Tac::Add { .. }
            | Tac::Sub { .. }
            | Tac::LeftShift { .. }
            | Tac::RightShift { .. }
            | Tac::And { .. }
            | Tac::Or { .. }
            | Tac::Xor { .. }
            | Tac::Less { .. }
            | Tac::LessOrEq { .. }
            | Tac::Greater { .. }
            | Tac::GreaterOrEq { .. }
            | Tac::Equal { .. }
            | Tac::NotEq { .. }
            | Tac::Copy { .. }
            | Tac::Jump(_)
            | Tac::JumpOnZero { .. }
            | Tac::JumpOnNotZero { .. }
            | Tac::Call { .. }
            | Tac::Truncate { .. }
            | Tac::SignExt { .. }
            | Tac::ZeroExt { .. }
            | Tac::DoubleToInt { .. }
            | Tac::DoubleToUlong { .. }
            | Tac::IntToDouble { .. }
            | Tac::Label(_) => {
                let expr = self.expr(node.clone());
                self.emit(expr);
            }
            Tac::Return(ty, expr) => {
                self.return_stmt(ty, expr);
            }
            Tac::RoData(_, _, _) => {
                let expr = self.expr(node.clone());
                self.emit(expr);
            }
            _ => {
                println!("Did not expect: {:#?}", node);
                unreachable!()
            }
        }
    }

    fn emit(&mut self, code: CodeRef) {
        self.code_vec.push(code.clone());
    }

    fn out(&mut self) -> Vec<CodeRef> {
        std::mem::take(&mut self.code_vec)
    }
}

impl AbstractCodeGenerator for CodeGenerator {
    type Code = CodeVec;

    fn new() -> Self {
        Self {
            code_vec: vec![],
            var_map: VarMap::new(),
            label_map: LabelMap::new(),
        }
    }

    fn lower(&mut self, ir: Vec<TacRef>) -> Self::Code {
        for tac in ir {
            self.stmt_or_decl(tac.clone());
        }
        self.code_vec.clone()
    }
}

fn is_immediate(code: &CodeRef) -> bool {
    matches!(*code.borrow(), Code::Imm { .. })
}

fn is_large_immediate(code: &CodeRef) -> bool {
    match &*code.borrow() {
        Code::Imm { val, size, .. } => {
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

fn is_mem_addr(code: &CodeRef) -> bool {
    match *code.borrow() {
        Code::Var { .. } | Code::Data { .. } => true,
        _ => false,
    }
}

fn is_register(code: &CodeRef) -> bool {
    match *code.borrow() {
        Code::Reg { .. } => true,
        _ => false,
    }
}
