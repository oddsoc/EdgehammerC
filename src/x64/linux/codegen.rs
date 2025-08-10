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
use crate::types::{TypeRef, is_signed};

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
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
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
    InitInt(i32),
    InitLong(i64),
    Ret,
    Mov(CodeRef, CodeRef, usize),
    MovAbs(CodeRef, CodeRef, usize),
    MovSignExt(CodeRef, CodeRef, usize),
    MovZeroExt(CodeRef, CodeRef, usize),
    Not(CodeRef, usize),
    Neg(CodeRef, usize),
    IMul(CodeRef, CodeRef, usize),
    IDiv(CodeRef, usize),
    Add(CodeRef, CodeRef, usize),
    Sub(CodeRef, CodeRef, usize),
    Shl(CodeRef, CodeRef, usize),
    Sar(CodeRef, CodeRef, usize),
    And(CodeRef, CodeRef, usize),
    Or(CodeRef, CodeRef, usize),
    Xor(CodeRef, CodeRef, usize),
    Cmp(CodeRef, CodeRef, usize),
    Push(CodeRef),
    Call(CodeRef),
    Imm {
        val: i64,
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
    JmpCC {
        cond: CondCode,
        label: CodeRef,
    },
    SetCC {
        cond: CondCode,
        dst: CodeRef,
    },
    Cdq(usize),
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
    fn new_label(&mut self, idx: usize) -> CodeRef {
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
                val: val as i32 as i64,
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

    fn direct_mov(&self, src: CodeRef, dst: CodeRef) -> CodeRef {
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
                            new_node!(MovZeroExt(
                                src.clone(),
                                dst.clone(),
                                *dst_size
                            ))
                        }
                    } else {
                        new_node!(Mov(src.clone(), dst.clone(), *dst_size))
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn mov(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        self.direct_mov(lhs.clone(), rhs.clone())
    }

    fn add_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        new_node!(Add(lhs, rhs, Self::operand_size(dst.clone())))
    }

    fn sub_op(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        let tmp = Self::reg_for(Register::R10, dst.clone());

        let (lhs, rhs) = self.lhs_rhs_fixup(src, dst, &tmp, None, None);

        new_node!(Sub(lhs, rhs, Self::operand_size(dst.clone())))
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

    fn cmp(&mut self, lhs: &mut CodeRef, rhs: &CodeRef) -> CodeRef {
        let size = Self::operand_size(lhs.clone());
        let tmpl = Self::reg_for(Register::R10, rhs.clone());
        let tmpr = Self::reg_for(Register::R11, rhs.clone());
        self.emit(self.direct_mov(lhs.clone(), tmpl.clone()));
        self.emit(self.direct_mov(rhs.clone(), tmpr.clone()));
        new_node!(Cmp(tmpl, tmpr, size))
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

    fn const_int(&mut self, val: i32) -> CodeRef {
        let imm = new_node!(Imm {
            val: val as i64,
            signed: true,
            size: 4
        });
        imm
    }

    fn const_long(&mut self, val: i64) -> CodeRef {
        let imm = new_node!(Imm {
            val: val as i64,
            signed: true,
            size: 8
        });
        imm
    }

    fn multiply(
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
        self.imul(&mut y, &dst)
    }

    fn divide(
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
        let cdq = new_node!(Cdq(ty.borrow().size));
        self.emit(cdq);
        let y = self.expr(rhs.clone());
        let idiv = self.idiv(y, ty.borrow().size);
        self.emit(idiv);
        let dst = self.expr(dst.clone());
        self.mov(&mut Self::reg_for(Register::Rax, dst.clone()), &dst)
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
        let cdq = new_node!(Cdq(ty.borrow().size));
        self.emit(cdq);
        let y = self.expr(rhs.clone());
        let idiv = self.idiv(y, ty.borrow().size);
        self.emit(idiv);
        let dst = self.expr(dst.clone());
        self.mov(&mut Self::reg_for(Register::Rdx, dst.clone()), &dst)
    }

    fn add(
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
        self.add_op(&mut y, &dst)
    }

    fn subtract(
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
        self.sub_op(&mut y, &dst)
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
        self.sar_op(&mut y, &dst)
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
        self.xor_op(&mut y, &dst)
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
        let cmp = self.cmp(&mut y.clone(), &x);
        self.emit(cmp);
        let dst = self.expr(dst.clone());
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::Eq,
            dst: dst.clone(),
        })
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
        let cmp = self.cmp(&mut y.clone(), &x);
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::NotEq,
            dst: dst.clone(),
        })
    }

    fn less(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let cmp = self.cmp(&mut y.clone(), &x);
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::Less,
            dst: dst.clone(),
        })
    }

    fn less_or_eq(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let cmp = self.cmp(&mut y.clone(), &x);
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::LessOrEq,
            dst: dst.clone(),
        })
    }

    fn greater(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let cmp = self.cmp(&mut y.clone(), &x);
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::Greater,
            dst: dst.clone(),
        })
    }

    fn greater_or_eq(
        &mut self,
        _ty: &TypeRef,
        lhs: &TacRef,
        rhs: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        let x = self.expr(lhs.clone());
        let y = self.expr(rhs.clone());
        let cmp = self.cmp(&mut y.clone(), &x);
        let dst = self.expr(dst.clone());
        self.emit(cmp);
        let mut imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(dst.clone())
        });
        let mov = self.mov(&mut imm, &dst);
        self.emit(mov);
        new_node!(SetCC {
            cond: CondCode::GreaterOrEq,
            dst: dst.clone(),
        })
    }

    fn copy(&mut self, _ty: &TypeRef, src: &TacRef, dst: &TacRef) -> CodeRef {
        let mut src = self.expr(src.clone());
        let dst = self.expr(dst.clone());
        self.mov(&mut src, &dst)
    }

    fn truncate(
        &mut self,
        _ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        self.copy(_ty, src, dst)
    }

    fn sign_ext(
        &mut self,
        _ty: &TypeRef,
        src: &TacRef,
        dst: &TacRef,
    ) -> CodeRef {
        self.copy(_ty, src, dst)
    }

    fn jump(&mut self, label: &TacRef) -> CodeRef {
        let label = self.expr(label.clone());
        new_node!(Jmp(label))
    }

    fn jump_on_zero(
        &mut self,
        _ty: &TypeRef,
        expr: &TacRef,
        label: &TacRef,
    ) -> CodeRef {
        let src = self.expr(expr.clone());
        let imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(src.clone())
        });
        let cmp = self.cmp(&mut imm.clone(), &src);
        self.emit(cmp);
        let label = self.expr(label.clone());
        new_node!(JmpCC {
            cond: CondCode::Eq,
            label: label,
        })
    }

    fn jump_on_not_zero(
        &mut self,
        _ty: &TypeRef,
        expr: &TacRef,
        label: &TacRef,
    ) -> CodeRef {
        let src = self.expr(expr.clone());
        let imm = new_node!(Imm {
            val: 0,
            signed: true,
            size: Self::operand_size(src.clone())
        });
        let cmp = self.cmp(&mut imm.clone(), &src);
        self.emit(cmp);
        let label = self.expr(label.clone());
        new_node!(JmpCC {
            cond: CondCode::NotEq,
            label: label,
        })
    }

    fn call(
        &mut self,
        _ty: &TypeRef,
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

        let reg_args: Vec<TacRef> = args.iter().take(6).cloned().collect();

        let stack_args: Vec<TacRef> = args.iter().skip(6).cloned().collect();

        let stack_padding = if (stack_args.len() & 1) != 0 { 8 } else { 0 };

        if stack_padding != 0 {
            self.emit(new_node!(PushBytes(stack_padding)));
        }

        for (i, arg) in reg_args.iter().enumerate() {
            let reg: &Register = &arg_regs[i];
            let src = self.expr(arg.clone());
            let dst = Self::reg_for(reg.clone(), src.clone());
            self.emit(new_node!(Mov(
                src.clone(),
                dst.clone(),
                Self::operand_size(dst.clone())
            )));
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
                self.emit(new_node!(Push(new_node!(Reg {
                    reg: Register::Rax,
                    signed: false,
                    size: 8
                }))));
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

        let mov = new_node!(Mov(
            Self::reg_for(Register::Rax, res.clone()),
            res.clone(),
            Self::operand_size(res.clone())
        ));
        mov
    }

    fn push(&mut self, src: &CodeRef) -> CodeRef {
        if is_large_immediate(src) {
            let tmp = Self::reg_for(Register::R11, src.clone());
            self.emit(self.direct_mov(src.clone(), tmp.clone()));
            new_node!(Push(tmp))
        } else {
            new_node!(Push(src.clone()))
        }
    }

    #[allow(unused_variables)]
    fn expr(&mut self, node: TacRef) -> CodeRef {
        match &*node {
            Tac::ConstInt(val) => self.const_int(*val),
            Tac::ConstLong(val) => self.const_long(*val),
            Tac::Var(ty, off) => self.local_variable(ty, *off),
            Tac::Inv { ty, src, dst } => self.invert(ty, src, dst),
            Tac::Neg { ty, src, dst } => self.negate(ty, src, dst),
            Tac::Not { ty, src, dst } => self.not(ty, src, dst),
            Tac::Mul { ty, lhs, rhs, dst } => self.multiply(ty, lhs, rhs, dst),
            Tac::Div { ty, lhs, rhs, dst } => self.divide(ty, lhs, rhs, dst),
            Tac::Mod { ty, lhs, rhs, dst } => self.modulo(ty, lhs, rhs, dst),
            Tac::Add { ty, lhs, rhs, dst } => self.add(ty, lhs, rhs, dst),
            Tac::Sub { ty, lhs, rhs, dst } => self.subtract(ty, lhs, rhs, dst),
            Tac::LShift { ty, lhs, rhs, dst } => {
                self.shift_left(ty, lhs, rhs, dst)
            }
            Tac::RShift { ty, lhs, rhs, dst } => {
                self.shift_right(ty, lhs, rhs, dst)
            }
            Tac::And { ty, lhs, rhs, dst } => self.and(ty, lhs, rhs, dst),
            Tac::Or { ty, lhs, rhs, dst } => self.or(ty, lhs, rhs, dst),
            Tac::Xor { ty, lhs, rhs, dst } => self.xor(ty, lhs, rhs, dst),
            Tac::LessThan { ty, lhs, rhs, dst } => self.less(ty, lhs, rhs, dst),
            Tac::LessOrEq { ty, lhs, rhs, dst } => {
                self.less_or_eq(ty, lhs, rhs, dst)
            }
            Tac::GreaterThan { ty, lhs, rhs, dst } => {
                self.greater(ty, lhs, rhs, dst)
            }
            Tac::GreaterOrEq { ty, lhs, rhs, dst } => {
                self.greater_or_eq(ty, lhs, rhs, dst)
            }
            Tac::Equal { ty, lhs, rhs, dst } => self.equal(ty, lhs, rhs, dst),
            Tac::NotEq { ty, lhs, rhs, dst } => self.not_eq(ty, lhs, rhs, dst),
            Tac::Copy { ty, src, dst } => self.copy(ty, src, dst),
            Tac::Truncate { ty, src, dst } => self.truncate(ty, src, dst),
            Tac::SignExt { ty, src, dst } => self.sign_ext(ty, src, dst),
            Tac::Jump(label) => self.jump(label),
            Tac::JumpOnZero { ty, expr, label } => {
                self.jump_on_zero(ty, expr, label)
            }
            Tac::JumpOnNotZero { ty, expr, label } => {
                self.jump_on_not_zero(ty, expr, label)
            }
            Tac::Label(idx) => self.new_label(*idx),
            Tac::FunctionRef(name, defined) => {
                new_node!(FunctionRef(name.clone(), *defined))
            }
            Tac::StaticVarRef(ty, name) => {
                new_node!(Data {
                    name: name.clone(),
                    signed: is_signed(&ty),
                    size: ty.borrow().size
                })
            }
            Tac::Call {
                ty,
                func,
                args,
                dst,
            } => self.call(ty, func, args, dst),
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

        let reg_args = if params.len() < 7 { params.len() } else { 6 };

        for i in 0..reg_args {
            let reg = &arg_regs[i];
            let var = self.expr(params[i].clone());
            codegen.emit(new_node!(Mov(
                Self::reg_for(reg.clone(), var.clone()),
                var.clone(),
                Self::operand_size(var.clone())
            )));
        }

        let stack_arg_count = params.len() - reg_args;

        for i in 0..stack_arg_count {
            let stack_par_idx: i32 = stack_arg_count as i32 - i as i32 - 1;
            let par =
                codegen.expr(params[stack_par_idx as usize + reg_args].clone());
            let mut from = new_node!(Var {
                off: -(stack_par_idx * 8 + 16),
                signed: false,
                size: Self::operand_size(par.clone())
            });
            let to = Self::reg_for(Register::R10, from.clone());
            let mut mov = codegen.mov(&mut from, &to);
            codegen.emit(mov);
            from = to.clone();
            mov = codegen.mov(&mut from, &par);
            codegen.emit(mov);
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
                Tac::ConstInt(value) => {
                    let initializer = new_node!(InitInt(*value));
                    self.emit(new_node!(StaticVar {
                        name: name.clone(),
                        global: global,
                        init: initializer
                    }));
                }
                Tac::ConstLong(value) => {
                    let initializer = new_node!(InitLong(*value));
                    self.emit(new_node!(StaticVar {
                        name: name.clone(),
                        global: global,
                        init: initializer
                    }));
                }
                _ => unreachable!(),
            }
        }
    }

    fn return_stmt(&mut self, expr: &TacRef) {
        let src = self.expr(expr.clone());
        let dst = Self::reg_for(Register::Rax, src.clone());

        if src != dst {
            self.code_vec.push(new_node!(Mov(
                src.clone(),
                dst.clone(),
                Self::operand_size(dst.clone())
            )));
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
            | Tac::LShift { .. }
            | Tac::RShift { .. }
            | Tac::And { .. }
            | Tac::Or { .. }
            | Tac::Xor { .. }
            | Tac::LessThan { .. }
            | Tac::LessOrEq { .. }
            | Tac::GreaterThan { .. }
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
            | Tac::Label(_) => {
                let expr = self.expr(node.clone());
                self.emit(expr);
            }
            Tac::Return(expr) => {
                self.return_stmt(expr);
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
        Code::Imm {
            val: _,
            signed: _,
            size,
        } => {
            if *size > 4 {
                return true;
            }
        }
        _ => {}
    }

    false
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
