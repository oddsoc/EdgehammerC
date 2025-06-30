//  SPDX-License-Identifier: MIT
/*
 *  Copyright (c) 2025 Andrew Scott-Jones <andrew@edgehammer.io>
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

use crate::ir::TAC;

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
    E,
    NE,
    L,
    LE,
    G,
    GE,
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
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
        stack: usize,
        code: CodeVec,
    },
    Ret,
    Mov(CodeRef, CodeRef, usize),
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
    Cdq,
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
    pub fn new() -> Self {
        Self {
            code_vec: vec![],
            var_map: VarMap::new(),
            label_map: LabelMap::new(),
        }
    }

    fn new_label(&mut self, idx: usize) -> CodeRef {
        self.label_map
            .entry(idx)
            .or_insert_with(|| new_node!(Label(idx)))
            .clone()
    }

    #[allow(dead_code)]
    fn unop_fixup(&mut self, operand: &mut CodeRef, arg_reg: Option<Code>) {
        if let Some(reg_val) = arg_reg {
            let reg = Rc::new(RefCell::new(reg_val));
            let mov = self.new_direct_mov(operand.clone(), reg.clone());
            self.emit(mov);
            *operand = reg.clone();
        }
    }

    fn binop_fixup(
        &mut self,
        lhs: &mut CodeRef,
        rhs: &CodeRef,
        src_reg: Option<CodeRef>,
        dst_reg: Option<CodeRef>,
        tmp_reg: CodeRef,
    ) -> CodeRef {
        let mut dst = rhs.clone();

        if src_reg.is_some() || dst_reg.is_some() {
            if let Some(src_val) = src_reg {
                let src = src_val.clone();
                let mov = self.new_direct_mov(lhs.clone(), src.clone());
                self.emit(mov);
                *lhs = src;
            }
            if let Some(dst_val) = dst_reg {
                let tmp = dst_val.clone();
                let mov = self.new_direct_mov(rhs.clone(), tmp.clone());
                self.emit(mov);
                dst = tmp.clone();
            }
        } else if is_mem_addr(lhs) && is_mem_addr(rhs) {
            let tmp = tmp_reg.clone();
            let mov = self.new_direct_mov(lhs.clone(), tmp.clone());
            self.emit(mov);
            *lhs = tmp.clone();
        }

        if is_immediate(rhs) {
            let tmp = tmp_reg.clone();
            let mov = self.new_direct_mov(rhs.clone(), tmp.clone());
            self.emit(mov);
            dst = tmp.clone();
        }

        dst
    }

    fn operand_size(operand: CodeRef) -> usize {
        match &*operand.borrow() {
            Code::Reg { size, .. }
            | Code::Imm { size, .. }
            | Code::Var { size, .. } => *size,
            _ => {
                unreachable!();
            }
        }
    }

    fn new_reg_for(reg: Register, src: CodeRef) -> CodeRef {
        match &*src.borrow() {
            Code::Reg { size, signed, .. }
            | Code::Imm { size, signed, .. }
            | Code::Var { size, signed, .. } => {
                new_node!(Reg {
                    reg: reg,
                    signed: *signed,
                    size: *size
                })
            }
            _ => unreachable!(),
        }
    }

    fn new_direct_mov(&self, src: CodeRef, dst: CodeRef) -> CodeRef {
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
                } => {
                    if *dst_size > *src_size {
                        if *src_signed {
                            return new_node!(MovSignExt(
                                src.clone(),
                                dst.clone(),
                                *dst_size
                            ));
                        } else {
                            return new_node!(MovZeroExt(
                                src.clone(),
                                dst.clone(),
                                *dst_size
                            ));
                        }
                    } else {
                        return new_node!(Mov(
                            src.clone(),
                            dst.clone(),
                            *dst_size
                        ));
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn new_binop<F>(
        &mut self,
        op_constructor: F,
        src: &mut CodeRef,
        final_dst: &CodeRef,
        tmp_reg: CodeRef,
        src_reg: Option<CodeRef>,
        dst_reg: Option<CodeRef>,
    ) -> CodeRef
    where
        F: Fn(CodeRef, CodeRef, usize) -> Code,
    {
        let dst = self.binop_fixup(src, final_dst, src_reg, dst_reg, tmp_reg);

        let mut op = Rc::new(RefCell::new(op_constructor(
            src.clone(),
            dst.clone(),
            Self::operand_size(dst.clone()),
        )));

        if dst != *final_dst {
            match *final_dst.borrow() {
                Code::Imm { .. } => {}
                _ => {
                    self.emit(op);
                    op = self.new_direct_mov(dst.clone(), final_dst.clone());
                }
            }
        }

        op
    }

    fn new_mov(&mut self, src: &mut CodeRef, final_dst: &CodeRef) -> CodeRef {
        let dst = self.binop_fixup(
            src,
            final_dst,
            None,
            None,
            Self::new_reg_for(Register::R10, final_dst.clone()),
        );

        let mut op = self.new_direct_mov(src.clone(), dst.clone());

        if dst != *final_dst {
            self.emit(op);
            op = self.new_direct_mov(dst, final_dst.clone());
        }

        op
    }

    fn new_add(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Add,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_sub(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Sub,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_shl(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Shl,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            Some(new_node!(Reg {
                reg: Register::RCX,
                signed: false,
                size: 1
            })),
            None,
        )
    }

    fn new_sar(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Sar,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            Some(new_node!(Reg {
                reg: Register::RCX,
                signed: false,
                size: 1
            })),
            None,
        )
    }

    fn new_and(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::And,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_or(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Or,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_xor(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Xor,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_cmp(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::Cmp,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            None,
        )
    }

    fn new_imul(&mut self, src: &mut CodeRef, dst: &CodeRef) -> CodeRef {
        self.new_binop(
            Code::IMul,
            src,
            dst,
            Self::new_reg_for(Register::R10, dst.clone()),
            None,
            Some(Self::new_reg_for(Register::R11, dst.clone())),
        )
    }

    fn new_idiv(&mut self, src: CodeRef) -> CodeRef {
        if is_immediate(&src) {
            let tmp = Self::new_reg_for(Register::R10, src.clone());
            let mov = self.new_direct_mov(src.clone(), tmp.clone());
            self.emit(mov);
            new_node!(IDiv(tmp.clone(), Self::operand_size(tmp.clone())))
        } else {
            new_node!(IDiv(src.clone(), Self::operand_size(src.clone())))
        }
    }

    #[allow(unused_variables)]
    fn transform_expr(&mut self, node: Rc<TAC>) -> CodeRef {
        match &*node {
            TAC::ConstInt(val) => {
                let imm = new_node!(Imm {
                    val: *val,
                    signed: true,
                    size: 4
                });
                return imm;
            }
            TAC::Var(ty, off) => {
                let ty_val = ty.borrow();
                return self
                    .var_map
                    .entry(*off)
                    .or_insert_with(|| {
                        new_node!(Var {
                            off: *off as i32,
                            signed: ty_val.signed,
                            size: ty_val.size
                        })
                    })
                    .clone();
            }
            TAC::Inv { ty, src, dst } => {
                let src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut src.clone(), &dst);
                self.emit(mov);
                return new_node!(Not(
                    dst.clone(),
                    Self::operand_size(dst.clone())
                ));
            }
            TAC::Neg { ty, src, dst } => {
                let src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut src.clone(), &dst);
                self.emit(mov);
                return new_node!(Neg(
                    dst.clone(),
                    Self::operand_size(dst.clone())
                ));
            }
            TAC::Not { ty, src, dst } => {
                let x = self.transform_expr(src.clone());
                let imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(x.clone())
                });
                let cmp = self.new_cmp(&mut imm.clone(), &x);
                self.emit(cmp);
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut imm.clone(), &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::E,
                    dst: dst.clone(),
                });
            }
            TAC::Mul { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_imul(&mut y, &dst);
            }
            TAC::IDiv { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let reg = Self::new_reg_for(Register::RAX, x.clone());
                let mov = self.new_mov(&mut x, &reg);
                self.emit(mov);
                let cdq = new_node!(Cdq);
                self.emit(cdq);
                let y = self.transform_expr(rhs.clone());
                let idiv = self.new_idiv(y);
                self.emit(idiv);
                let dst = self.transform_expr(dst.clone());
                return self.new_mov(
                    &mut Self::new_reg_for(Register::RAX, dst.clone()),
                    &dst,
                );
            }
            TAC::Mod { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let reg = Self::new_reg_for(Register::RAX, x.clone());
                let mov = self.new_mov(&mut x, &reg);
                self.emit(mov);
                let cdq = new_node!(Cdq);
                self.emit(cdq);
                let y = self.transform_expr(rhs.clone());
                let idiv = self.new_idiv(y);
                self.emit(idiv);
                let dst = self.transform_expr(dst.clone());
                return self.new_mov(
                    &mut Self::new_reg_for(Register::RDX, dst.clone()),
                    &dst,
                );
            }
            TAC::Add { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_add(&mut y, &dst);
            }
            TAC::Sub { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_sub(&mut y, &dst);
            }
            TAC::LShift { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_shl(&mut y, &dst);
            }
            TAC::RShift { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_sar(&mut y, &dst);
            }
            TAC::And { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_and(&mut y, &dst);
            }
            TAC::Or { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_or(&mut y, &dst);
            }
            TAC::Xor { ty, lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.new_mov(&mut x, &dst);
                self.emit(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.new_xor(&mut y, &dst);
            }
            TAC::LessThan { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.emit(cmp);
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::L,
                    dst: dst.clone(),
                });
            }
            TAC::LessOrEq { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.emit(cmp);
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::LE,
                    dst: dst.clone(),
                });
            }
            TAC::GreaterThan { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.emit(cmp);
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::G,
                    dst: dst.clone(),
                });
            }
            TAC::GreaterOrEq { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.emit(cmp);
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::GE,
                    dst: dst.clone(),
                });
            }
            TAC::Equal { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                self.emit(cmp);
                let dst = self.transform_expr(dst.clone());
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::E,
                    dst: dst.clone(),
                });
            }
            TAC::NotEq { ty, lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.new_cmp(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.emit(cmp);
                let mut imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(dst.clone())
                });
                let mov = self.new_mov(&mut imm, &dst);
                self.emit(mov);
                return new_node!(SetCC {
                    cond: CondCode::NE,
                    dst: dst.clone(),
                });
            }
            TAC::Copy { ty, src, dst } => {
                let mut src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                return self.new_mov(&mut src, &dst);
            }
            TAC::Jump(label) => {
                let label = self.transform_expr(label.clone());
                return new_node!(Jmp(label));
            }
            TAC::JumpOnZero { ty, expr, label } => {
                let src = self.transform_expr(expr.clone());
                let imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(src.clone())
                });
                let cmp = self.new_cmp(&mut imm.clone(), &src);
                self.emit(cmp);
                let label = self.transform_expr(label.clone());
                return new_node!(JmpCC {
                    cond: CondCode::E,
                    label: label,
                });
            }
            TAC::JumpOnNotZero { ty, expr, label } => {
                let src = self.transform_expr(expr.clone());
                let imm = new_node!(Imm {
                    val: 0,
                    signed: true,
                    size: Self::operand_size(src.clone())
                });
                let cmp = self.new_cmp(&mut imm.clone(), &src);
                self.emit(cmp);
                let label = self.transform_expr(label.clone());
                return new_node!(JmpCC {
                    cond: CondCode::NE,
                    label: label,
                });
            }
            TAC::Label(idx) => {
                return self.new_label(*idx);
            }

            TAC::FunctionRef(name, defined) => {
                return new_node!(FunctionRef(name.clone(), *defined));
            }

            TAC::Call {
                ty,
                func,
                args,
                dst,
            } => {
                let arg_regs = vec![
                    Register::RDI,
                    Register::RSI,
                    Register::RDX,
                    Register::RCX,
                    Register::R8,
                    Register::R9,
                ];

                let reg_args: Vec<Rc<TAC>> =
                    args.iter().take(6).cloned().collect();

                let stack_args: Vec<Rc<TAC>> =
                    args.iter().skip(6).cloned().collect();

                let stack_padding =
                    if (stack_args.len() & 1) != 0 { 8 } else { 0 };

                if stack_padding != 0 {
                    self.emit(new_node!(PushBytes(stack_padding)));
                }

                for (i, arg) in reg_args.iter().enumerate() {
                    let reg = &arg_regs[i];
                    let src = self.transform_expr(arg.clone());
                    let dst =
                        Self::new_reg_for(reg.clone().into(), src.clone());
                    self.emit(new_node!(Mov(
                        src.clone(),
                        dst.clone(),
                        Self::operand_size(dst.clone())
                    )));
                }

                for arg in stack_args.iter().rev() {
                    let src = self.transform_expr(arg.clone());
                    if is_register(&src) || is_immediate(&src) {
                        self.emit(new_node!(Push(src)));
                    } else {
                        let dst = Self::new_reg_for(Register::RAX, src.clone());
                        self.emit(new_node!(Mov(
                            src.clone(),
                            dst.clone(),
                            Self::operand_size(dst.clone())
                        )));
                        self.emit(new_node!(Push(new_node!(Reg {
                            reg: Register::RAX,
                            signed: false,
                            size: 8
                        }))));
                    }
                }

                let callee = self.transform_expr(func.clone());
                let call = new_node!(Call(callee));

                self.emit(call);

                let pop_bytes = 8 * stack_args.len() + stack_padding;

                if pop_bytes > 0 {
                    self.emit(new_node!(PopBytes(pop_bytes)));
                }

                let res = self.transform_expr(dst.clone());
                let mov = new_node!(Mov(
                    Self::new_reg_for(Register::RAX, res.clone()),
                    res.clone(),
                    Self::operand_size(res.clone())
                ));
                mov
            }

            _ => {
                unreachable!()
            }
        }
    }

    fn transform(&mut self, node: Rc<TAC>) {
        match &*node {
            TAC::Function {
                name,
                params,
                code,
                depth,
            } => {
                let mut codegen = CodeGenerator::new();

                let arg_regs = vec![
                    Register::RDI,
                    Register::RSI,
                    Register::RDX,
                    Register::RCX,
                    Register::R8,
                    Register::R9,
                ];

                let reg_args = if params.len() < 7 { params.len() } else { 6 };

                for i in 0..reg_args {
                    let reg = &arg_regs[i];
                    let var = self.transform_expr(params[i].clone());
                    codegen.emit(new_node!(Mov(
                        Self::new_reg_for(reg.clone().into(), var.clone()),
                        var.clone(),
                        Self::operand_size(var.clone())
                    )));
                }

                let stack_arg_count = params.len() - reg_args;

                for i in 0..stack_arg_count {
                    let stack_par_idx: i32 =
                        stack_arg_count as i32 - i as i32 - 1;
                    let par = codegen.transform_expr(
                        params[stack_par_idx as usize + reg_args].clone(),
                    );
                    let mut from = new_node!(Var {
                        off: -(stack_par_idx * 8 + 16),
                        signed: false,
                        size: Self::operand_size(par.clone())
                    });
                    let to = Self::new_reg_for(Register::R10, from.clone());
                    let mut mov = codegen.new_mov(&mut from, &to);
                    codegen.emit(mov);
                    from = to.clone();
                    mov = codegen.new_mov(&mut from, &par);
                    codegen.emit(mov);
                }

                for op in code {
                    codegen.transform(op.clone());
                }

                self.emit(new_node!(Function {
                    name: name.clone(),
                    stack: (*depth + 15) & !15,
                    code: codegen.out(),
                }));
            }
            TAC::Inv { .. }
            | TAC::Neg { .. }
            | TAC::Not { .. }
            | TAC::Mul { .. }
            | TAC::IDiv { .. }
            | TAC::Mod { .. }
            | TAC::Add { .. }
            | TAC::Sub { .. }
            | TAC::LShift { .. }
            | TAC::RShift { .. }
            | TAC::And { .. }
            | TAC::Or { .. }
            | TAC::Xor { .. }
            | TAC::LessThan { .. }
            | TAC::LessOrEq { .. }
            | TAC::GreaterThan { .. }
            | TAC::GreaterOrEq { .. }
            | TAC::Equal { .. }
            | TAC::NotEq { .. }
            | TAC::Copy { .. }
            | TAC::Jump(_)
            | TAC::JumpOnZero { .. }
            | TAC::JumpOnNotZero { .. }
            | TAC::Call { .. }
            | TAC::Label(_) => {
                let expr = self.transform_expr(node.clone());
                self.emit(expr);
            }
            TAC::Return(expr) => {
                let src = self.transform_expr(expr.clone());
                let dst = Self::new_reg_for(Register::RAX, src.clone());

                if src != dst {
                    self.code_vec.push(new_node!(Mov(
                        src.clone(),
                        dst.clone(),
                        Self::operand_size(dst.clone())
                    )));
                }

                self.emit(new_node!(Ret));
            }
            _ => {
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

    pub fn generate(mut self, ir: Vec<Rc<TAC>>) -> CodeVec {
        for tac in ir {
            self.transform(tac.clone());
        }
        self.code_vec
    }
}

fn is_immediate(code: &CodeRef) -> bool {
    matches!(*code.borrow(), Code::Imm { .. })
}

#[allow(dead_code)]
fn is_mem_addr(code: &CodeRef) -> bool {
    match *code.borrow() {
        Code::Var { .. } => true,
        _ => false,
    }
}

#[allow(dead_code)]
fn is_register(code: &CodeRef) -> bool {
    match *code.borrow() {
        Code::Reg { .. } => true,
        _ => false,
    }
}
