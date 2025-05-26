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

macro_rules! asm_rc {
    ($variant:ident) => {
        Rc::new(RefCell::new(Asm::$variant))
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(RefCell::new(Asm::$variant( $($args),* )))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(RefCell::new(Asm::$variant {
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
pub enum Asm {
    Function {
        name: String,
        stack: usize,
        asm: AsmVec,
    },
    Ret,
    Movb(AsmRef, AsmRef),
    Movw(AsmRef, AsmRef),
    Movl(AsmRef, AsmRef),
    Movq(AsmRef, AsmRef),
    Notl(AsmRef),
    Negl(AsmRef),
    IMull(AsmRef, AsmRef),
    IDivl(AsmRef),
    Addl(AsmRef, AsmRef),
    Subl(AsmRef, AsmRef),
    Shll(AsmRef, AsmRef),
    Sarl(AsmRef, AsmRef),
    Andl(AsmRef, AsmRef),
    Orl(AsmRef, AsmRef),
    Xorl(AsmRef, AsmRef),
    Cmpl(AsmRef, AsmRef),
    Imm(i64),
    Var(usize, usize),
    Label(usize),
    Stack(usize, usize),
    Jmp(AsmRef),
    JmpCC {
        cond: CondCode,
        label: AsmRef,
    },
    SetCC {
        cond: CondCode,
        dst: AsmRef,
    },
    Cdq,
    Al,
    Ax,
    Eax,
    Rax,
    Bl,
    Bx,
    Ebx,
    Rbx,
    Cl,
    Cx,
    Ecx,
    Rcx,
    Dl,
    Dx,
    Edx,
    Rdx,
    Sil,
    Si,
    Esi,
    Rsi,
    Dil,
    Di,
    Edi,
    Rdi,
    Spl,
    Sp,
    Esp,
    Rsp,
    Bpl,
    Bp,
    Ebp,
    Rbp,
    R8b,
    R8w,
    R8d,
    R8,
    R9b,
    R9w,
    R9d,
    R9,
    R10b,
    R10w,
    R10d,
    R10,
    R11b,
    R11w,
    R11d,
    R11,
    R12b,
    R12w,
    R12d,
    R12,
    R13b,
    R13w,
    R13d,
    R13,
    R14b,
    R14w,
    R14d,
    R14,
    R15b,
    R15w,
    R15d,
    R15,
}

type AsmRef = Rc<RefCell<Asm>>;
type AsmVec = Vec<AsmRef>;
type VarMap = HashMap<usize, AsmRef>;
type LabelMap = HashMap<usize, AsmRef>;

pub struct CodeGenerator {
    asm_vec: AsmVec,
    var_map: VarMap,
    label_map: LabelMap,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            asm_vec: vec![],
            var_map: VarMap::new(),
            label_map: LabelMap::new(),
        }
    }

    fn create_var(&mut self, idx: usize) -> AsmRef {
        self.var_map
            .entry(idx)
            .or_insert_with(|| asm_rc!(Var(idx, 4)))
            .clone()
    }

    fn create_label(&mut self, idx: usize) -> AsmRef {
        self.label_map
            .entry(idx)
            .or_insert_with(|| asm_rc!(Label(idx)))
            .clone()
    }

    fn is_immediate(asm: &AsmRef) -> bool {
        matches!(*asm.borrow(), Asm::Imm(_))
    }

    #[allow(dead_code)]
    fn is_variable(asm: &AsmRef) -> bool {
        matches!(*asm.borrow(), Asm::Var(_, _))
    }

    #[allow(dead_code)]
    fn is_register(asm: &AsmRef) -> bool {
        match *asm.borrow() {
            Asm::Al
            | Asm::Ax
            | Asm::Eax
            | Asm::Rax
            | Asm::Bl
            | Asm::Bx
            | Asm::Ebx
            | Asm::Rbx
            | Asm::Cl
            | Asm::Cx
            | Asm::Ecx
            | Asm::Rcx
            | Asm::Dl
            | Asm::Dx
            | Asm::Edx
            | Asm::Rdx
            | Asm::Sil
            | Asm::Si
            | Asm::Esi
            | Asm::Rsi
            | Asm::Dil
            | Asm::Di
            | Asm::Edi
            | Asm::Rdi
            | Asm::Spl
            | Asm::Sp
            | Asm::Esp
            | Asm::Rsp
            | Asm::Bpl
            | Asm::Bp
            | Asm::Ebp
            | Asm::Rbp
            | Asm::R8b
            | Asm::R8w
            | Asm::R8d
            | Asm::R8
            | Asm::R9b
            | Asm::R9w
            | Asm::R9d
            | Asm::R9
            | Asm::R10b
            | Asm::R10w
            | Asm::R10d
            | Asm::R10
            | Asm::R11b
            | Asm::R11w
            | Asm::R11d
            | Asm::R11
            | Asm::R12b
            | Asm::R12w
            | Asm::R12d
            | Asm::R12
            | Asm::R13b
            | Asm::R13w
            | Asm::R13d
            | Asm::R13
            | Asm::R14b
            | Asm::R14w
            | Asm::R14d
            | Asm::R14
            | Asm::R15b
            | Asm::R15w
            | Asm::R15d
            | Asm::R15 => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn unop_fixup(&mut self, operand: &mut AsmRef, arg_reg: Option<Asm>) {
        if let Some(reg_val) = arg_reg {
            let reg = Rc::new(RefCell::new(reg_val));
            let mov = self.mov_op(operand.clone(), reg.clone());
            self.asm_vec.push(mov);
            *operand = reg.clone();
        }
    }

    fn binop_fixup(
        &mut self,
        lhs: &mut AsmRef,
        rhs: &AsmRef,
        src_reg: Option<Asm>,
        dst_reg: Option<Asm>,
        tmp_reg: Asm,
    ) -> AsmRef {
        let mut dst = rhs.clone();

        if src_reg.is_some() || dst_reg.is_some() {
            if let Some(src_val) = src_reg {
                let src = Rc::new(RefCell::new(src_val));
                let mov = self.mov_op(lhs.clone(), src.clone());
                self.asm_vec.push(mov);
                *lhs = src;
            }
            if let Some(dst_val) = dst_reg {
                let tmp = Rc::new(RefCell::new(dst_val));
                let mov = self.mov_op(rhs.clone(), tmp.clone());
                self.asm_vec.push(mov);
                dst = tmp.clone();
            }
        } else if Self::is_variable(lhs) && Self::is_variable(rhs) {
            let tmp = Rc::new(RefCell::new(tmp_reg.clone()));
            let mov = self.mov_op(rhs.clone(), tmp.clone());
            self.asm_vec.push(mov);
            dst = tmp.clone();
        }

        if Self::is_immediate(rhs) {
            let tmp = Rc::new(RefCell::new(tmp_reg));
            let mov = self.mov_op(rhs.clone(), tmp.clone());
            self.asm_vec.push(mov);
            dst = tmp.clone();
        }

        dst
    }

    fn mov_op(&self, src: AsmRef, dst: AsmRef) -> AsmRef {
        match *dst.borrow() {
            Asm::Cl => {
                asm_rc!(Movb(src.clone(), dst.clone()))
            }
            _ => {
                asm_rc!(Movl(src.clone(), dst.clone()))
            }
        }
    }

    fn create_binop<F>(
        &mut self,
        op_constructor: F,
        src: &mut AsmRef,
        final_dst: &AsmRef,
        tmp_reg: Asm,
        src_reg: Option<Asm>,
        dst_reg: Option<Asm>,
    ) -> AsmRef
    where
        F: Fn(AsmRef, AsmRef) -> Asm,
    {
        let dst = self.binop_fixup(src, final_dst, src_reg, dst_reg, tmp_reg);

        let mut op =
            Rc::new(RefCell::new(op_constructor(src.clone(), dst.clone())));

        if dst != *final_dst {
            match *final_dst.borrow() {
                Asm::Imm(_) => {}
                _ => {
                    self.asm_vec.push(op);
                    op = self.mov_op(dst.clone(), final_dst.clone());
                }
            }
        }

        op
    }

    fn create_mov(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Movl, src, dst, Asm::R10d, None, None)
    }

    fn create_addl(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Addl, src, dst, Asm::R10d, None, None)
    }

    fn create_subl(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Subl, src, dst, Asm::R10d, None, None)
    }

    fn create_shll(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Shll, src, dst, Asm::R10d, Some(Asm::Cl), None)
    }

    fn create_sarl(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Sarl, src, dst, Asm::R10d, Some(Asm::Cl), None)
    }

    fn create_and(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Andl, src, dst, Asm::R10d, None, None)
    }

    fn create_or(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Orl, src, dst, Asm::R10d, None, None)
    }

    fn create_xor(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Xorl, src, dst, Asm::R10d, None, None)
    }

    fn create_cmpl(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(Asm::Cmpl, src, dst, Asm::R10d, None, None)
    }

    fn create_imull(&mut self, src: &mut AsmRef, dst: &AsmRef) -> AsmRef {
        self.create_binop(
            Asm::IMull,
            src,
            dst,
            Asm::R10d,
            None,
            Some(Asm::R11d),
        )
    }

    fn create_idiv(&mut self, src: AsmRef) -> AsmRef {
        if Self::is_immediate(&src) {
            let tmp = asm_rc!(R10d);
            let mov = self.mov_op(src, tmp.clone());
            self.asm_vec.push(mov);
            asm_rc!(IDivl(tmp.clone()))
        } else {
            asm_rc!(IDivl(src.clone()))
        }
    }

    fn transform_expr(&mut self, node: Rc<TAC>) -> AsmRef {
        match &*node {
            TAC::ConstInt(val) => {
                return asm_rc!(Imm(*val));
            }
            TAC::Var(idx, _) => {
                return self.create_var(*idx);
            }
            TAC::Inv { src, dst } => {
                let src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut src.clone(), &dst);
                self.asm_vec.push(mov);
                return asm_rc!(Notl(dst.clone()));
            }
            TAC::Neg { src, dst } => {
                let src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut src.clone(), &dst);
                self.asm_vec.push(mov);
                return asm_rc!(Negl(dst.clone()));
            }
            TAC::Not { src, dst } => {
                let x = self.transform_expr(src.clone());
                let imm = asm_rc!(Imm(0));
                let cmp = self.create_cmpl(&mut imm.clone(), &x);
                self.asm_vec.push(cmp);
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut imm.clone(), &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::E,
                    dst: dst.clone(),
                });
            }
            TAC::Mul { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_imull(&mut y, &dst);
            }
            TAC::IDiv { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let reg = asm_rc!(Eax);
                let mov = self.create_mov(&mut x, &reg);
                self.asm_vec.push(mov);
                let cdq = asm_rc!(Cdq);
                self.asm_vec.push(cdq);
                let y = self.transform_expr(rhs.clone());
                let idiv = self.create_idiv(y);
                self.asm_vec.push(idiv);
                let dst = self.transform_expr(dst.clone());
                return self.create_mov(&mut asm_rc!(Eax), &dst);
            }
            TAC::Mod { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let reg = asm_rc!(Eax);
                let mov = self.create_mov(&mut x, &reg);
                self.asm_vec.push(mov);
                let cdq = asm_rc!(Cdq);
                self.asm_vec.push(cdq);
                let y = self.transform_expr(rhs.clone());
                let idiv = self.create_idiv(y);
                self.asm_vec.push(idiv);
                let dst = self.transform_expr(dst.clone());
                return self.create_mov(&mut asm_rc!(Edx), &dst);
            }
            TAC::Add { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_addl(&mut y, &dst);
            }
            TAC::Sub { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_subl(&mut y, &dst);
            }
            TAC::LShift { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_shll(&mut y, &dst);
            }
            TAC::RShift { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_sarl(&mut y, &dst);
            }
            TAC::And { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_and(&mut y, &dst);
            }
            TAC::Or { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_or(&mut y, &dst);
            }
            TAC::Xor { lhs, rhs, dst } => {
                let mut x = self.transform_expr(lhs.clone());
                let dst = self.transform_expr(dst.clone());
                let mov = self.create_mov(&mut x, &dst);
                self.asm_vec.push(mov);
                let mut y = self.transform_expr(rhs.clone());
                return self.create_xor(&mut y, &dst);
            }
            TAC::LessThan { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.asm_vec.push(cmp);
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::L,
                    dst: dst.clone(),
                });
            }
            TAC::LessOrEq { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.asm_vec.push(cmp);
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::LE,
                    dst: dst.clone(),
                });
            }
            TAC::GreaterThan { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.asm_vec.push(cmp);
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::G,
                    dst: dst.clone(),
                });
            }
            TAC::GreaterOrEq { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.asm_vec.push(cmp);
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::GE,
                    dst: dst.clone(),
                });
            }
            TAC::Equal { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                self.asm_vec.push(cmp);
                let dst = self.transform_expr(dst.clone());
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::E,
                    dst: dst.clone(),
                });
            }
            TAC::NotEq { lhs, rhs, dst } => {
                let x = self.transform_expr(lhs.clone());
                let y = self.transform_expr(rhs.clone());
                let cmp = self.create_cmpl(&mut y.clone(), &x);
                let dst = self.transform_expr(dst.clone());
                self.asm_vec.push(cmp);
                let mut imm = asm_rc!(Imm(0));
                let mov = self.create_mov(&mut imm, &dst);
                self.asm_vec.push(mov);
                return asm_rc!(SetCC {
                    cond: CondCode::NE,
                    dst: dst.clone(),
                });
            }
            TAC::Copy { src, dst } => {
                let mut src = self.transform_expr(src.clone());
                let dst = self.transform_expr(dst.clone());
                return self.create_mov(&mut src, &dst);
            }
            TAC::Jump(label) => {
                let label = self.transform_expr(label.clone());
                return asm_rc!(Jmp(label));
            }
            TAC::JumpOnZero { expr, label } => {
                let src = self.transform_expr(expr.clone());
                let imm = asm_rc!(Imm(0));
                let cmp = self.create_cmpl(&mut imm.clone(), &src);
                self.asm_vec.push(cmp);
                let label = self.transform_expr(label.clone());
                return asm_rc!(JmpCC {
                    cond: CondCode::E,
                    label: label,
                });
            }
            TAC::JumpOnNotZero { expr, label } => {
                let src = self.transform_expr(expr.clone());
                let imm = asm_rc!(Imm(0));
                let cmp = self.create_cmpl(&mut imm.clone(), &src);
                self.asm_vec.push(cmp);
                let label = self.transform_expr(label.clone());
                return asm_rc!(JmpCC {
                    cond: CondCode::NE,
                    label: label,
                });
            }
            TAC::Label(idx) => {
                return self.create_label(*idx);
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn transform(&mut self, node: Rc<TAC>) {
        match &*node {
            TAC::Function { name, code, depth } => {
                let mut func_asm_vec: AsmVec = vec![];
                let func_var_map = VarMap::new();
                let func_label_map = LabelMap::new();
                let mut func_codegen = CodeGenerator {
                    asm_vec: vec![],
                    var_map: func_var_map,
                    label_map: func_label_map,
                };
                for op in code {
                    func_codegen.transform(op.clone());
                }
                func_asm_vec.append(&mut func_codegen.asm_vec);
                self.asm_vec.push(asm_rc!(Function {
                    name: name.clone(),
                    stack: *depth,
                    asm: func_asm_vec,
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
            | TAC::Label(_) => {
                let expr = self.transform_expr(node.clone());
                self.asm_vec.push(expr);
            }
            TAC::Return(expr) => {
                let src = self.transform_expr(expr.clone());
                let dst = asm_rc!(Eax);
                if src != dst {
                    self.asm_vec.push(asm_rc!(Movl(src.clone(), dst.clone(),)));
                }
                self.asm_vec.push(asm_rc!(Ret));
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn fixup(&mut self, asm: AsmRef) {
        let op = &mut *asm.borrow_mut();
        match op {
            Asm::Function {
                name: _,
                stack: _,
                asm,
            } => {
                for op in asm {
                    self.fixup(op.clone());
                }
            }
            Asm::IMull(src, dst)
            | Asm::Addl(src, dst)
            | Asm::Subl(src, dst)
            | Asm::Shll(src, dst)
            | Asm::Sarl(src, dst)
            | Asm::Andl(src, dst)
            | Asm::Orl(src, dst)
            | Asm::Xorl(src, dst)
            | Asm::Cmpl(src, dst) => {
                self.fixup(src.clone());
                self.fixup(dst.clone());
            }
            Asm::IDivl(src) => {
                self.fixup(src.clone());
            }
            Asm::Movb(src, dst)
            | Asm::Movw(src, dst)
            | Asm::Movl(src, dst)
            | Asm::Movq(src, dst) => {
                self.fixup(src.clone());
                self.fixup(dst.clone());
            }
            Asm::Notl(dst) | Asm::Negl(dst) => {
                self.fixup(dst.clone());
            }
            Asm::SetCC { dst, .. } => {
                self.fixup(dst.clone());
            }
            Asm::Jmp(label) => {
                self.fixup(label.clone());
            }
            Asm::JmpCC { label, .. } => {
                self.fixup(label.clone());
            }
            Asm::Var(idx, size) => {
                *op = Asm::Stack(*idx, *size).try_into().unwrap();
            }
            _ => {}
        }
    }

    pub fn generate(mut self, ir: Vec<Rc<TAC>>) -> AsmVec {
        for tac in ir {
            self.transform(tac.clone());
        }
        let asm_vec_clones: Vec<_> = self.asm_vec.iter().cloned().collect();
        for asm in asm_vec_clones {
            self.fixup(asm);
        }
        self.asm_vec
    }
}
