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

fn create_var(idx: usize, var_map: &mut VarMap) -> AsmRef {
    var_map
        .entry(idx)
        .or_insert_with(|| asm_rc!(Var(idx, 4)))
        .clone()
}

fn create_label(idx: usize, label_map: &mut LabelMap) -> AsmRef {
    label_map
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
fn unop_fixup(
    operand: &mut AsmRef,
    arg_reg: Option<Asm>,
    asm_vec: &mut AsmVec,
) {
    if arg_reg.is_some() {
        let reg = Rc::new(RefCell::new(arg_reg.unwrap()));
        let mov = mov_op(operand.clone(), reg.clone());
        asm_vec.push(mov);
        *operand = reg.clone();
    }
}

fn binop_fixup(
    lhs: &mut AsmRef,
    rhs: &AsmRef,
    src_reg: Option<Asm>,
    dst_reg: Option<Asm>,
    tmp_reg: Asm,
    asm_vec: &mut AsmVec,
) -> AsmRef {
    let mut dst = rhs.clone();

    if src_reg.is_some() || dst_reg.is_some() {
        if src_reg.is_some() {
            let src = Rc::new(RefCell::new(src_reg.unwrap()));
            let mov = mov_op(lhs.clone(), src.clone());
            asm_vec.push(mov);
            *lhs = src;
        }
        if dst_reg.is_some() {
            let tmp = Rc::new(RefCell::new(dst_reg.unwrap()));
            let mov = mov_op(rhs.clone(), tmp.clone());
            asm_vec.push(mov);
            dst = tmp.clone();
        }
    } else if is_variable(lhs) && is_variable(rhs) {
        let tmp = Rc::new(RefCell::new(tmp_reg.clone()));
        let mov = mov_op(rhs.clone(), tmp.clone());
        asm_vec.push(mov);
        dst = tmp.clone();
    }

    if is_immediate(rhs) {
        let tmp = Rc::new(RefCell::new(tmp_reg));
        let mov = mov_op(rhs.clone(), tmp.clone());
        asm_vec.push(mov);
        dst = tmp.clone();
    }

    return dst;
}

fn mov_op(src: AsmRef, dst: AsmRef) -> AsmRef {
    match *dst.borrow() {
        Asm::Cl => {
            return asm_rc!(Movb(src.clone(), dst.clone()));
        }
        _ => {
            return asm_rc!(Movl(src.clone(), dst.clone()));
        }
    }
}

fn create_binop<F>(
    op_constructor: F,
    src: &mut AsmRef,
    final_dst: &AsmRef,
    tmp_reg: Asm,
    src_reg: Option<Asm>,
    dst_reg: Option<Asm>,
    asm_vec: &mut AsmVec,
) -> AsmRef
where
    F: Fn(AsmRef, AsmRef) -> Asm,
{
    let dst = binop_fixup(src, final_dst, src_reg, dst_reg, tmp_reg, asm_vec);

    let mut op =
        Rc::new(RefCell::new(op_constructor(src.clone(), dst.clone())));

    if dst != *final_dst {
        match *final_dst.borrow() {
            Asm::Imm(_) => {}
            _ => {
                asm_vec.push(op);
                op = mov_op(dst.clone(), final_dst.clone());
            }
        }
    }

    return op;
}

fn create_mov(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Movl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_addl(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Addl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_subl(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Subl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_shll(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Shll, src, dst, Asm::R10d, Some(Asm::Cl), None, asm_vec)
}

fn create_sarl(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Sarl, src, dst, Asm::R10d, Some(Asm::Cl), None, asm_vec)
}

fn create_and(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Andl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_or(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Orl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_xor(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Xorl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_cmpl(src: &mut AsmRef, dst: &AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    create_binop(Asm::Cmpl, src, dst, Asm::R10d, None, None, asm_vec)
}

fn create_imull(
    src: &mut AsmRef,
    dst: &AsmRef,
    asm_vec: &mut AsmVec,
) -> AsmRef {
    create_binop(
        Asm::IMull,
        src,
        dst,
        Asm::R10d,
        None,
        Some(Asm::R11d),
        asm_vec,
    )
}

fn create_idiv(src: AsmRef, asm_vec: &mut AsmVec) -> AsmRef {
    if matches!(*src.borrow(), Asm::Imm(_)) {
        let tmp = asm_rc!(R10d);
        let mov = mov_op(src, tmp.clone());
        asm_vec.push(mov);
        return asm_rc!(IDivl(tmp.clone()));
    } else {
        return asm_rc!(IDivl(src.clone()));
    }
}

fn transform_expr(
    node: Rc<TAC>,
    var_map: &mut VarMap,
    label_map: &mut LabelMap,
    asm_vec: &mut AsmVec,
) -> AsmRef {
    match &*node {
        TAC::ConstInt(val) => {
            return asm_rc!(Imm(*val));
        }

        TAC::Var(idx, _) => {
            return create_var(*idx, var_map);
        }

        TAC::Inv { src, dst } => {
            let src = transform_expr(src.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut src.clone(), &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(Notl(dst.clone()));
        }

        TAC::Neg { src, dst } => {
            let src = transform_expr(src.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut src.clone(), &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(Negl(dst.clone()));
        }

        TAC::Not { src, dst } => {
            let x = transform_expr(src.clone(), var_map, label_map, asm_vec);
            let imm = asm_rc!(Imm(0));
            let cmp = create_cmpl(&mut imm.clone(), &x, asm_vec);
            asm_vec.push(cmp);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut imm.clone(), &dst, asm_vec);
            asm_vec.push(mov);

            return asm_rc!(SetCC {
                cond: CondCode::E,
                dst: dst.clone(),
            });
        }

        TAC::Mul { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_imull(&mut y, &dst, asm_vec);
        }

        TAC::IDiv { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let reg = asm_rc!(Eax);
            let mov = create_mov(&mut x, &reg, asm_vec);
            asm_vec.push(mov);
            let cdq = asm_rc!(Cdq);
            asm_vec.push(cdq);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let idiv = create_idiv(y, asm_vec);
            asm_vec.push(idiv);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            return create_mov(&mut asm_rc!(Eax), &dst, asm_vec);
        }

        TAC::Mod { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let reg = asm_rc!(Eax);
            let mov = create_mov(&mut x, &reg, asm_vec);
            asm_vec.push(mov);
            let cdq = asm_rc!(Cdq);
            asm_vec.push(cdq);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let idiv = create_idiv(y, asm_vec);
            asm_vec.push(idiv);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            return create_mov(&mut asm_rc!(Edx), &dst, asm_vec);
        }

        TAC::Add { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_addl(&mut y, &dst, asm_vec);
        }

        TAC::Sub { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_subl(&mut y, &dst, asm_vec);
        }

        TAC::LShift { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_shll(&mut y, &dst, asm_vec);
        }

        TAC::RShift { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_sarl(&mut y, &dst, asm_vec);
        }

        TAC::And { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_and(&mut y, &dst, asm_vec);
        }

        TAC::Or { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_or(&mut y, &dst, asm_vec);
        }

        TAC::Xor { lhs, rhs, dst } => {
            let mut x =
                transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mov = create_mov(&mut x, &dst, asm_vec);
            asm_vec.push(mov);
            let mut y =
                transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            return create_xor(&mut y, &dst, asm_vec);
        }

        TAC::LessThan { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            asm_vec.push(cmp);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::L,
                dst: dst.clone(),
            });
        }

        TAC::LessOrEq { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            asm_vec.push(cmp);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::LE,
                dst: dst.clone(),
            });
        }

        TAC::GreaterThan { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            asm_vec.push(cmp);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::G,
                dst: dst.clone(),
            });
        }

        TAC::GreaterOrEq { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            asm_vec.push(cmp);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::GE,
                dst: dst.clone(),
            });
        }

        TAC::Equal { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            asm_vec.push(cmp);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::E,
                dst: dst.clone(),
            });
        }

        TAC::NotEq { lhs, rhs, dst } => {
            let x = transform_expr(lhs.clone(), var_map, label_map, asm_vec);
            let y = transform_expr(rhs.clone(), var_map, label_map, asm_vec);
            let cmp = create_cmpl(&mut y.clone(), &x, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            asm_vec.push(cmp);
            let mut imm = asm_rc!(Imm(0));
            let mov = create_mov(&mut imm, &dst, asm_vec);
            asm_vec.push(mov);
            return asm_rc!(SetCC {
                cond: CondCode::NE,
                dst: dst.clone(),
            });
        }

        TAC::Copy { src, dst } => {
            let mut src =
                transform_expr(src.clone(), var_map, label_map, asm_vec);
            let dst = transform_expr(dst.clone(), var_map, label_map, asm_vec);
            return create_mov(&mut src, &dst, asm_vec);
        }

        TAC::Jump(label) => {
            let label =
                transform_expr(label.clone(), var_map, label_map, asm_vec);
            return asm_rc!(Jmp(label));
        }

        TAC::JumpOnZero { expr, label } => {
            let src = transform_expr(expr.clone(), var_map, label_map, asm_vec);
            let imm = asm_rc!(Imm(0));
            let cmp = create_cmpl(&mut imm.clone(), &src, asm_vec);
            asm_vec.push(cmp);
            let label =
                transform_expr(label.clone(), var_map, label_map, asm_vec);
            return asm_rc!(JmpCC {
                cond: CondCode::E,
                label: label,
            });
        }

        TAC::JumpOnNotZero { expr, label } => {
            let src = transform_expr(expr.clone(), var_map, label_map, asm_vec);
            let imm = asm_rc!(Imm(0));
            let cmp = create_cmpl(&mut imm.clone(), &src, asm_vec);
            asm_vec.push(cmp);
            let label =
                transform_expr(label.clone(), var_map, label_map, asm_vec);
            return asm_rc!(JmpCC {
                cond: CondCode::NE,
                label: label,
            });
        }

        TAC::Label(idx) => {
            return create_label(*idx, label_map);
        }

        _ => {
            unreachable!()
        }
    }
}

fn transform(
    node: Rc<TAC>,
    var_map: &mut VarMap,
    label_map: &mut LabelMap,
    asm_vec: &mut AsmVec,
) {
    match &*node {
        TAC::Function { name, code, depth } => {
            let mut func_asm_vec: AsmVec = vec![];
            let mut func_var_map = VarMap::new();

            for op in code {
                transform(
                    op.clone(),
                    &mut func_var_map,
                    label_map,
                    &mut func_asm_vec,
                );
            }

            asm_vec.push(asm_rc!(Function {
                name: name.clone(),
                stack: *depth,
                asm: func_asm_vec,
            }));
        }

        TAC::Inv { src: _, dst: _ }
        | TAC::Neg { src: _, dst: _ }
        | TAC::Not { src: _, dst: _ }
        | TAC::Mul {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::IDiv {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Mod {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Add {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Sub {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::LShift {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::RShift {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::And {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Or {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Xor {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::LessThan {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::LessOrEq {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::GreaterThan {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::GreaterOrEq {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Equal {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::NotEq {
            lhs: _,
            rhs: _,
            dst: _,
        }
        | TAC::Copy { src: _, dst: _ }
        | TAC::Jump(_)
        | TAC::JumpOnZero { expr: _, label: _ }
        | TAC::JumpOnNotZero { expr: _, label: _ }
        | TAC::Label(_) => {
            let expr =
                transform_expr(node.clone(), var_map, label_map, asm_vec);
            asm_vec.push(expr);
        }

        TAC::Return(expr) => {
            let src = transform_expr(expr.clone(), var_map, label_map, asm_vec);
            let dst = asm_rc!(Eax);

            if src != dst {
                asm_vec.push(asm_rc!(Movl(src.clone(), dst.clone(),)));
            }

            asm_vec.push(asm_rc!(Ret));
        }

        _ => {
            unreachable!()
        }
    }
}

fn fixup(asm: AsmRef, var_map: &mut VarMap) {
    let op = &mut *asm.borrow_mut();
    match op {
        Asm::Function {
            name: _,
            stack: _,
            asm,
        } => {
            for op in asm {
                fixup(op.clone(), var_map);
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
            fixup(src.clone(), var_map);
            fixup(dst.clone(), var_map);
        }

        Asm::IDivl(src) => {
            fixup(src.clone(), var_map);
        }

        Asm::Movb(src, dst)
        | Asm::Movw(src, dst)
        | Asm::Movl(src, dst)
        | Asm::Movq(src, dst) => {
            fixup(src.clone(), var_map);
            fixup(dst.clone(), var_map);
        }

        Asm::Notl(dst) | Asm::Negl(dst) => {
            fixup(dst.clone(), var_map);
        }

        Asm::SetCC { dst, .. } => {
            fixup(dst.clone(), var_map);
        }

        Asm::Jmp(label) => {
            fixup(label.clone(), var_map);
        }

        Asm::JmpCC { label, .. } => {
            fixup(label.clone(), var_map);
        }

        Asm::Var(idx, size) => {
            *op = Asm::Stack(*idx, *size).try_into().unwrap();
        }

        _ => {}
    }
}

pub fn generate(tac_asm: Vec<Rc<TAC>>) -> AsmVec {
    let mut asm_vec: AsmVec = vec![];
    let mut var_map = VarMap::new();
    let mut label_map = LabelMap::new();

    for tac in tac_asm {
        transform(tac.clone(), &mut var_map, &mut label_map, &mut asm_vec);
    }

    for asm in &asm_vec {
        fixup(asm.clone(), &mut var_map);
    }

    return asm_vec;
}
