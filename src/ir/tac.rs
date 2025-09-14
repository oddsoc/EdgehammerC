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
use std::rc::Rc;

use crate::ast::*;
use crate::ir::IrGenerator;
use crate::scope::*;
use crate::types::*;

macro_rules! new_node {
    ($variant:ident) => {
        Rc::new(Tac::$variant)
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(Tac::$variant( $($args),* ))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(Tac::$variant {
            $($field: $value),*
        })
    };
}

pub type TacRef = Rc<Tac>;

#[derive(Debug, PartialEq, Clone)]
pub enum Tac {
    StaticInitializer(TypeRef, TacRef),
    Function {
        name: String,
        global: bool,
        params: Vec<TacRef>,
        code: Vec<TacRef>,
        depth: usize,
    },
    Call {
        ty: TypeRef,
        func: TacRef,
        args: Vec<TacRef>,
        dst: TacRef,
    },
    Integer {
        ty: TypeRef,
        value: u64,
    },
    Double(f64),
    Var(TypeRef, usize),
    StaticVar(TypeRef, String, bool, TacRef),
    DoubleToInt {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    DoubleToUlong {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    IntToDouble {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    SignExt {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    ZeroExt {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Truncate {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Inv {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Neg {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Not {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Mul {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Div {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Mod {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Add {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Sub {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    LeftShift {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    RightShift {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    And {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Or {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Xor {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Equal {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    NotEq {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Less {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    LessOrEq {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Greater {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    GreaterOrEq {
        ty: TypeRef,
        lhs: TacRef,
        rhs: TacRef,
        dst: TacRef,
    },
    Copy {
        ty: TypeRef,
        src: TacRef,
        dst: TacRef,
    },
    Jump(TacRef),
    JumpOnZero {
        ty: TypeRef,
        expr: TacRef,
        label: TacRef,
    },
    JumpOnNotZero {
        ty: TypeRef,
        expr: TacRef,
        label: TacRef,
    },
    Label(usize),
    FunctionRef(String, bool),
    StaticVarRef(TypeRef, String),
    Return(TypeRef, TacRef),
    RoData(TypeRef, String, u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Fp64Bits(u64);

impl From<f64> for Fp64Bits {
    fn from(f: f64) -> Self {
        Self(f.to_bits())
    }
}

impl From<Fp64Bits> for f64 {
    fn from(bits: Fp64Bits) -> Self {
        f64::from_bits(bits.0)
    }
}

pub struct TacGenerator {
    label_idx: usize,
    label_map: HashMap<String, usize>,
    fp_consts: HashMap<Fp64Bits, usize>,
    tac_code: Vec<TacRef>,
}

impl IrGenerator for TacGenerator {
    type Ir = Vec<TacRef>;

    fn new() -> Self {
        Self {
            label_idx: 0,
            label_map: HashMap::new(),
            fp_consts: HashMap::new(),
            tac_code: Vec::new(),
        }
    }

    fn lower(&mut self, ast: Vec<AstRef>) -> Self::Ir {
        let mut scope: Option<ScopeRef> = None;
        self.tac_code.clear();
        for node in ast {
            scope = Some(node.as_ref().borrow().scope.clone());
            self.stmt_or_decl(node);
        }
        if let Some(s) = scope {
            let tac_vec = self.symbols(upto_top(s.clone()));
            self.tac_code.extend(tac_vec);
        }
        self.tac_code.clone()
    }
}

impl TacGenerator {
    pub fn from(parent: &TacGenerator) -> Self {
        Self {
            label_idx: parent.label_idx,
            label_map: HashMap::new(),
            fp_consts: parent.fp_consts.clone(),
            tac_code: Vec::new(),
        }
    }

    pub fn join(&mut self, child: &TacGenerator) {
        self.fp_consts.extend(child.fp_consts.clone());
        self.label_idx = child.label_idx;
    }

    fn incr_label_idx(&mut self) -> usize {
        let idx = self.label_idx;
        self.label_idx += 1;
        idx
    }

    fn const_double(&mut self, value: f64) -> TacRef {
        let key: Fp64Bits = value.into();
        let idx: usize;

        if !self.fp_consts.contains_key(&key) {
            idx = self.incr_label_idx();
            self.fp_consts.insert(key, idx);
        } else {
            idx = *self.fp_consts.get(&key).unwrap();
        }

        let name = format!(".L{idx}");
        new_node!(StaticVarRef(double_type(), name.clone()))
    }

    fn named_label(&mut self, name: &str) -> TacRef {
        if !self.label_map.contains_key(name) {
            let idx = self.incr_label_idx();
            self.label_map.insert(name.to_string(), idx);
        }
        new_node!(Label(*self.label_map.get(name).unwrap()))
    }

    fn label(&mut self) -> TacRef {
        let idx = self.incr_label_idx();
        new_node!(Label(idx))
    }

    fn continue_label(&mut self, scope: &ScopeRef) -> TacRef {
        let loop_scope = upto(scope.clone(), ScopeKind::Loop).unwrap();
        let id = loop_scope.borrow().id;
        let label = format!(".continue.{}", id);
        self.named_label(&label)
    }

    fn break_label(&mut self, scope: &ScopeRef) -> TacRef {
        let loop_or_switch =
            upto_any(scope.clone(), &[ScopeKind::Loop, ScopeKind::Switch])
                .unwrap();
        let id = loop_or_switch.borrow().id;
        let label = format!(".break.{}", id);
        self.named_label(&label)
    }

    fn case_label(&mut self, scope: &ScopeRef, case_idx: usize) -> TacRef {
        let switch_scope = upto(scope.clone(), ScopeKind::Switch).unwrap();
        let id = switch_scope.borrow().id;
        let label = format!(".case.{}.{}", case_idx, id);
        self.named_label(&label)
    }

    fn tmp_var(&mut self, ty: TypeRef, scope: &ScopeRef) -> TacRef {
        let ty_val = ty.as_ref().borrow();
        let pos = make_space(scope.clone(), ty_val.size, ty_val.alignment);
        new_node!(Var(ty.clone(), pos))
    }

    fn binop(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> (TacRef, TacRef, TacRef) {
        let lhs = self.expr(left.clone());
        let rhs = self.expr(right.clone());
        let dst = self.tmp_var(node.ty.clone(), &node.scope);

        (lhs, rhs, dst)
    }

    fn unop(
        &mut self,
        scope: &ScopeRef,
        ty: &TypeRef,
        subexpr: &AstRef,
    ) -> (TacRef, TacRef) {
        let src = self.expr(subexpr.clone());
        let dst = self.tmp_var(ty.clone(), scope);

        (src, dst)
    }

    fn post_incr_or_decr(
        &mut self,
        scope: &ScopeRef,
        ty: &TypeRef,
        subexpr: &AstRef,
        incr: bool,
    ) -> TacRef {
        let src = self.expr(subexpr.clone());
        let dst = self.tmp_var(ty.clone(), scope);
        let tmp = self.tmp_var(ty.clone(), scope);

        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: src.clone(),
            dst: tmp.clone()
        }));

        let one = if is_double_type(ty) {
            self.const_double(1.0)
        } else {
            new_node!(Integer {
                ty: ty.clone(),
                value: 1
            })
        };

        if incr {
            self.emit(new_node!(Add {
                ty: ty.clone(),
                lhs: one.clone(),
                rhs: tmp.clone(),
                dst: dst.clone()
            }));
        } else {
            self.emit(new_node!(Sub {
                ty: ty.clone(),
                lhs: tmp.clone(),
                rhs: one.clone(),
                dst: dst.clone()
            }));
        }

        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: dst.clone(),
            dst: src.clone()
        }));

        tmp
    }

    fn logical_and(
        &mut self,
        scope: &ScopeRef,
        ty: &TypeRef,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let false_label = self.label();
        let end_label = self.label();
        let x = self.expr(left.clone());
        self.emit(new_node!(JumpOnZero {
            ty: ty.clone(),
            expr: x,
            label: false_label.clone()
        }));
        let y = self.expr(right.clone());
        self.emit(new_node!(JumpOnZero {
            ty: ty.clone(),
            expr: y,
            label: false_label.clone()
        }));
        let dst = self.tmp_var(ty.clone(), scope);
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: new_node!(Integer {
                ty: ty.clone(),
                value: 1
            }),
            dst: dst.clone()
        }));
        self.emit(new_node!(Jump(end_label.clone())));
        self.emit(false_label);
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: new_node!(Integer {
                ty: ty.clone(),
                value: 0
            }),
            dst: dst.clone()
        }));
        self.emit(end_label);
        dst
    }

    fn logical_or(
        &mut self,
        scope: &ScopeRef,
        ty: &TypeRef,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let true_label = self.label();
        let end_label = self.label();
        let x = self.expr(left.clone());
        self.emit(new_node!(JumpOnNotZero {
            ty: ty.clone(),
            expr: x,
            label: true_label.clone()
        }));
        let y = self.expr(right.clone());
        self.emit(new_node!(JumpOnNotZero {
            ty: ty.clone(),
            expr: y,
            label: true_label.clone()
        }));
        let dst = self.tmp_var(ty.clone(), scope);
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: new_node!(Integer {
                ty: ty.clone(),
                value: 0
            }),
            dst: dst.clone()
        }));
        self.emit(new_node!(Jump(end_label.clone())));
        self.emit(true_label);
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: new_node!(Integer {
                ty: ty.clone(),
                value: 1
            }),
            dst: dst.clone()
        }));
        self.emit(end_label);
        dst
    }

    fn assign(
        &mut self,
        ty: &TypeRef,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let src = self.expr(right.clone());
        let dst = self.expr(left.clone());

        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: src,
            dst: dst.clone()
        }));

        dst
    }

    fn ternary(
        &mut self,
        scope: &ScopeRef,
        ty: &TypeRef,
        left: &AstRef,
        middle: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let e2_label = self.label();
        let c = self.expr(left.clone());
        self.emit(new_node!(JumpOnZero {
            ty: left.borrow().ty.clone(),
            expr: c,
            label: e2_label.clone()
        }));
        let e1 = self.expr(middle.clone());
        let res = self.tmp_var(ty.clone(), scope);
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: e1.clone(),
            dst: res.clone()
        }));
        let end_label = self.label();
        self.emit(new_node!(Jump(end_label.clone())));
        self.emit(e2_label.clone());
        let e2 = self.expr(right.clone());
        self.emit(new_node!(Copy {
            ty: ty.clone(),
            src: e2.clone(),
            dst: res.clone()
        }));
        self.emit(end_label.clone());
        res
    }

    fn multiply(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Mul {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn divide(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Div {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn modulo(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Mod {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn add(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Add {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn subtract(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Sub {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn shift_left(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(LeftShift {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn shift_right(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(RightShift {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn and(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(And {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn or(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Or {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn xor(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Xor {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn equal(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Equal {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn not_eq(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(NotEq {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn less(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Less {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn less_or_eq(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(LessOrEq {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn greater(&mut self, node: &Ast, left: &AstRef, right: &AstRef) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(Greater {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn greater_or_eq(
        &mut self,
        node: &Ast,
        left: &AstRef,
        right: &AstRef,
    ) -> TacRef {
        let (lhs, rhs, dst) = self.binop(&node, &left, &right);

        self.emit(new_node!(GreaterOrEq {
            ty: node.ty.clone(),
            lhs: lhs,
            rhs: rhs,
            dst: dst.clone()
        }));

        dst
    }

    fn identifier(
        &mut self,
        node: &Ast,
        expr: &AstRef,
        name: &String,
    ) -> TacRef {
        let sym = resolve(&expr).unwrap();
        let is_defined = has_def(node.scope.clone(), name);
        let sym_node = sym_as_node(sym.clone());
        let sym_type = sym_node.as_ref().unwrap().borrow().ty.clone();

        match &sym_node.unwrap().borrow().kind {
            AstKind::Function { name, .. } => {
                new_node!(FunctionRef(name.clone(), is_defined))
            }
            _ => {
                if has_static_storage_duration(sym.clone()) {
                    if has_linkage(sym.clone()) {
                        new_node!(StaticVarRef(sym_type, name.clone()))
                    } else {
                        let at_scope = sym.borrow().at_scope;
                        let mangled_name = format!("{}.{}", name, at_scope);
                        new_node!(StaticVarRef(sym_type, mangled_name.clone()))
                    }
                } else {
                    new_node!(Var(node.ty.clone(), sym.borrow().pos))
                }
            }
        }
    }

    fn call(&mut self, node: &Ast, expr: &AstRef, args: &[AstRef]) -> TacRef {
        let mut arg_exprs: Vec<TacRef> = vec![];

        for arg in args {
            arg_exprs.push(self.expr(arg.clone()));
        }

        let dst = self.tmp_var(node.ty.clone(), &node.scope);
        let fun = self.expr(expr.clone());

        self.emit(new_node!(Call {
            ty: node.ty.clone(),
            func: fun,
            args: arg_exprs,
            dst: dst.clone()
        }));

        dst
    }

    fn cast_double_to_ulong(
        &mut self,
        node: &Ast,
        src: TacRef,
        dst: TacRef,
        scope: &ScopeRef,
    ) {
        let out_of_range = self.label();
        let end = self.label();
        let upper_bound = self.const_double((i64::MAX as u64 + 1) as f64);

        let tmp0 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(GreaterOrEq {
            ty: node.ty.clone(),
            lhs: src.clone(),
            rhs: upper_bound.clone(),
            dst: tmp0.clone()
        }));

        self.emit(new_node!(JumpOnNotZero {
            ty: node.ty.clone(),
            expr: tmp0.clone(),
            label: out_of_range.clone()
        }));

        self.emit(new_node!(DoubleToUlong {
            ty: node.ty.clone(),
            src: src.clone(),
            dst: dst.clone()
        }));
        self.emit(new_node!(Jump(end.clone())));

        self.emit(out_of_range.clone());
        let tmp1 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(Sub {
            ty: node.ty.clone(),
            lhs: src.clone(),
            rhs: upper_bound.clone(),
            dst: tmp1.clone()
        }));
        let tmp2 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(DoubleToUlong {
            ty: node.ty.clone(),
            src: tmp1.clone(),
            dst: tmp2.clone()
        }));
        let tmp3 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(Add {
            ty: node.ty.clone(),
            lhs: tmp2.clone(),
            rhs: new_node!(Integer {
                ty: node.ty.clone(),
                value: 9223372036854775808
            }),
            dst: tmp3.clone()
        }));
        self.emit(new_node!(Copy {
            ty: node.ty.clone(),
            src: tmp3.clone(),
            dst: dst.clone()
        }));
        self.emit(end.clone());
    }

    fn cast_ulong_to_double(
        &mut self,
        node: &Ast,
        src: TacRef,
        dst: TacRef,
        scope: &ScopeRef,
    ) {
        let out_of_range = self.label();
        let end = self.label();

        let zero = new_node!(Integer {
            ty: long_type(false),
            value: 0
        });
        let tmp0 = self.tmp_var(int_type(true), scope);
        self.emit(new_node!(Less {
            ty: long_type(true),
            lhs: src.clone(),
            rhs: zero.clone(),
            dst: tmp0.clone()
        }));

        self.emit(new_node!(JumpOnNotZero {
            ty: long_type(true),
            expr: tmp0.clone(),
            label: out_of_range.clone()
        }));

        self.emit(new_node!(IntToDouble {
            ty: node.ty.clone(),
            src: src.clone(),
            dst: dst.clone()
        }));
        self.emit(new_node!(Jump(end.clone())));

        self.emit(out_of_range.clone());

        let tmp1 = self.tmp_var(long_type(false), scope);

        self.emit(new_node!(Copy {
            ty: long_type(false),
            src: src.clone(),
            dst: tmp1.clone()
        }));

        let tmp2 = self.tmp_var(long_type(false), scope);
        self.emit(new_node!(Copy {
            ty: long_type(false),
            src: tmp1.clone(),
            dst: tmp2.clone()
        }));
        let tmp2_shr = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(RightShift {
            ty: long_type(false),
            lhs: tmp2.clone(),
            rhs: new_node!(Integer {
                ty: long_type(false),
                value: 1
            }),
            dst: tmp2_shr.clone()
        }));

        let tmp3 = self.tmp_var(long_type(false), scope);
        self.emit(new_node!(And {
            ty: long_type(false),
            lhs: tmp1.clone(),
            rhs: new_node!(Integer {
                ty: long_type(false),
                value: 1
            }),
            dst: tmp3.clone()
        }));

        let tmp4 = self.tmp_var(long_type(false), scope);
        self.emit(new_node!(Or {
            ty: long_type(false),
            lhs: tmp2_shr.clone(),
            rhs: tmp3.clone(),
            dst: tmp4.clone()
        }));

        let tmp5 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(IntToDouble {
            ty: node.ty.clone(),
            src: tmp4.clone(),
            dst: tmp5.clone()
        }));

        let tmp6 = self.tmp_var(node.ty.clone(), scope);
        self.emit(new_node!(Add {
            ty: node.ty.clone(),
            lhs: tmp5.clone(),
            rhs: tmp5.clone(),
            dst: tmp6.clone()
        }));

        self.emit(new_node!(Copy {
            ty: node.ty.clone(),
            src: tmp6.clone(),
            dst: dst.clone()
        }));

        self.emit(end.clone());
    }

    fn cast_double_to_uint(
        &mut self,
        node: &Ast,
        src: TacRef,
        dst: TacRef,
        scope: &ScopeRef,
    ) {
        let tmp = self.tmp_var(long_type(true), scope);
        self.emit(new_node!(DoubleToInt {
            ty: long_type(true),
            src: src,
            dst: tmp.clone()
        }));
        self.emit(new_node!(Truncate {
            ty: node.ty.clone(),
            src: tmp,
            dst: dst
        }));
    }

    fn cast(&mut self, node: &Ast, expr: &AstRef) -> TacRef {
        let scope = scope_of(expr);
        let src = self.expr(expr.clone());
        let src_ty = expr.borrow().ty.clone();

        if is_match(&node.ty, &src_ty) {
            return src;
        }

        let ty = node.ty.borrow();
        let pos = make_space(scope.clone(), ty.size, ty.alignment);
        let dst = new_node!(Var(node.ty.clone(), pos));

        let src_is_double = is_double_type(&src_ty);
        let dst_is_double = is_double_type(&node.ty);
        let src_is_int = is_int_type(&src_ty);
        let dst_is_int = is_int_type(&node.ty);

        if src_is_double && dst_is_int {
            if is_signed(&node.ty) {
                self.emit(new_node!(DoubleToInt {
                    ty: node.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
            } else {
                if ty.size == 8 {
                    self.cast_double_to_ulong(node, src, dst.clone(), &scope);
                } else {
                    self.cast_double_to_uint(node, src, dst.clone(), &scope);
                }
            }
        } else if src_is_int && dst_is_double {
            if is_signed(&src_ty) {
                self.emit(new_node!(IntToDouble {
                    ty: node.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
            } else {
                if src_ty.borrow().size == 8 {
                    self.cast_ulong_to_double(node, src, dst.clone(), &scope);
                } else {
                    let tmp = self.tmp_var(src_ty.clone(), &scope);
                    self.emit(new_node!(ZeroExt {
                        ty: src_ty.clone(),
                        src: src,
                        dst: tmp.clone()
                    }));
                    self.emit(new_node!(IntToDouble {
                        ty: node.ty.clone(),
                        src: tmp,
                        dst: dst.clone()
                    }));
                }
            }
        } else if ty.size == src_ty.borrow().size {
            self.emit(new_node!(Copy {
                ty: node.ty.clone(),
                src: src,
                dst: dst.clone()
            }));
        } else if ty.size < src_ty.borrow().size {
            self.emit(new_node!(Truncate {
                ty: node.ty.clone(),
                src: src,
                dst: dst.clone()
            }));
        } else if is_signed(&src_ty) {
            self.emit(new_node!(SignExt {
                ty: node.ty.clone(),
                src: src,
                dst: dst.clone()
            }));
        } else {
            self.emit(new_node!(ZeroExt {
                ty: node.ty.clone(),
                src: src,
                dst: dst.clone()
            }));
        }

        dst
    }

    fn complement(&mut self, node: &Ast, subexpr: &AstRef) -> TacRef {
        let (src, dst) = self.unop(&node.scope, &node.ty, &subexpr);

        self.emit(new_node!(Inv {
            ty: node.ty.clone(),
            src: src,
            dst: dst.clone()
        }));

        dst
    }

    fn negate(&mut self, node: &Ast, subexpr: &AstRef) -> TacRef {
        if is_double_type(&node.ty) {
            let (src, dst) = self.unop(&node.scope, &node.ty, &subexpr);
            let minus_zero = self.const_double(-0.0f64);
            self.emit(new_node!(Xor {
                ty: node.ty.clone(),
                lhs: src.clone(),
                rhs: minus_zero,
                dst: dst.clone()
            }));

            dst
        } else {
            let (src, dst) = self.unop(&node.scope, &node.ty, &subexpr);

            self.emit(new_node!(Neg {
                ty: node.ty.clone(),
                src: src,
                dst: dst.clone()
            }));

            dst
        }
    }

    fn not(&mut self, node: &Ast, subexpr: &AstRef) -> TacRef {
        let (src, dst) = self.unop(&node.scope, &node.ty, &subexpr);

        self.emit(new_node!(Not {
            ty: node.ty.clone(),
            src: src,
            dst: dst.clone()
        }));

        dst
    }

    fn expr(&mut self, expr: AstRef) -> TacRef {
        let node = expr.borrow();
        match &node.kind {
            AstKind::ConstInt(val) => new_node!(Integer {
                ty: type_of(&expr),
                value: *val as u64
            }),
            AstKind::ConstLong(val) => new_node!(Integer {
                ty: type_of(&expr),
                value: *val as u64
            }),
            AstKind::ConstUnsignedInt(val) => new_node!(Integer {
                ty: type_of(&expr),
                value: *val as u64
            }),
            AstKind::ConstUnsignedLong(val) => {
                new_node!(Integer {
                    ty: type_of(&expr),
                    value: *val as u64
                })
            }
            AstKind::ConstDouble(val) => self.const_double(*val),
            AstKind::Complement { expr: subexpr } => {
                self.complement(&node, subexpr)
            }
            AstKind::Negate { expr: subexpr } => self.negate(&node, subexpr),
            AstKind::Not { expr: subexpr } => self.not(&node, subexpr),
            AstKind::PostIncr { expr: subexpr } => self.post_incr_or_decr(
                &node.scope,
                &node.ty,
                subexpr,
                true, /* incr */
            ),
            AstKind::PostDecr { expr: subexpr } => self.post_incr_or_decr(
                &node.scope,
                &node.ty,
                subexpr,
                false, /* decr */
            ),
            AstKind::Multiply { left, right } => {
                self.multiply(&node, left, right)
            }
            AstKind::Divide { left, right } => self.divide(&node, left, right),
            AstKind::Modulo { left, right } => self.modulo(&node, left, right),
            AstKind::Add { left, right } => self.add(&node, left, right),
            AstKind::Subtract { left, right } => {
                self.subtract(&node, left, right)
            }
            AstKind::LeftShift { left, right } => {
                self.shift_left(&node, left, right)
            }
            AstKind::RightShift { left, right } => {
                self.shift_right(&node, left, right)
            }
            AstKind::And { left, right } => self.and(&node, left, right),
            AstKind::Or { left, right } => self.or(&node, left, right),
            AstKind::Xor { left, right } => self.xor(&node, left, right),
            AstKind::Equal { left, right } => self.equal(&node, left, right),
            AstKind::NotEq { left, right } => self.not_eq(&node, left, right),
            AstKind::Less { left, right } => self.less(&node, left, right),
            AstKind::LessOrEq { left, right } => {
                self.less_or_eq(&node, left, right)
            }
            AstKind::Greater { left, right } => {
                self.greater(&node, left, right)
            }
            AstKind::GreaterOrEq { left, right } => {
                self.greater_or_eq(&node, left, right)
            }
            AstKind::LogicAnd { left, right } => {
                self.logical_and(&node.scope, &node.ty, left, right)
            }
            AstKind::LogicOr { left, right } => {
                self.logical_or(&node.scope, &node.ty, left, right)
            }
            AstKind::Assign { left, right } => {
                self.assign(&node.ty, left, right)
            }
            AstKind::Ternary {
                left,
                middle,
                right,
            } => self.ternary(&node.scope, &node.ty, left, middle, right),
            AstKind::Identifier { name, .. } => {
                self.identifier(&node, &expr, name)
            }
            AstKind::Call { expr, args } => self.call(&node, expr, args),

            AstKind::Cast { type_spec: _, expr } => self.cast(&node, expr),

            AstKind::Initializer(expr) => self.expr(expr.clone()),

            _ => {
                println!("Unexpected node: {:#?}", node);
                unreachable!()
            }
        }
    }

    fn function(
        &mut self,
        node: &Ast,
        name: &String,
        sym: &Option<SymWeakRef>,
        params: &[AstRef],
        body: &Option<AstRef>,
    ) {
        let mut irgen = TacGenerator::from(self);

        if body.is_some() {
            let mut tac_params: Vec<TacRef> = vec![];

            for param in params {
                if let AstKind::Parameter {
                    name,
                    sym: _,
                    idx: _,
                    type_spec: _,
                } = &param.borrow().kind
                {
                    if name.is_some() {
                        // otherwise 'void'
                        let sym = resolve(param).unwrap();
                        let ty = param.as_ref().borrow().ty.clone();
                        let p = new_node!(Var(ty, sym.borrow().pos));
                        tac_params.push(p);
                    }
                }
            }

            if let AstKind::Block { body: b } =
                &body.as_ref().unwrap().borrow().kind
            {
                for stmt in b {
                    irgen.stmt_or_decl(stmt.clone());
                }
            }

            irgen.emit(new_node!(Return(
                int_type(true),
                new_node!(Integer {
                    ty: int_type(true),
                    value: 0
                })
            )));

            self.join(&irgen);

            let global = if let Some(s) = sym.as_ref().and_then(|w| w.upgrade())
            {
                !matches!(s.borrow().storage_class, Some(StorageClass::Static),)
            } else {
                unreachable!()
            };

            let func = new_node!(Function {
                name: name.clone(),
                global: global,
                params: tac_params,
                code: irgen.out(),
                depth: node.scope.borrow().size,
            });

            self.emit(func);
        }
    }

    fn block(&mut self, body: &[AstRef]) {
        for stmt in body {
            self.stmt_or_decl(stmt.clone());
        }
    }

    fn if_stmt(
        &mut self,
        node: &Ast,
        cond: &AstRef,
        then: &AstRef,
        otherwise: &Option<AstRef>,
    ) {
        let else_label = if otherwise.is_some() {
            Some(self.label())
        } else {
            None
        };
        let end_label = self.label();
        let c = self.expr(cond.clone());
        self.emit(new_node!(JumpOnZero {
            ty: node.ty.clone(),
            expr: c,
            label: if else_label.is_some() {
                else_label.as_ref().unwrap().clone()
            } else {
                end_label.clone()
            }
        }));
        self.stmt_or_decl(then.clone());
        if otherwise.is_some() {
            self.emit(new_node!(Jump(end_label.clone())));
            self.emit(else_label.unwrap());
            self.stmt_or_decl(otherwise.as_ref().unwrap().clone());
        }
        self.emit(end_label);
    }

    fn switch_stmt(
        &mut self,
        node: &Ast,
        cond: &AstRef,
        body: &AstRef,
        cases: &Vec<AstRef>,
    ) {
        let src = self.expr(cond.clone());
        let dst = self.tmp_var(cond.borrow().ty.clone(), &node.scope);
        self.emit(new_node!(Copy {
            ty: node.ty.clone(),
            src: src,
            dst: dst.clone()
        }));
        for c in cases.iter() {
            if let AstKind::Case {
                expr,
                stmt: _,
                idx: _,
            } = &c.borrow().kind
            {
                let e = self.expr(expr.clone());
                let tmp = self.tmp_var(expr.borrow().ty.clone(), &node.scope);
                self.emit(new_node!(Equal {
                    ty: node.ty.clone(),
                    lhs: dst.clone(),
                    rhs: e,
                    dst: tmp.clone()
                }));
                let case_label =
                    self.case_label(&c.borrow().scope, c.borrow().id);
                self.emit(new_node!(JumpOnNotZero {
                    ty: node.ty.clone(),
                    expr: tmp,
                    label: case_label
                }));
            }
        }
        let mut has_default = false;
        for c in cases.iter() {
            if let AstKind::Default { .. } = &c.borrow().kind {
                let case_label =
                    self.case_label(&c.borrow().scope, c.borrow().id);
                self.emit(new_node!(Jump(case_label)));
                has_default = true;
            }
        }
        let break_label = self.break_label(&body.borrow().scope);
        if !has_default {
            self.emit(new_node!(Jump(break_label.clone())));
        }
        if !cases.is_empty() {
            self.stmt_or_decl(body.clone());
        }
        self.emit(break_label);
    }

    fn do_while_stmt(&mut self, node: &Ast, cond: &AstRef, body: &AstRef) {
        let start_label = self.label();
        self.emit(start_label.clone());
        self.stmt_or_decl(body.clone());
        let continue_label = self.continue_label(&body.borrow().scope);
        self.emit(continue_label);
        let c = self.expr(cond.clone());
        self.emit(new_node!(JumpOnNotZero {
            ty: node.ty.clone(),
            expr: c,
            label: start_label
        }));
        let break_label = self.break_label(&body.borrow().scope);
        self.emit(break_label);
    }

    fn while_stmt(&mut self, node: &Ast, cond: &AstRef, body: &AstRef) {
        let continue_label = self.continue_label(&body.borrow().scope);
        self.emit(continue_label.clone());
        let c = self.expr(cond.clone());
        let break_label = self.break_label(&body.borrow().scope);
        self.emit(new_node!(JumpOnZero {
            ty: node.ty.clone(),
            expr: c,
            label: break_label.clone()
        }));
        self.stmt_or_decl(body.clone());
        self.emit(new_node!(Jump(continue_label)));
        self.emit(break_label);
    }

    fn for_stmt(
        &mut self,
        node: &Ast,
        init: &Option<AstRef>,
        cond: &Option<AstRef>,
        post: &Option<AstRef>,
        body: &AstRef,
    ) {
        if init.is_some() {
            self.stmt_or_decl(init.as_ref().unwrap().clone());
        }
        let start_label = self.label();
        self.emit(start_label.clone());
        let break_label = self.break_label(&body.borrow().scope);
        if cond.is_some() {
            let c = self.expr(cond.as_ref().unwrap().clone());
            self.emit(new_node!(JumpOnZero {
                ty: node.ty.clone(),
                expr: c,
                label: break_label.clone()
            }));
        }
        self.stmt_or_decl(body.clone());
        let continue_label = self.continue_label(&body.borrow().scope);
        self.emit(continue_label.clone());
        if post.is_some() {
            self.stmt_or_decl(post.as_ref().unwrap().clone());
        }
        self.emit(new_node!(Jump(start_label.clone())));
        self.emit(break_label);
    }

    fn return_stmt(&mut self, ty: &TypeRef, expr: &AstRef) {
        let e = self.expr(expr.clone());
        self.emit(new_node!(Return(ty.clone(), e)));
    }

    fn goto_stmt(&mut self, label: &str) {
        let label = self.named_label(label);
        self.emit(new_node!(Jump(label.clone())));
    }

    fn labelled_stmt(&mut self, name: &str, stmt: &AstRef) {
        let label = self.named_label(name);
        self.emit(label);
        self.stmt_or_decl(stmt.clone());
    }

    fn case_stmt(&mut self, node: &Ast, stmt: &AstRef) {
        let label = self.case_label(&node.scope, node.id);
        self.emit(label);
        self.stmt_or_decl(stmt.clone());
    }

    fn default_stmt(&mut self, node: &Ast, stmt: &AstRef) {
        let label = self.case_label(&node.scope, node.id);
        self.emit(label);
        self.stmt_or_decl(stmt.clone());
    }

    fn break_stmt(&mut self, node: &Ast) {
        let label = self.break_label(&node.scope);
        self.emit(new_node!(Jump(label.clone())));
    }

    fn continue_stmt(&mut self, node: &Ast) {
        let label = self.continue_label(&node.scope);
        self.emit(new_node!(Jump(label.clone())));
    }

    fn variable(
        &mut self,
        node: &Ast,
        ast: &AstRef,
        sym: &Option<SymWeakRef>,
        init: &Option<AstRef>,
    ) {
        if let Some(s) = sym.as_ref().and_then(|w| w.upgrade()) {
            if !has_static_storage_duration(s.clone())
                && has_parent(&node.scope)
                && init.is_some()
            {
                let sym = resolve(&ast).unwrap();
                let e = self.expr(init.as_ref().unwrap().clone());
                let v = new_node!(Var(node.ty.clone(), sym.borrow().pos));
                self.emit(new_node!(Copy {
                    ty: node.ty.clone(),
                    src: e,
                    dst: v
                }));
            }
        }
    }

    fn stmt_or_decl(&mut self, ast: AstRef) {
        let node = ast.borrow();

        match &node.kind {
            AstKind::Function {
                name,
                sym,
                params,
                block,
                type_spec: _,
                scope: _,
            } => self.function(&node, name, sym, &params, block),
            AstKind::Block { body } => self.block(body),
            AstKind::If {
                cond,
                then,
                otherwise,
            } => self.if_stmt(&node, cond, then, otherwise),
            AstKind::Switch { cond, body, cases } => {
                self.switch_stmt(&node, cond, body, cases)
            }
            AstKind::DoWhile { cond, body } => {
                self.do_while_stmt(&node, cond, body)
            }
            AstKind::While { cond, body } => self.while_stmt(&node, cond, body),
            AstKind::For {
                init,
                cond,
                post,
                body,
            } => self.for_stmt(&node, init, cond, post, body),
            AstKind::Return { expr, .. } => self.return_stmt(&node.ty, expr),
            AstKind::GoTo { label } => self.goto_stmt(label),
            AstKind::Label { name, stmt } => self.labelled_stmt(name, stmt),
            AstKind::Case { stmt, .. } => self.case_stmt(&node, stmt),
            AstKind::Default { stmt, .. } => self.default_stmt(&node, stmt),
            AstKind::Break { .. } => self.break_stmt(&node),
            AstKind::Continue { .. } => self.continue_stmt(&node),
            AstKind::ExprStmt { expr } => {
                _ = self.expr(expr.clone());
            }
            AstKind::Variable {
                name: _,
                sym,
                type_spec: _,
                init,
            } => self.variable(&node, &ast, sym, init),
            AstKind::EmptyStmt => {}
            _ => {
                unreachable!();
            }
        }
    }

    fn emit(&mut self, ir: TacRef) {
        self.tac_code.push(ir.clone());
    }

    fn out(&mut self) -> Vec<TacRef> {
        std::mem::take(&mut self.tac_code)
    }

    fn static_initializer(&mut self, ty: &TypeRef, init: &AstRef) -> TacRef {
        let value: u64;

        match &init.borrow().kind {
            AstKind::StaticInitializer(expr) => {
                return self.static_initializer(ty, expr);
            }
            AstKind::Cast { expr, .. } => {
                return self.static_initializer(ty, expr);
            }
            AstKind::ConstInt(v) => {
                if is_double_type(ty) {
                    value = ((*v as u64) as f64).to_bits() as u64;
                } else {
                    value = *v as u64;
                }
            }
            AstKind::ConstLong(v) => {
                if is_double_type(ty) {
                    value = ((*v as u64) as f64).to_bits() as u64;
                } else {
                    value = *v as u64;
                }
            }
            AstKind::ConstUnsignedInt(v) => {
                if is_double_type(ty) {
                    value = ((*v as u64) as f64).to_bits() as u64;
                } else {
                    value = *v as u64;
                }
            }
            AstKind::ConstUnsignedLong(v) => {
                if is_double_type(ty) {
                    value = ((*v as u64) as f64).to_bits() as u64;
                } else {
                    value = *v as u64;
                }
            }
            AstKind::ConstDouble(v) => {
                if is_double_type(ty) {
                    value = v.to_bits() as u64;
                } else if is_signed(ty) {
                    if size_of(ty) == 8 {
                        value = *v as i64 as u64;
                    } else {
                        value = *v as i32 as u64;
                    }
                } else if size_of(ty) == 8 {
                    value = *v as u64;
                } else {
                    value = *v as u32 as u64;
                }
            }
            _ => {
                println!("Cannot handle: {:#?}", init);
                unreachable!()
            }
        }

        new_node!(StaticInitializer(
            ty.clone(),
            if is_double_type(ty) {
                new_node!(Double(f64::from_bits(value)))
            } else {
                new_node!(Integer {
                    ty: ty.clone(),
                    value: value
                })
            }
        ))
    }

    fn symbol(&mut self, sym: SymRef, name: &str) -> Option<TacRef> {
        let s = sym.borrow();

        if let SymKind::Variable = &s.kind {
            if let Some(Definition::Concrete) = s.definition {
                if let Some(node) = sym_as_node(sym.clone()) {
                    let ty = node.borrow().ty.clone();
                    if let AstKind::Variable { init, .. } = &node.borrow().kind
                    {
                        let initializer = self
                            .static_initializer(&ty, &init.as_ref().unwrap());
                        let global = !matches!(
                            s.storage_class,
                            Some(StorageClass::Static)
                        );

                        return Some(new_node!(StaticVar(
                            ty.clone(),
                            name.to_string(),
                            global,
                            initializer
                        )));
                    }
                }
            } else if let Some(node) = sym_as_node(sym.clone()) {
                let ty = node.borrow().ty.clone();

                if let AstKind::Variable { .. } = &node.borrow().kind {
                    let global =
                        !matches!(s.storage_class, Some(StorageClass::Static));

                    return Some(new_node!(StaticVar(
                        ty.clone(),
                        name.to_string(),
                        global,
                        new_node!(StaticInitializer(
                            ty.clone(),
                            new_node!(Integer {
                                ty: ty.clone(),
                                value: 0
                            })
                        )),
                    )));
                }
            }
        }

        None
    }

    fn rodata(&self, ty: TypeRef, name: &str, bits: u64) -> TacRef {
        new_node!(RoData(ty, name.to_string(), bits))
    }

    fn symbols(&mut self, scope: ScopeRef) -> Vec<TacRef> {
        assert!(!has_parent(&scope));
        let externs = get_externs(&scope);
        let mut tac_vec = vec![];

        for def in externs.borrow().defs() {
            let name = def.borrow().name.clone();
            if let Some(tac) = self.symbol(def.clone(), &name) {
                tac_vec.push(tac);
            }
        }

        for def in scope.borrow().symbols.defs() {
            let name = def.borrow().name.clone();
            if externs.borrow().def(&name).is_none() {
                if let Some(tac) = self.symbol(def.clone(), &name) {
                    tac_vec.push(tac);
                }
            }
        }

        for (bits, idx) in &self.fp_consts {
            let name = format!(".L{}", idx);
            tac_vec.push(self.rodata(double_type(), &name, bits.0));
        }

        tac_vec
    }
}
