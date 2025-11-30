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

use crate::air::AirGenerator;
use crate::ast::*;
use crate::lexing::Token;
use crate::symtab::*;
use crate::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TacId(pub usize);

#[derive(Debug, Clone)]
pub enum Op {
    Call {
        ty: TypeRef,
        func: TacId,
        args: Vec<TacId>,
        dst: TacId,
    },
    DoubleToInt {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    DoubleToUlong {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    IntToDouble {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    SignExt {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    ZeroExt {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Truncate {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Inv {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Neg {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Not {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Mul {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Div {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Mod {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Add {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    AddPtr {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        scale: usize,
        dst: TacId,
    },
    Sub {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    LeftShift {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    RightShift {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    And {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Or {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Xor {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Equal {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    NotEq {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Less {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    LessOrEq {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Greater {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    GreaterOrEq {
        ty: TypeRef,
        lhs: TacId,
        rhs: TacId,
        dst: TacId,
    },
    Copy {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    CopyToOffset {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
        off: usize,
    },
    Jump(TacId),
    JumpOnZero {
        ty: TypeRef,
        expr: TacId,
        label: TacId,
    },
    JumpOnNotZero {
        ty: TypeRef,
        expr: TacId,
        label: TacId,
    },
    Label(usize),
    Return(TypeRef, TacId),
    GetAddr {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Load {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
    Store {
        ty: TypeRef,
        src: TacId,
        dst: TacId,
    },
}

#[derive(Debug, Clone)]
pub enum Operand {
    Integer { ty: TypeRef, value: u64 },
    Double(f64),
    Var(TypeRef, String),
    PlainOperand(TacId),
    DereferencedPtr(TacId),
    FunctionRef(String, bool),
    StaticVarRef(TypeRef, String),
}

#[derive(Debug, Clone)]
pub enum Object {
    StaticInit(TypeRef, TacId),
    StaticInitList(Vec<TacId>, usize),
    Function {
        name: String,
        global: bool,
        params: Vec<TacId>,
        code: Vec<TacId>,
    },
    RoData(TypeRef, String, u64),
    StaticVar(TypeRef, String, bool, TacId),
}

#[derive(Debug, Clone)]
pub enum Tac {
    Op(Op),
    Operand(Operand),
    Object(Object),
}

#[derive(Debug)]
pub struct TacArena {
    pub arena: Vec<Tac>,
    pub top_level: Vec<TacId>,
}

impl std::ops::Index<TacId> for TacArena {
    type Output = Tac;

    fn index(&self, id: TacId) -> &Tac {
        &self.arena[id.0]
    }
}

#[derive(Debug)]
pub struct AirStage {
    pub tac: TacArena,
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
    tmp_idx: usize,
    label_map: HashMap<String, usize>,
    fp_consts: HashMap<Fp64Bits, usize>,
    pub arena: Vec<Tac>,
    tac_code: Vec<TacId>,
    lhs_as_lval: Option<TacId>,
    symtab: Option<SymTab>,
}

impl AirGenerator for TacGenerator {
    type Air = AirStage;

    fn new() -> Self {
        Self {
            label_idx: 0,
            tmp_idx: 0,
            label_map: HashMap::new(),
            fp_consts: HashMap::new(),
            arena: Vec::new(),
            tac_code: Vec::new(),
            lhs_as_lval: None,
            symtab: None,
        }
    }

    fn lower(&mut self, stage: AstStage) -> Self::Air {
        self.symtab = Some(stage.symtab);
        let mut scope: Option<ScopeId> = None;
        self.tac_code.clear();
        for &node_id in &stage.root {
            scope = Some(stage.arena[node_id].scope);
            self.stmt_or_decl(&stage.arena, node_id);
        }
        if let Some(s) = scope {
            let symtab = self.symtab.take().unwrap();
            let top_scope = symtab.upto_top(s);
            let tac_vec = self.symbols(&stage.arena, &symtab, top_scope);
            self.tac_code.extend(tac_vec);
        }
        AirStage {
            tac: TacArena {
                arena: std::mem::take(&mut self.arena),
                top_level: std::mem::take(&mut self.tac_code),
            },
        }
    }
}

impl TacGenerator {
    fn alloc(&mut self, tac: Tac) -> TacId {
        let id = TacId(self.arena.len());
        self.arena.push(tac);
        id
    }

    fn scopes(&self) -> &SymTab {
        self.symtab.as_ref().unwrap()
    }

    fn incr_label_idx(&mut self) -> usize {
        let idx = self.label_idx;
        self.label_idx += 1;
        idx
    }

    fn const_double(&mut self, value: f64) -> TacId {
        let key: Fp64Bits = value.into();
        let idx: usize;

        if !self.fp_consts.contains_key(&key) {
            idx = self.incr_label_idx();
            self.fp_consts.insert(key, idx);
        } else {
            idx = *self.fp_consts.get(&key).unwrap();
        }

        let name = format!(".L{idx}");
        self.alloc(Tac::Operand(Operand::StaticVarRef(
            double_type(),
            name.clone(),
        )))
    }

    fn named_label(&mut self, name: &str) -> TacId {
        if !self.label_map.contains_key(name) {
            let idx = self.incr_label_idx();
            self.label_map.insert(name.to_string(), idx);
        }
        self.alloc(Tac::Op(Op::Label(*self.label_map.get(name).unwrap())))
    }

    fn label(&mut self) -> TacId {
        let idx = self.incr_label_idx();
        self.alloc(Tac::Op(Op::Label(idx)))
    }

    fn continue_label(&mut self, scope: ScopeId) -> TacId {
        let loop_scope = self.scopes().upto(scope, ScopeKind::Loop).unwrap();
        let label = format!(".continue.{}", loop_scope.0);
        self.named_label(&label)
    }

    fn break_label(&mut self, scope: ScopeId) -> TacId {
        let loop_or_switch = self
            .scopes()
            .upto_any(scope, &[ScopeKind::Loop, ScopeKind::Switch])
            .unwrap();
        let label = format!(".break.{}", loop_or_switch.0);
        self.named_label(&label)
    }

    fn case_label(&mut self, scope: ScopeId, case_idx: usize) -> TacId {
        let switch_scope =
            self.scopes().upto(scope, ScopeKind::Switch).unwrap();
        let label = format!(".case.{}.{}", case_idx, switch_scope.0);
        self.named_label(&label)
    }

    fn tmp_var(&mut self, ty: TypeRef) -> TacId {
        let name = format!(".tmp.{}", self.tmp_idx);
        self.tmp_idx += 1;
        self.alloc(Tac::Operand(Operand::Var(ty.clone(), name)))
    }

    fn pseudo_name(symtab: &SymTab, sym: SymId) -> String {
        symtab[sym].name.clone()
    }

    fn binop(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> (TacId, TacId, TacId) {
        let lhs_ty = type_of(arena, lhs);
        let mut v0: TacId;

        let lhs_as_lval = if let AstKind::Cast { expr, .. } = &arena[lhs].kind {
            let v1 = self.expr(arena, *expr);
            let lval_ty = type_of(arena, *expr);
            v0 = self.convert(v1, &lval_ty);
            v0 = self.cast(&lval_ty, &lhs_ty, &v0);
            v0 = self.alloc(Tac::Operand(Operand::PlainOperand(v0)));
            Some(v1)
        } else {
            v0 = self.expr(arena, lhs);
            Some(v0)
        };

        v0 = self.convert(v0, &lhs_ty);
        let v1 = self.expr_and_convert(arena, rhs);
        let v2 = self.tmp_var(node.ty.clone());

        self.lhs_as_lval = lhs_as_lval;

        (v0, v1, v2)
    }

    fn unop(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
    ) -> (TacId, TacId) {
        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.tmp_var(ty.clone());

        (v0, v1)
    }

    fn pre_incr_or_decr(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        incr: bool,
    ) -> TacId {
        let v0 = self.expr(arena, lhs);
        let v1 = self.convert(v0, ty);

        let delta = if is_double_type(ty) {
            self.const_double(1.0)
        } else if is_pointer_type(ty) {
            let base_ty = base_type_v2(ty);
            self.alloc(Tac::Operand(Operand::Integer {
                ty: long_type(false),
                value: size_of(&base_ty) as u64,
            }))
        } else {
            self.alloc(Tac::Operand(Operand::Integer {
                ty: ty.clone(),
                value: 1,
            }))
        };

        let v2 = self.tmp_var(ty.clone());
        if incr {
            let v3 = self.alloc(Tac::Op(Op::Add {
                ty: ty.clone(),
                lhs: v1,
                rhs: delta,
                dst: v2,
            }));
            self.emit(v3);
        } else {
            let v3 = self.alloc(Tac::Op(Op::Sub {
                ty: ty.clone(),
                lhs: v1,
                rhs: delta,
                dst: v2,
            }));
            self.emit(v3);
        }

        let (is_plain, inner) = match &self.arena[v0.0] {
            Tac::Operand(Operand::PlainOperand(obj)) => (true, *obj),
            Tac::Operand(Operand::DereferencedPtr(ptr)) => (false, *ptr),
            _ => unreachable!(),
        };

        if is_plain {
            let v3 = self.alloc(Tac::Op(Op::Copy {
                ty: ty.clone(),
                src: v2,
                dst: inner,
            }));
            self.emit(v3);
        } else {
            let v3 = self.alloc(Tac::Op(Op::Store {
                ty: ty.clone(),
                src: v2,
                dst: inner,
            }));
            self.emit(v3);
        }

        v2
    }

    fn post_incr_or_decr(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        incr: bool,
    ) -> TacId {
        let v0 = self.expr(arena, lhs);
        let v1 = self.expr_and_convert(arena, lhs);

        let v2 = self.tmp_var(ty.clone());
        let v3 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v1,
            dst: v2,
        }));
        self.emit(v3);

        let delta = if is_double_type(ty) {
            self.const_double(1.0)
        } else if is_pointer_type(ty) {
            let base_ty = base_type_v2(ty);
            self.alloc(Tac::Operand(Operand::Integer {
                ty: long_type(false),
                value: size_of(&base_ty) as u64,
            }))
        } else {
            self.alloc(Tac::Operand(Operand::Integer {
                ty: ty.clone(),
                value: 1,
            }))
        };

        let v4 = self.tmp_var(ty.clone());

        if incr {
            let v5 = self.alloc(Tac::Op(Op::Add {
                ty: ty.clone(),
                lhs: delta,
                rhs: v1,
                dst: v4,
            }));
            self.emit(v5);
        } else {
            let v5 = self.alloc(Tac::Op(Op::Sub {
                ty: ty.clone(),
                lhs: v1,
                rhs: delta,
                dst: v4,
            }));
            self.emit(v5);
        }

        let (is_plain, inner) = match &self.arena[v0.0] {
            Tac::Operand(Operand::PlainOperand(obj)) => (true, *obj),
            Tac::Operand(Operand::DereferencedPtr(ptr)) => (false, *ptr),
            _ => unreachable!(),
        };

        if is_plain {
            let v5 = self.alloc(Tac::Op(Op::Copy {
                ty: ty.clone(),
                src: v4,
                dst: inner,
            }));
            self.emit(v5);
        } else {
            let v5 = self.alloc(Tac::Op(Op::Store {
                ty: ty.clone(),
                src: v4,
                dst: inner,
            }));
            self.emit(v5);
        }

        v2
    }

    fn logical_and(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let false_label = self.label();
        let end_label = self.label();
        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.alloc(Tac::Op(Op::JumpOnZero {
            ty: ty.clone(),
            expr: v0,
            label: false_label,
        }));
        self.emit(v1);
        let v2 = self.expr_and_convert(arena, rhs);
        let v3 = self.alloc(Tac::Op(Op::JumpOnZero {
            ty: ty.clone(),
            expr: v2,
            label: false_label,
        }));
        self.emit(v3);
        let v4 = self.tmp_var(ty.clone());
        let v5 = self.alloc(Tac::Operand(Operand::Integer {
            ty: ty.clone(),
            value: 1,
        }));
        let v6 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v5,
            dst: v4,
        }));
        self.emit(v6);
        let v7 = self.alloc(Tac::Op(Op::Jump(end_label)));
        self.emit(v7);
        self.emit(false_label);
        let v8 = self.alloc(Tac::Operand(Operand::Integer {
            ty: ty.clone(),
            value: 0,
        }));
        let v9 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v8,
            dst: v4,
        }));
        self.emit(v9);
        self.emit(end_label);
        v4
    }

    fn logical_or(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let true_label = self.label();
        let end_label = self.label();
        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.alloc(Tac::Op(Op::JumpOnNotZero {
            ty: ty.clone(),
            expr: v0,
            label: true_label,
        }));
        self.emit(v1);
        let v2 = self.expr_and_convert(arena, rhs);
        let v3 = self.alloc(Tac::Op(Op::JumpOnNotZero {
            ty: ty.clone(),
            expr: v2,
            label: true_label,
        }));
        self.emit(v3);
        let v4 = self.tmp_var(ty.clone());
        let v5 = self.alloc(Tac::Operand(Operand::Integer {
            ty: ty.clone(),
            value: 0,
        }));
        let v6 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v5,
            dst: v4,
        }));
        self.emit(v6);
        let v7 = self.alloc(Tac::Op(Op::Jump(end_label)));
        self.emit(v7);
        self.emit(true_label);
        let v8 = self.alloc(Tac::Operand(Operand::Integer {
            ty: ty.clone(),
            value: 1,
        }));
        let v9 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v8,
            dst: v4,
        }));
        self.emit(v9);
        self.emit(end_label);
        v4
    }

    fn assign(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let v0 = self.expr(arena, lhs);
        let v1 = self.expr_and_convert(arena, rhs);

        let (is_plain, inner) = match &self.arena[v0.0] {
            Tac::Operand(Operand::PlainOperand(obj)) => (true, *obj),
            Tac::Operand(Operand::DereferencedPtr(ptr)) => (false, *ptr),
            _ => unreachable!(),
        };

        if is_plain {
            let v2 = self.alloc(Tac::Op(Op::Copy {
                ty: ty.clone(),
                src: v1,
                dst: inner,
            }));
            self.emit(v2);

            v0
        } else {
            let v2 = self.alloc(Tac::Op(Op::Store {
                ty: ty.clone(),
                src: v1,
                dst: inner,
            }));
            self.emit(v2);

            self.alloc(Tac::Operand(Operand::PlainOperand(v1)))
        }
    }

    fn compound_assign(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        rhs: AstId,
    ) -> TacId {
        let v0 = self.expr_and_convert(arena, rhs);
        let v1 = self.lhs_as_lval.unwrap();

        let (is_plain, inner) = match &self.arena[v1.0] {
            Tac::Operand(Operand::PlainOperand(obj)) => (true, *obj),
            Tac::Operand(Operand::DereferencedPtr(ptr)) => (false, *ptr),
            _ => unreachable!(),
        };

        if is_plain {
            let v2 = self.alloc(Tac::Op(Op::Copy {
                ty: ty.clone(),
                src: v0,
                dst: inner,
            }));
            self.emit(v2);

            v1
        } else {
            let v2 = self.alloc(Tac::Op(Op::Store {
                ty: ty.clone(),
                src: v0,
                dst: inner,
            }));
            self.emit(v2);

            self.alloc(Tac::Operand(Operand::PlainOperand(v0)))
        }
    }

    fn ternary(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        lhs: AstId,
        middle: AstId,
        rhs: AstId,
    ) -> TacId {
        let e2_label = self.label();
        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.alloc(Tac::Op(Op::JumpOnZero {
            ty: arena[lhs].ty.clone(),
            expr: v0,
            label: e2_label,
        }));
        self.emit(v1);
        let v2 = self.expr_and_convert(arena, middle);
        let v3 = self.tmp_var(ty.clone());
        let v4 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v2,
            dst: v3,
        }));
        self.emit(v4);
        let end_label = self.label();
        let v5 = self.alloc(Tac::Op(Op::Jump(end_label)));
        self.emit(v5);
        self.emit(e2_label);
        let v6 = self.expr_and_convert(arena, rhs);
        let v7 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v6,
            dst: v3,
        }));
        self.emit(v7);
        self.emit(end_label);
        v3
    }

    fn multiply(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Mul {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn divide(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Div {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn modulo(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Mod {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn add(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);
        let lhs_ty = type_of(arena, lhs);

        if is_pointer_type(&lhs_ty) {
            let base_ty = base_type_v2(&lhs_ty);
            let elem_size = size_of(&base_ty);

            let v3 = self.alloc(Tac::Op(Op::AddPtr {
                ty: node.ty.clone(),
                lhs: v0,
                rhs: v1,
                scale: elem_size,
                dst: v2,
            }));
            self.emit(v3);
        } else {
            let v3 = self.alloc(Tac::Op(Op::Add {
                ty: node.ty.clone(),
                lhs: v0,
                rhs: v1,
                dst: v2,
            }));
            self.emit(v3);
        }

        v2
    }

    fn subtract_ptrs(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let lhs_ty = type_of(arena, lhs);
        let base_ty = base_type_v2(&lhs_ty);

        let v3 = self.alloc(Tac::Operand(Operand::Integer {
            ty: long_type(true),
            value: size_of(&base_ty) as u64,
        }));

        let long_ty = long_type(true);
        let v4 = self.tmp_var(long_ty.clone());

        let v5 = self.alloc(Tac::Op(Op::Sub {
            ty: long_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v4,
        }));
        self.emit(v5);

        let v6 = self.alloc(Tac::Op(Op::Div {
            ty: long_ty.clone(),
            lhs: v4,
            rhs: v3,
            dst: v2,
        }));
        self.emit(v6);

        v2
    }

    fn subtract(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let lhs_ty = type_of(arena, lhs);
        let rhs_ty = type_of(arena, rhs);

        if is_pointer_type(&lhs_ty) && is_pointer_type(&rhs_ty) {
            return self.subtract_ptrs(arena, node, lhs, rhs);
        }

        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        if is_pointer_type(&lhs_ty) {
            let base_ty = base_type_v2(&lhs_ty);
            let elem_size = size_of(&base_ty);

            let byte_offset = if elem_size == 1 {
                v1
            } else {
                let v3 = self.alloc(Tac::Operand(Operand::Integer {
                    ty: long_type(false),
                    value: elem_size as u64,
                }));
                let v4 = self.tmp_var(long_type(false));
                let v5 = self.alloc(Tac::Op(Op::Mul {
                    ty: long_type(false),
                    lhs: v1,
                    rhs: v3,
                    dst: v4,
                }));
                self.emit(v5);
                v4
            };

            let v3 = self.tmp_var(long_type(false));
            let v4 = self.alloc(Tac::Op(Op::Neg {
                ty: long_type(false),
                src: byte_offset,
                dst: v3,
            }));
            self.emit(v4);

            let v5 = self.alloc(Tac::Op(Op::AddPtr {
                ty: node.ty.clone(),
                lhs: v0,
                rhs: v3,
                scale: 1,
                dst: v2,
            }));
            self.emit(v5);
        } else {
            let v3 = self.alloc(Tac::Op(Op::Sub {
                ty: node.ty.clone(),
                lhs: v0,
                rhs: v1,
                dst: v2,
            }));
            self.emit(v3);
        }

        v2
    }

    fn shift_left(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::LeftShift {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn shift_right(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::RightShift {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn and(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::And {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn or(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Or {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn xor(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Xor {
            ty: node.ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn equal(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let cmp_ty = type_of(arena, lhs);
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Equal {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn not_eq(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let cmp_ty = type_of(arena, lhs);
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::NotEq {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn less(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let cmp_ty = type_of(arena, lhs);
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Less {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn less_or_eq(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let cmp_ty = type_of(arena, lhs);
        let v3 = self.alloc(Tac::Op(Op::LessOrEq {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn greater(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let cmp_ty = type_of(arena, lhs);
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let v3 = self.alloc(Tac::Op(Op::Greater {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn greater_or_eq(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        rhs: AstId,
    ) -> TacId {
        let (v0, v1, v2) = self.binop(arena, node, lhs, rhs);

        let cmp_ty = type_of(arena, lhs);
        let v3 = self.alloc(Tac::Op(Op::GreaterOrEq {
            ty: cmp_ty.clone(),
            lhs: v0,
            rhs: v1,
            dst: v2,
        }));
        self.emit(v3);

        v2
    }

    fn identifier(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        expr_id: AstId,
        name: &Token,
    ) -> TacId {
        let symtab = self.scopes();
        let sym = resolve(symtab, arena, &expr_id).unwrap();
        let name_str = arena.token_str(name);
        let is_defined = symtab.has_def(node.scope, name_str);
        let sym_node = sym_as_node(symtab, sym).unwrap();
        let sym_type = arena[sym_node].ty.clone();

        match &arena[sym_node].kind {
            AstKind::Function { name, .. } => {
                self.alloc(Tac::Operand(Operand::FunctionRef(
                    arena.token_str(name.as_ref().unwrap()).to_string(),
                    is_defined,
                )))
            }
            _ => {
                if has_static_storage_duration(symtab, sym) {
                    if has_linkage(symtab, sym) {
                        self.alloc(Tac::Operand(Operand::StaticVarRef(
                            sym_type,
                            name_str.to_string(),
                        )))
                    } else {
                        let at_scope = symtab[sym].at_scope.0;
                        let mangled_name = format!("{}.{}", name_str, at_scope);
                        self.alloc(Tac::Operand(Operand::StaticVarRef(
                            sym_type,
                            mangled_name.clone(),
                        )))
                    }
                } else {
                    self.alloc(Tac::Operand(Operand::Var(
                        node.ty.clone(),
                        Self::pseudo_name(symtab, sym),
                    )))
                }
            }
        }
    }

    fn call(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
        args: &[AstId],
    ) -> TacId {
        let mut v0: Vec<TacId> = vec![];

        for &arg in args {
            v0.push(self.expr_and_convert(arena, arg));
        }

        let v1 = self.tmp_var(node.ty.clone());
        let v2 = self.expr_and_convert(arena, lhs);

        let v3 = self.alloc(Tac::Op(Op::Call {
            ty: node.ty.clone(),
            func: v2,
            args: v0,
            dst: v1,
        }));
        self.emit(v3);

        v1
    }

    fn cast_double_to_ulong(&mut self, ty: &TypeRef, lhs: TacId, dst: TacId) {
        let out_of_range = self.label();
        let end = self.label();
        let upper_bound = self.const_double((i64::MAX as u64 + 1) as f64);

        let v0 = self.tmp_var(ty.clone());
        let v1 = self.alloc(Tac::Op(Op::GreaterOrEq {
            ty: ty.clone(),
            lhs: lhs,
            rhs: upper_bound,
            dst: v0,
        }));
        self.emit(v1);

        let v2 = self.alloc(Tac::Op(Op::JumpOnNotZero {
            ty: ty.clone(),
            expr: v0,
            label: out_of_range,
        }));
        self.emit(v2);

        let v3 = self.alloc(Tac::Op(Op::DoubleToUlong {
            ty: ty.clone(),
            src: lhs,
            dst: dst,
        }));
        self.emit(v3);
        let v4 = self.alloc(Tac::Op(Op::Jump(end)));
        self.emit(v4);

        self.emit(out_of_range);
        let v5 = self.tmp_var(ty.clone());
        let v6 = self.alloc(Tac::Op(Op::Sub {
            ty: ty.clone(),
            lhs: lhs,
            rhs: upper_bound,
            dst: v5,
        }));
        self.emit(v6);
        let v7 = self.tmp_var(ty.clone());
        let v8 = self.alloc(Tac::Op(Op::DoubleToUlong {
            ty: ty.clone(),
            src: v5,
            dst: v7,
        }));
        self.emit(v8);
        let v9 = self.tmp_var(ty.clone());
        let v10 = self.alloc(Tac::Operand(Operand::Integer {
            ty: ty.clone(),
            value: 9223372036854775808,
        }));
        let v11 = self.alloc(Tac::Op(Op::Add {
            ty: ty.clone(),
            lhs: v7,
            rhs: v10,
            dst: v9,
        }));
        self.emit(v11);
        let v12 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v9,
            dst: dst,
        }));
        self.emit(v12);
        self.emit(end);
    }

    fn cast_ulong_to_double(&mut self, ty: &TypeRef, lhs: TacId, dst: TacId) {
        let out_of_range = self.label();
        let end = self.label();

        let v0 = self.alloc(Tac::Operand(Operand::Integer {
            ty: long_type(false),
            value: 0,
        }));
        let v1 = self.tmp_var(int_type(true));
        let v2 = self.alloc(Tac::Op(Op::Less {
            ty: long_type(true),
            lhs: lhs,
            rhs: v0,
            dst: v1,
        }));
        self.emit(v2);

        let v3 = self.alloc(Tac::Op(Op::JumpOnNotZero {
            ty: long_type(true),
            expr: v1,
            label: out_of_range,
        }));
        self.emit(v3);

        let v4 = self.alloc(Tac::Op(Op::IntToDouble {
            ty: ty.clone(),
            src: lhs,
            dst: dst,
        }));
        self.emit(v4);
        let v5 = self.alloc(Tac::Op(Op::Jump(end)));
        self.emit(v5);

        self.emit(out_of_range);

        let v6 = self.tmp_var(long_type(false));

        let v7 = self.alloc(Tac::Op(Op::Copy {
            ty: long_type(false),
            src: lhs,
            dst: v6,
        }));
        self.emit(v7);

        let v8 = self.tmp_var(long_type(false));
        let v9 = self.alloc(Tac::Op(Op::Copy {
            ty: long_type(false),
            src: v6,
            dst: v8,
        }));
        self.emit(v9);
        let v10 = self.tmp_var(ty.clone());
        let v11 = self.alloc(Tac::Operand(Operand::Integer {
            ty: long_type(false),
            value: 1,
        }));
        let v12 = self.alloc(Tac::Op(Op::RightShift {
            ty: long_type(false),
            lhs: v8,
            rhs: v11,
            dst: v10,
        }));
        self.emit(v12);

        let v13 = self.tmp_var(long_type(false));
        let v14 = self.alloc(Tac::Operand(Operand::Integer {
            ty: long_type(false),
            value: 1,
        }));
        let v15 = self.alloc(Tac::Op(Op::And {
            ty: long_type(false),
            lhs: v6,
            rhs: v14,
            dst: v13,
        }));
        self.emit(v15);

        let v16 = self.tmp_var(long_type(false));
        let v17 = self.alloc(Tac::Op(Op::Or {
            ty: long_type(false),
            lhs: v10,
            rhs: v13,
            dst: v16,
        }));
        self.emit(v17);

        let v18 = self.tmp_var(ty.clone());
        let v19 = self.alloc(Tac::Op(Op::IntToDouble {
            ty: ty.clone(),
            src: v16,
            dst: v18,
        }));
        self.emit(v19);

        let v20 = self.tmp_var(ty.clone());
        let v21 = self.alloc(Tac::Op(Op::Add {
            ty: ty.clone(),
            lhs: v18,
            rhs: v18,
            dst: v20,
        }));
        self.emit(v21);

        let v22 = self.alloc(Tac::Op(Op::Copy {
            ty: ty.clone(),
            src: v20,
            dst: dst,
        }));
        self.emit(v22);

        self.emit(end);
    }

    fn cast_double_to_uint(&mut self, ty: &TypeRef, lhs: TacId, dst: TacId) {
        let v0 = self.tmp_var(long_type(true));
        let v1 = self.alloc(Tac::Op(Op::DoubleToInt {
            ty: long_type(true),
            src: lhs,
            dst: v0,
        }));
        self.emit(v1);
        let v2 = self.alloc(Tac::Op(Op::Truncate {
            ty: ty.clone(),
            src: v0,
            dst: dst,
        }));
        self.emit(v2);
    }

    fn cast(
        &mut self,
        from_ty: &TypeRef,
        to_ty: &TypeRef,
        lhs: &TacId,
    ) -> TacId {
        let v0 = *lhs;

        if is_match(from_ty, to_ty) {
            return v0;
        }

        let name = format!(".tmp.{}", self.tmp_idx);
        self.tmp_idx += 1;
        let v1 = self.alloc(Tac::Operand(Operand::Var(to_ty.clone(), name)));

        let src_is_double = is_double_type(from_ty);
        let dst_is_double = is_double_type(to_ty);
        let src_is_int = is_int_type(from_ty);
        let dst_is_int = is_int_type(to_ty);

        if src_is_double && dst_is_int {
            if is_signed(to_ty) {
                let v2 = self.alloc(Tac::Op(Op::DoubleToInt {
                    ty: to_ty.clone(),
                    src: v0,
                    dst: v1,
                }));
                self.emit(v2);
            } else {
                if size_of(to_ty) == 8 {
                    self.cast_double_to_ulong(to_ty, v0, v1);
                } else {
                    self.cast_double_to_uint(to_ty, v0, v1);
                }
            }
        } else if src_is_int && dst_is_double {
            if is_signed(from_ty) {
                let v2 = self.alloc(Tac::Op(Op::IntToDouble {
                    ty: to_ty.clone(),
                    src: v0,
                    dst: v1,
                }));
                self.emit(v2);
            } else {
                if size_of(from_ty) == 8 {
                    self.cast_ulong_to_double(to_ty, v0, v1);
                } else {
                    let v2 = self.tmp_var(from_ty.clone());
                    let v3 = self.alloc(Tac::Op(Op::ZeroExt {
                        ty: from_ty.clone(),
                        src: v0,
                        dst: v2,
                    }));
                    self.emit(v3);
                    let v4 = self.alloc(Tac::Op(Op::IntToDouble {
                        ty: to_ty.clone(),
                        src: v2,
                        dst: v1,
                    }));
                    self.emit(v4);
                }
            }
        } else if size_of(to_ty) == size_of(from_ty) {
            let v2 = self.alloc(Tac::Op(Op::Copy {
                ty: to_ty.clone(),
                src: v0,
                dst: v1,
            }));
            self.emit(v2);
        } else if size_of(to_ty) < size_of(from_ty) {
            let v2 = self.alloc(Tac::Op(Op::Truncate {
                ty: to_ty.clone(),
                src: v0,
                dst: v1,
            }));
            self.emit(v2);
        } else if is_signed(from_ty) {
            let v2 = self.alloc(Tac::Op(Op::SignExt {
                ty: to_ty.clone(),
                src: v0,
                dst: v1,
            }));
            self.emit(v2);
        } else {
            let v2 = self.alloc(Tac::Op(Op::ZeroExt {
                ty: to_ty.clone(),
                src: v0,
                dst: v1,
            }));
            self.emit(v2);
        }

        v1
    }

    fn complement(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        lhs: AstId,
    ) -> TacId {
        let (v0, v1) = self.unop(arena, &node.ty, lhs);

        let v2 = self.alloc(Tac::Op(Op::Inv {
            ty: node.ty.clone(),
            src: v0,
            dst: v1,
        }));
        self.emit(v2);

        v1
    }

    fn negate(&mut self, arena: &AstArena, node: &Ast, lhs: AstId) -> TacId {
        if is_double_type(&node.ty) {
            let (v0, v1) = self.unop(arena, &node.ty, lhs);
            let minus_zero = self.const_double(-0.0f64);
            let v2 = self.alloc(Tac::Op(Op::Xor {
                ty: node.ty.clone(),
                lhs: v0,
                rhs: minus_zero,
                dst: v1,
            }));
            self.emit(v2);

            v1
        } else {
            let (v0, v1) = self.unop(arena, &node.ty, lhs);

            let v2 = self.alloc(Tac::Op(Op::Neg {
                ty: node.ty.clone(),
                src: v0,
                dst: v1,
            }));
            self.emit(v2);

            v1
        }
    }

    fn not(&mut self, arena: &AstArena, node: &Ast, lhs: AstId) -> TacId {
        let (v0, v1) = self.unop(arena, &node.ty, lhs);

        let v2 = self.alloc(Tac::Op(Op::Not {
            ty: node.ty.clone(),
            src: v0,
            dst: v1,
        }));
        self.emit(v2);

        v1
    }

    fn deref(&mut self, arena: &AstArena, lhs: AstId) -> TacId {
        let v0 = self.expr_and_convert(arena, lhs);
        self.alloc(Tac::Operand(Operand::DereferencedPtr(v0)))
    }

    fn addr_of(
        &mut self,
        arena: &AstArena,
        ptr_ty: &TypeRef,
        lhs: AstId,
    ) -> TacId {
        let v0 = self.expr(arena, lhs);
        let (is_plain, inner) = match &self.arena[v0.0] {
            Tac::Operand(Operand::PlainOperand(obj)) => (true, *obj),
            Tac::Operand(Operand::DereferencedPtr(ptr)) => (false, *ptr),
            _ => unreachable!(),
        };

        if is_plain {
            let v1 = self.tmp_var(ptr_ty.clone());
            let v2 = self.alloc(Tac::Op(Op::GetAddr {
                ty: ptr_ty.clone(),
                src: inner,
                dst: v1,
            }));
            self.emit(v2);
            self.alloc(Tac::Operand(Operand::PlainOperand(v1)))
        } else {
            self.alloc(Tac::Operand(Operand::PlainOperand(inner)))
        }
    }

    fn convert(&mut self, operand: TacId, ty: &TypeRef) -> TacId {
        let plain_val = match &self.arena[operand.0] {
            Tac::Operand(Operand::PlainOperand(value)) => Some(*value),
            _ => None,
        };
        if let Some(v0) = plain_val {
            return v0;
        }

        if let Tac::Operand(Operand::DereferencedPtr(ptr)) =
            &self.arena[operand.0]
        {
            let ptr = *ptr;
            let v0 = self.tmp_var(ty.clone());
            let v1 = self.alloc(Tac::Op(Op::Load {
                ty: ty.clone(),
                src: ptr,
                dst: v0,
            }));
            self.emit(v1);

            return v0;
        }

        unreachable!()
    }

    fn subscript(&mut self, arena: &AstArena, lhs: AstId, rhs: AstId) -> TacId {
        let ptr_ty = type_of(arena, lhs);
        let base_ty = base_type_v2(&ptr_ty);
        let elem_size = size_of(&base_ty);

        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.expr_and_convert(arena, rhs);

        let v2 = self.tmp_var(ptr_ty.clone());
        let v3 = self.alloc(Tac::Op(Op::AddPtr {
            ty: ptr_ty.clone(),
            lhs: v0,
            rhs: v1,
            scale: elem_size,
            dst: v2,
        }));
        self.emit(v3);

        self.alloc(Tac::Operand(Operand::DereferencedPtr(v2)))
    }

    fn expr_and_convert(&mut self, arena: &AstArena, lhs: AstId) -> TacId {
        let v0 = self.expr(arena, lhs);
        let ty = type_of(arena, lhs);

        self.convert(v0, &ty)
    }

    fn expr(&mut self, arena: &AstArena, expr_id: AstId) -> TacId {
        let node = &arena[expr_id];
        let ty = type_of(arena, expr_id);
        let v0: TacId;
        match &node.kind {
            AstKind::ConstInt(val) => {
                v0 = self.alloc(Tac::Operand(Operand::Integer {
                    ty: type_of(arena, expr_id),
                    value: *val as u64,
                }));
            }
            AstKind::ConstLong(val) => {
                v0 = self.alloc(Tac::Operand(Operand::Integer {
                    ty: type_of(arena, expr_id),
                    value: *val as u64,
                }));
            }
            AstKind::ConstUnsignedInt(val) => {
                v0 = self.alloc(Tac::Operand(Operand::Integer {
                    ty: type_of(arena, expr_id),
                    value: *val as u64,
                }));
            }
            AstKind::ConstUnsignedLong(val) => {
                v0 = self.alloc(Tac::Operand(Operand::Integer {
                    ty: type_of(arena, expr_id),
                    value: *val as u64,
                }));
            }
            AstKind::ConstDouble(val) => {
                v0 = self.const_double(*val);
            }
            AstKind::Complement { expr: lhs } => {
                v0 = self.complement(arena, node, *lhs);
            }
            AstKind::Negate { expr: lhs } => {
                v0 = self.negate(arena, node, *lhs);
            }
            AstKind::Not { expr: lhs } => {
                v0 = self.not(arena, node, *lhs);
            }
            AstKind::PostIncr { expr: lhs } => {
                v0 = self.post_incr_or_decr(arena, &node.ty, *lhs, true);
            }
            AstKind::PostDecr { expr: lhs } => {
                v0 = self.post_incr_or_decr(arena, &node.ty, *lhs, false);
            }
            AstKind::PreIncr { expr: lhs } => {
                v0 = self.pre_incr_or_decr(arena, &node.ty, *lhs, true);
            }
            AstKind::PreDecr { expr: lhs } => {
                v0 = self.pre_incr_or_decr(arena, &node.ty, *lhs, false);
            }
            AstKind::Multiply {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.multiply(arena, node, *lhs, *rhs);
            }
            AstKind::Divide {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.divide(arena, node, *lhs, *rhs);
            }
            AstKind::Modulo {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.modulo(arena, node, *lhs, *rhs);
            }
            AstKind::Add {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.add(arena, node, *lhs, *rhs);
            }
            AstKind::Subtract {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.subtract(arena, node, *lhs, *rhs);
            }
            AstKind::LeftShift {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.shift_left(arena, node, *lhs, *rhs);
            }
            AstKind::RightShift {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.shift_right(arena, node, *lhs, *rhs);
            }
            AstKind::And {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.and(arena, node, *lhs, *rhs);
            }
            AstKind::Or {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.or(arena, node, *lhs, *rhs);
            }
            AstKind::Xor {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.xor(arena, node, *lhs, *rhs);
            }
            AstKind::Equal {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.equal(arena, node, *lhs, *rhs);
            }
            AstKind::NotEq {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.not_eq(arena, node, *lhs, *rhs);
            }
            AstKind::Less {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.less(arena, node, *lhs, *rhs);
            }
            AstKind::LessOrEq {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.less_or_eq(arena, node, *lhs, *rhs);
            }
            AstKind::Greater {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.greater(arena, node, *lhs, *rhs);
            }
            AstKind::GreaterOrEq {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.greater_or_eq(arena, node, *lhs, *rhs);
            }
            AstKind::LogicAnd {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.logical_and(arena, &node.ty, *lhs, *rhs);
            }
            AstKind::LogicOr {
                left: lhs,
                right: rhs,
            } => {
                v0 = self.logical_or(arena, &node.ty, *lhs, *rhs);
            }
            AstKind::Assign {
                left: lhs,
                right: rhs,
            } => {
                return self.assign(arena, &node.ty, *lhs, *rhs);
            }
            AstKind::CompoundAssign {
                left: _,
                right: rhs,
            } => {
                return self.compound_assign(arena, &node.ty, *rhs);
            }
            AstKind::Ternary {
                left: lhs,
                middle,
                right: rhs,
            } => {
                v0 = self.ternary(arena, &node.ty, *lhs, *middle, *rhs);
            }
            AstKind::Identifier { name, .. } => {
                v0 = self.identifier(arena, node, expr_id, name);
            }
            AstKind::Call { expr: lhs, args } => {
                v0 = self.call(arena, node, *lhs, args);
            }

            AstKind::Cast {
                type_spec: _,
                expr: lhs,
            } => {
                let v1 = self.expr_and_convert(arena, *lhs);
                let from_ty = type_of(arena, *lhs);
                let to_ty = node.ty.clone();
                v0 = self.cast(&from_ty, &to_ty, &v1);
            }

            AstKind::Initialiser {
                type_spec: _,
                value,
            } => {
                if let Some(lhs) = value {
                    v0 = self.expr_and_convert(arena, *lhs);
                } else {
                    unreachable!()
                }
            }

            AstKind::Deref { expr: lhs } => {
                return self.deref(arena, *lhs);
            }

            AstKind::AddrOf { expr: lhs } => {
                return self.addr_of(arena, &ty, *lhs);
            }

            AstKind::Subscript {
                left: lhs,
                right: rhs,
            } => {
                return self.subscript(arena, *lhs, *rhs);
            }

            _ => {
                unreachable!()
            }
        }

        self.alloc(Tac::Operand(Operand::PlainOperand(v0)))
    }

    fn function(
        &mut self,
        arena: &AstArena,
        name: &Token,
        sym: Option<SymId>,
        params: &[AstId],
        body: &Option<AstId>,
    ) {
        let saved_label_map = std::mem::take(&mut self.label_map);
        let body_start = self.tac_code.len();

        if body.is_some() {
            let mut tac_params: Vec<TacId> = vec![];

            for &param in params {
                if let AstKind::Parameter {
                    name: _,
                    sym: _,
                    type_spec: _,
                } = &arena[param].kind
                {
                    let symtab = self.scopes();
                    let sym = resolve(symtab, arena, &param).unwrap();
                    let ty = arena[param].ty.clone();
                    let v0 = self.alloc(Tac::Operand(Operand::Var(
                        ty.clone(),
                        Self::pseudo_name(symtab, sym),
                    )));
                    tac_params.push(v0);
                }
            }

            if let AstKind::Block { body: b } = &arena[body.unwrap()].kind {
                for &stmt in b {
                    self.stmt_or_decl(arena, stmt);
                }
            }

            let v0 = self.alloc(Tac::Operand(Operand::Integer {
                ty: int_type(true),
                value: 0,
            }));
            let v1 = self.alloc(Tac::Op(Op::Return(int_type(true), v0)));
            self.emit(v1);

            let body_code: Vec<TacId> =
                self.tac_code.drain(body_start..).collect();

            self.label_map = saved_label_map;

            let global = if let Some(s) = sym {
                let symtab = self.scopes();
                !matches!(symtab[s].storage_class, Some(StorageClass::Static),)
            } else {
                unreachable!()
            };

            let v2 = self.alloc(Tac::Object(Object::Function {
                name: arena.token_str(name).to_string(),
                global: global,
                params: tac_params,
                code: body_code,
            }));

            self.emit(v2);
        }
    }

    fn block(&mut self, arena: &AstArena, body: &[AstId]) {
        for &stmt in body {
            self.stmt_or_decl(arena, stmt);
        }
    }

    fn if_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        cond: AstId,
        then: AstId,
        otherwise: &Option<AstId>,
    ) {
        let else_label = if otherwise.is_some() {
            Some(self.label())
        } else {
            None
        };
        let end_label = self.label();
        let v0 = self.expr_and_convert(arena, cond);
        let v1 = self.alloc(Tac::Op(Op::JumpOnZero {
            ty: node.ty.clone(),
            expr: v0,
            label: if else_label.is_some() {
                else_label.unwrap()
            } else {
                end_label
            },
        }));
        self.emit(v1);
        self.stmt_or_decl(arena, then);
        if otherwise.is_some() {
            let v2 = self.alloc(Tac::Op(Op::Jump(end_label)));
            self.emit(v2);
            self.emit(else_label.unwrap());
            self.stmt_or_decl(arena, otherwise.unwrap());
        }
        self.emit(end_label);
    }

    fn switch_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        cond: AstId,
        body: AstId,
        cases: &[AstId],
    ) {
        let v0 = self.expr_and_convert(arena, cond);
        let v1 = self.tmp_var(arena[cond].ty.clone());
        let v2 = self.alloc(Tac::Op(Op::Copy {
            ty: node.ty.clone(),
            src: v0,
            dst: v1,
        }));
        self.emit(v2);
        for &c in cases {
            if let AstKind::Case {
                expr,
                stmt: _,
                idx: _,
            } = &arena[c].kind
            {
                let v3 = self.expr_and_convert(arena, *expr);
                let v4 = self.tmp_var(arena[*expr].ty.clone());
                let v5 = self.alloc(Tac::Op(Op::Equal {
                    ty: node.ty.clone(),
                    lhs: v1,
                    rhs: v3,
                    dst: v4,
                }));
                self.emit(v5);
                let case_label = self.case_label(arena[c].scope, c.0);
                let v6 = self.alloc(Tac::Op(Op::JumpOnNotZero {
                    ty: node.ty.clone(),
                    expr: v4,
                    label: case_label,
                }));
                self.emit(v6);
            }
        }
        let mut has_default = false;
        for &c in cases {
            if let AstKind::Default { .. } = &arena[c].kind {
                let case_label = self.case_label(arena[c].scope, c.0);
                let v3 = self.alloc(Tac::Op(Op::Jump(case_label)));
                self.emit(v3);
                has_default = true;
            }
        }
        let break_label = self.break_label(arena[body].scope);
        if !has_default {
            let v3 = self.alloc(Tac::Op(Op::Jump(break_label)));
            self.emit(v3);
        }
        if !cases.is_empty() {
            self.stmt_or_decl(arena, body);
        }
        self.emit(break_label);
    }

    fn do_while_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        cond: AstId,
        body: AstId,
    ) {
        let start_label = self.label();
        self.emit(start_label);
        self.stmt_or_decl(arena, body);
        let continue_label = self.continue_label(arena[body].scope);
        self.emit(continue_label);
        let v0 = self.expr_and_convert(arena, cond);
        let v1 = self.alloc(Tac::Op(Op::JumpOnNotZero {
            ty: node.ty.clone(),
            expr: v0,
            label: start_label,
        }));
        self.emit(v1);
        let break_label = self.break_label(arena[body].scope);
        self.emit(break_label);
    }

    fn while_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        cond: AstId,
        body: AstId,
    ) {
        let continue_label = self.continue_label(arena[body].scope);
        self.emit(continue_label);
        let v0 = self.expr_and_convert(arena, cond);
        let break_label = self.break_label(arena[body].scope);
        let v1 = self.alloc(Tac::Op(Op::JumpOnZero {
            ty: node.ty.clone(),
            expr: v0,
            label: break_label,
        }));
        self.emit(v1);
        self.stmt_or_decl(arena, body);
        let v2 = self.alloc(Tac::Op(Op::Jump(continue_label)));
        self.emit(v2);
        self.emit(break_label);
    }

    fn for_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        init: &Option<AstId>,
        cond: &Option<AstId>,
        post: &Option<AstId>,
        body: AstId,
    ) {
        if let Some(init_id) = init {
            self.stmt_or_decl(arena, *init_id);
        }
        let start_label = self.label();
        self.emit(start_label);
        let break_label = self.break_label(arena[body].scope);
        if let Some(cond_id) = cond {
            let v0 = self.expr_and_convert(arena, *cond_id);
            let v1 = self.alloc(Tac::Op(Op::JumpOnZero {
                ty: node.ty.clone(),
                expr: v0,
                label: break_label,
            }));
            self.emit(v1);
        }
        self.stmt_or_decl(arena, body);
        let continue_label = self.continue_label(arena[body].scope);
        self.emit(continue_label);
        if let Some(post_id) = post {
            self.stmt_or_decl(arena, *post_id);
        }
        let v0 = self.alloc(Tac::Op(Op::Jump(start_label)));
        self.emit(v0);
        self.emit(break_label);
    }

    fn return_stmt(&mut self, arena: &AstArena, ty: &TypeRef, lhs: AstId) {
        let v0 = self.expr_and_convert(arena, lhs);
        let v1 = self.alloc(Tac::Op(Op::Return(ty.clone(), v0)));
        self.emit(v1);
    }

    fn goto_stmt(&mut self, arena: &AstArena, lhs: &Token) {
        let v0 = self.named_label(arena.token_str(lhs));
        let v1 = self.alloc(Tac::Op(Op::Jump(v0)));
        self.emit(v1);
    }

    fn labelled_stmt(&mut self, arena: &AstArena, name: &Token, stmt: AstId) {
        let v0 = self.named_label(arena.token_str(name));
        self.emit(v0);
        self.stmt_or_decl(arena, stmt);
    }

    fn case_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        case_id: AstId,
        stmt: AstId,
    ) {
        let v0 = self.case_label(node.scope, case_id.0);
        self.emit(v0);
        self.stmt_or_decl(arena, stmt);
    }

    fn default_stmt(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        case_id: AstId,
        stmt: AstId,
    ) {
        let v0 = self.case_label(node.scope, case_id.0);
        self.emit(v0);
        self.stmt_or_decl(arena, stmt);
    }

    fn break_stmt(&mut self, node: &Ast) {
        let v0 = self.break_label(node.scope);
        let v1 = self.alloc(Tac::Op(Op::Jump(v0)));
        self.emit(v1);
    }

    fn continue_stmt(&mut self, node: &Ast) {
        let v0 = self.continue_label(node.scope);
        let v1 = self.alloc(Tac::Op(Op::Jump(v0)));
        self.emit(v1);
    }

    fn compound_elem(
        &mut self,
        arena: &AstArena,
        init_ref: AstId,
        dst: &TacId,
        offset: &mut usize,
    ) {
        let is_compound =
            matches!(arena[init_ref].kind, AstKind::CompoundInitialiser(_));

        if is_compound {
            let elem_total_size = size_of(&arena[init_ref].ty);
            let start_offset = *offset;

            let initialisers = if let AstKind::CompoundInitialiser(ci) =
                &arena[init_ref].kind
            {
                ci.initialisers.clone()
            } else {
                unreachable!()
            };

            for &initialiser in &initialisers {
                self.compound_elem(arena, initialiser, dst, offset);
            }

            let scalar_ty = innermost_base_type(&arena[init_ref].ty);
            let scalar_size = size_of(&scalar_ty);
            while *offset < start_offset + elem_total_size {
                let zero_src = if is_double_type(&scalar_ty) {
                    self.const_double(0.0)
                } else {
                    self.alloc(Tac::Operand(Operand::Integer {
                        ty: scalar_ty.clone(),
                        value: 0,
                    }))
                };
                let v0 = self.alloc(Tac::Op(Op::CopyToOffset {
                    ty: scalar_ty.clone(),
                    src: zero_src,
                    dst: *dst,
                    off: *offset,
                }));
                self.emit(v0);
                *offset += scalar_size;
            }
        } else {
            let elem_ty = arena[init_ref].ty.clone();
            let elem_size = size_of(&elem_ty);
            let v0 = self.expr_and_convert(arena, init_ref);
            let v1 = self.alloc(Tac::Op(Op::CopyToOffset {
                ty: elem_ty,
                src: v0,
                dst: *dst,
                off: *offset,
            }));
            self.emit(v1);
            *offset += elem_size;
        }
    }

    fn variable(
        &mut self,
        arena: &AstArena,
        node: &Ast,
        ast: AstId,
        sym: Option<SymId>,
        init: &Option<AstId>,
    ) {
        if let Some(s) = sym {
            let symtab = self.scopes();
            if !has_static_storage_duration(symtab, s)
                && symtab.has_parent(node.scope)
                && init.is_some()
            {
                let init_ref = init.unwrap();
                let is_compound = matches!(
                    arena[init_ref].kind,
                    AstKind::CompoundInitialiser(_)
                );

                if is_compound {
                    let symtab = self.scopes();
                    let sym_resolved = resolve(symtab, arena, &ast).unwrap();
                    let v0 = self.alloc(Tac::Operand(Operand::Var(
                        node.ty.clone(),
                        Self::pseudo_name(symtab, sym_resolved),
                    )));
                    let initialisers = if let AstKind::CompoundInitialiser(ci) =
                        &arena[init_ref].kind
                    {
                        ci.initialisers.clone()
                    } else {
                        unreachable!()
                    };
                    let mut offset = 0usize;
                    for &initialiser in &initialisers {
                        self.compound_elem(
                            arena,
                            initialiser,
                            &v0,
                            &mut offset,
                        );
                    }

                    let total_size = size_of(&node.ty);
                    if offset < total_size {
                        let elem_ty = innermost_base_type(&node.ty);
                        let elem_size = size_of(&elem_ty);
                        while offset < total_size {
                            let zero_src = if is_double_type(&elem_ty) {
                                self.const_double(0.0)
                            } else {
                                self.alloc(Tac::Operand(Operand::Integer {
                                    ty: elem_ty.clone(),
                                    value: 0,
                                }))
                            };
                            let v1 = self.alloc(Tac::Op(Op::CopyToOffset {
                                ty: elem_ty.clone(),
                                src: zero_src,
                                dst: v0,
                                off: offset,
                            }));
                            self.emit(v1);
                            offset += elem_size;
                        }
                    }
                } else {
                    let name = {
                        let symtab = self.scopes();
                        let sym = resolve(symtab, arena, &ast).unwrap();
                        Self::pseudo_name(symtab, sym)
                    };
                    let v0 = self.expr_and_convert(arena, init_ref);
                    let v1 = self.alloc(Tac::Operand(Operand::Var(
                        node.ty.clone(),
                        name,
                    )));
                    let v2 = self.alloc(Tac::Op(Op::Copy {
                        ty: node.ty.clone(),
                        src: v0,
                        dst: v1,
                    }));
                    self.emit(v2);
                }
            }
        }
    }

    fn stmt_or_decl(&mut self, arena: &AstArena, ast: AstId) {
        let node = &arena[ast];

        match &node.kind {
            AstKind::Function {
                name,
                sym,
                params,
                block,
                type_spec: _,
            } => {
                if let Some(ident) = name {
                    self.function(arena, ident, *sym, params, block)
                }
            }
            AstKind::Block { body } => self.block(arena, body),
            AstKind::If {
                cond,
                then,
                otherwise,
            } => self.if_stmt(arena, node, *cond, *then, otherwise),
            AstKind::Switch { cond, body, cases } => {
                self.switch_stmt(arena, node, *cond, *body, cases)
            }
            AstKind::DoWhile { cond, body } => {
                self.do_while_stmt(arena, node, *cond, *body)
            }
            AstKind::While { cond, body } => {
                self.while_stmt(arena, node, *cond, *body)
            }
            AstKind::For {
                init,
                cond,
                post,
                body,
            } => self.for_stmt(arena, node, init, cond, post, *body),
            AstKind::Return { expr, .. } => {
                self.return_stmt(arena, &node.ty, *expr)
            }
            AstKind::GoTo { label } => self.goto_stmt(arena, label),
            AstKind::Label { name, stmt } => {
                self.labelled_stmt(arena, name, *stmt)
            }
            AstKind::Case { stmt, .. } => {
                self.case_stmt(arena, node, ast, *stmt)
            }
            AstKind::Default { stmt, .. } => {
                self.default_stmt(arena, node, ast, *stmt)
            }
            AstKind::Break { .. } => self.break_stmt(node),
            AstKind::Continue { .. } => self.continue_stmt(node),
            AstKind::ExprStmt { expr } => {
                _ = self.expr_and_convert(arena, *expr);
            }
            AstKind::Variable {
                name: _,
                sym,
                type_spec: _,
                init,
            } => self.variable(arena, node, ast, *sym, init),
            AstKind::Parameter {
                name: _,
                sym,
                type_spec: _,
            } => self.variable(arena, node, ast, *sym, &None),
            AstKind::EmptyStmt => {}
            _ => {
                unreachable!();
            }
        }
    }

    fn emit(&mut self, ir: TacId) {
        self.tac_code.push(ir);
    }

    fn symbol(
        &mut self,
        arena: &AstArena,
        symtab: &SymTab,
        sym: SymId,
        name: &str,
    ) -> Option<TacId> {
        let s = &symtab[sym];
        let kind = s.kind;
        let definition = s.definition.clone();
        let storage_class = s.storage_class;

        if let SymKind::Variable = &kind {
            if let Some(Definition::Concrete) = definition {
                if let Some(node) = sym_as_node(symtab, sym) {
                    let ty = arena[node].ty.clone();
                    if let AstKind::Variable { init, .. } = &arena[node].kind {
                        let v0 = self.static_init(arena, &ty, init.unwrap());
                        let global = !matches!(
                            storage_class,
                            Some(StorageClass::Static)
                        );

                        return Some(self.alloc(Tac::Object(
                            Object::StaticVar(
                                ty.clone(),
                                name.to_string(),
                                global,
                                v0,
                            ),
                        )));
                    }
                }
            } else if let Some(node) = sym_as_node(symtab, sym) {
                let ty = arena[node].ty.clone();

                if let AstKind::Variable { .. } = &arena[node].kind {
                    let global =
                        !matches!(storage_class, Some(StorageClass::Static));

                    let v0 = self.alloc(Tac::Operand(Operand::Integer {
                        ty: ty.clone(),
                        value: 0,
                    }));
                    let v1 = self
                        .alloc(Tac::Object(Object::StaticInit(ty.clone(), v0)));

                    let mut init_list: Vec<TacId> = Vec::new();
                    init_list.push(v1);
                    let scalar_ty = innermost_base_type(&ty);

                    let v2 = self.alloc(Tac::Object(Object::StaticInitList(
                        init_list,
                        size_of(&scalar_ty),
                    )));
                    let v3 = self.alloc(Tac::Object(Object::StaticVar(
                        ty.clone(),
                        name.to_string(),
                        global,
                        v2,
                    )));

                    return Some(v3);
                }
            }
        }

        None
    }

    fn rodata(&mut self, ty: TypeRef, name: &str, bits: u64) -> TacId {
        self.alloc(Tac::Object(Object::RoData(ty, name.to_string(), bits)))
    }

    fn symbols(
        &mut self,
        arena: &AstArena,
        symtab: &SymTab,
        scope: ScopeId,
    ) -> Vec<TacId> {
        assert!(!symtab.has_parent(scope));
        let mut v0 = vec![];

        let defs = symtab.defs();
        for &def in &defs {
            if has_static_storage_duration(symtab, def) {
                let name = symtab[def].name.clone();
                if let Some(tac) = self.symbol(arena, symtab, def, &name) {
                    v0.push(tac);
                }
            }
        }

        let fp_consts: Vec<_> = self
            .fp_consts
            .iter()
            .map(|(bits, idx)| (bits.0, *idx))
            .collect();
        for (bits, idx) in &fp_consts {
            let name = format!(".L{}", idx);
            v0.push(self.rodata(double_type(), &name, *bits));
        }

        v0
    }

    fn static_init(
        &mut self,
        arena: &AstArena,
        ty: &TypeRef,
        init: AstId,
    ) -> TacId {
        let mut init_list: Vec<TacId> = Vec::new();
        let value: u64;

        match &arena[init].kind {
            AstKind::CompoundInitialiser(_) => {
                let initialisers = if let AstKind::CompoundInitialiser(ci) =
                    &arena[init].kind
                {
                    ci.initialisers.clone()
                } else {
                    unreachable!()
                };

                let mut list: Vec<TacId> = Vec::new();

                let elem_ty = base_type_v2(ty);
                let elem_size = size_of(&elem_ty);

                let scalar_ty = innermost_base_type(&elem_ty);
                let scalar_size = size_of(&scalar_ty);

                for &initialiser in &initialisers {
                    let before = list.len();
                    let v0 = self.static_init(arena, &elem_ty, initialiser);
                    if let Tac::Object(Object::StaticInitList(
                        items,
                        _alignment,
                    )) = &self.arena[v0.0]
                    {
                        list.extend(items.clone());
                    } else {
                        list.push(v0);
                    }

                    let emitted: usize = list[before..]
                        .iter()
                        .map(|item| {
                            if let Tac::Object(Object::StaticInit(t, _)) =
                                &self.arena[item.0]
                            {
                                size_of(t)
                            } else {
                                0
                            }
                        })
                        .sum();
                    let gap = elem_size.saturating_sub(emitted);
                    for _ in 0..(gap / scalar_size) {
                        let v1 = self.alloc(Tac::Operand(Operand::Integer {
                            ty: scalar_ty.clone(),
                            value: 0,
                        }));
                        list.push(self.alloc(Tac::Object(Object::StaticInit(
                            scalar_ty.clone(),
                            v1,
                        ))));
                    }
                }

                return self.alloc(Tac::Object(Object::StaticInitList(
                    list,
                    scalar_size,
                )));
            }
            AstKind::Initialiser {
                type_spec: _,
                value,
            } => {
                if let Some(expr) = value {
                    return self.static_init(arena, ty, *expr);
                } else {
                    unreachable!()
                }
            }
            AstKind::Cast { expr, .. } => {
                return self.static_init(arena, ty, *expr);
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
                unreachable!()
            }
        }

        let v0 = if is_double_type(ty) {
            self.alloc(Tac::Operand(Operand::Double(f64::from_bits(value))))
        } else {
            self.alloc(Tac::Operand(Operand::Integer {
                ty: ty.clone(),
                value: value,
            }))
        };
        let v1 = self.alloc(Tac::Object(Object::StaticInit(ty.clone(), v0)));

        init_list.push(v1);

        self.alloc(Tac::Object(Object::StaticInitList(init_list, size_of(&ty))))
    }
}
