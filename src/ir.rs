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

use crate::ast::{ASTKind, AST};
use crate::scope::{Scope, ScopeKind};

macro_rules! new_node {
    ($variant:ident) => {
        Rc::new(TAC::$variant)
    };

    ($variant:ident ( $($args:expr),* $(,)? )) => {
        Rc::new(TAC::$variant( $($args),* ))
    };

    ($variant:ident { $($field:ident : $value:expr),* $(,)? }) => {
        Rc::new(TAC::$variant {
            $($field: $value),*
        })
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TAC {
    Function {
        name: String,
        code: Vec<Rc<TAC>>,
        depth: usize,
    },
    ConstInt(i64),
    Var(usize, usize),
    Inv {
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Neg {
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Not {
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Mul {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    IDiv {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Mod {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Add {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Sub {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LShift {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    RShift {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    And {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Or {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Xor {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Equal {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    NotEq {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LessThan {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LessOrEq {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    GreaterThan {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    GreaterOrEq {
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Copy {
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Jump(Rc<TAC>),
    JumpOnZero {
        expr: Rc<TAC>,
        label: Rc<TAC>,
    },
    JumpOnNotZero {
        expr: Rc<TAC>,
        label: Rc<TAC>,
    },
    Label(usize),
    Return(Rc<TAC>),
}

pub struct IrGenerator {
    label_index: usize,
    label_map: HashMap<String, usize>,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            label_index: 0,
            label_map: HashMap::new(),
        }
    }

    fn incr_label_index(&mut self) -> usize {
        let idx = self.label_index;
        self.label_index += 1;
        idx
    }

    fn add_named_label(&mut self, name: &str) -> Option<usize> {
        if !self.label_map.contains_key(name) {
            let idx = self.incr_label_index();
            self.label_map.insert(name.to_string(), idx);
        }
        self.label_map.get(name).cloned()
    }

    fn add_continue_label(
        &mut self,
        scope: &Rc<RefCell<Scope>>,
    ) -> Option<usize> {
        let scope_ref = scope.borrow();
        let id = if scope_ref.kind == ScopeKind::Loop {
            scope_ref.id
        } else {
            scope_ref.loop_scope().unwrap().borrow().id
        };
        let label = format!(".continue.{}", id);
        self.add_named_label(&label)
    }

    fn add_break_label(&mut self, scope: &Rc<RefCell<Scope>>) -> Option<usize> {
        let scope_ref = scope.borrow();
        let id = if scope_ref.kind == ScopeKind::Loop
            || scope_ref.kind == ScopeKind::Switch
        {
            scope_ref.id
        } else {
            scope_ref.loop_or_switch_scope().unwrap().borrow().id
        };
        let label = format!(".break.{}", id);
        self.add_named_label(&label)
    }

    fn add_case_label(
        &mut self,
        scope: &Rc<RefCell<Scope>>,
        case_idx: usize,
    ) -> Option<usize> {
        let scope_ref = scope.borrow();
        let id = if scope_ref.kind == ScopeKind::Switch {
            scope_ref.id
        } else {
            scope_ref.switch_scope().unwrap().borrow().id
        };
        let label = format!(".case.{}.{}", case_idx, id);
        self.add_named_label(&label)
    }

    fn create_tmp_var(&mut self, scope: &Rc<RefCell<Scope>>) -> Rc<TAC> {
        let off = scope.borrow_mut().add_tmp(4, 4);
        new_node!(Var(off, 4))
    }

    fn transform_expr(
        &mut self,
        expr: Rc<AST>,
        tac_code: &mut Vec<Rc<TAC>>,
    ) -> Rc<TAC> {
        let binding = expr;
        match &binding.kind {
            ASTKind::ConstInt(val) => new_node!(ConstInt(*val)),
            ASTKind::Complement { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Inv {
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Negate { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Neg {
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Not { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Not {
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::PreIncr { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                let tmp = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                tac_code.push(new_node!(Add {
                    lhs: tmp.clone(),
                    rhs: src.clone(),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Copy {
                    src: dst.clone(),
                    dst: src.clone()
                }));
                src
            }
            ASTKind::PostIncr { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                let tmp = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: src.clone(),
                    dst: tmp.clone()
                }));
                tac_code.push(new_node!(Add {
                    lhs: new_node!(ConstInt(1)),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Copy {
                    src: dst.clone(),
                    dst: src.clone()
                }));
                tmp
            }
            ASTKind::PreDecr { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                let tmp = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                tac_code.push(new_node!(Sub {
                    lhs: src.clone(),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Copy {
                    src: dst.clone(),
                    dst: src.clone()
                }));
                src
            }
            ASTKind::PostDecr { expr: inner } => {
                let src = self.transform_expr(inner.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                let tmp = self.create_tmp_var(&binding.scope);
                let res = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: src.clone(),
                    dst: res.clone()
                }));
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                tac_code.push(new_node!(Sub {
                    lhs: src.clone(),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Copy {
                    src: dst.clone(),
                    dst: src.clone()
                }));
                res
            }
            ASTKind::Multiply { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Mul {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Divide { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(IDiv {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Modulo { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Mod {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Add { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Add {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Subtract { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Sub {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LShift { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(LShift {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::RShift { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(RShift {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::And { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(And {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Or { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Or {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Xor { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Xor {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Equal { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Equal {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::NotEq { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(NotEq {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LessThan { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(LessThan {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LessOrEq { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(LessOrEq {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::GreaterThan { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(GreaterThan {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::GreaterOrEq { left, right } => {
                let lhs = self.transform_expr(left.clone(), tac_code);
                let rhs = self.transform_expr(right.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(GreaterOrEq {
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LogicAnd { left, right } => {
                let false_label = new_node!(Label(self.incr_label_index()));
                let end_label = new_node!(Label(self.incr_label_index()));
                let x = self.transform_expr(left.clone(), tac_code);
                tac_code.push(new_node!(JumpOnZero {
                    expr: x,
                    label: false_label.clone()
                }));
                let y = self.transform_expr(right.clone(), tac_code);
                tac_code.push(new_node!(JumpOnZero {
                    expr: y,
                    label: false_label.clone()
                }));
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(1)),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Jump(end_label.clone())));
                tac_code.push(false_label);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(0)),
                    dst: dst.clone()
                }));
                tac_code.push(end_label);
                dst
            }
            ASTKind::LogicOr { left, right } => {
                let true_label = new_node!(Label(self.incr_label_index()));
                let end_label = new_node!(Label(self.incr_label_index()));
                let x = self.transform_expr(left.clone(), tac_code);
                tac_code.push(new_node!(JumpOnNotZero {
                    expr: x,
                    label: true_label.clone()
                }));
                let y = self.transform_expr(right.clone(), tac_code);
                tac_code.push(new_node!(JumpOnNotZero {
                    expr: y,
                    label: true_label.clone()
                }));
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(0)),
                    dst: dst.clone()
                }));
                tac_code.push(new_node!(Jump(end_label.clone())));
                tac_code.push(true_label);
                tac_code.push(new_node!(Copy {
                    src: new_node!(ConstInt(1)),
                    dst: dst.clone()
                }));
                tac_code.push(end_label);
                dst
            }
            ASTKind::Assign { left, right } => {
                let exp = self.transform_expr(right.clone(), tac_code);
                let dst = self.transform_expr(left.clone(), tac_code);
                tac_code.push(new_node!(Copy {
                    src: exp,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Conditional {
                left,
                middle,
                right,
            } => {
                let e2_label = new_node!(Label(self.incr_label_index()));
                let c = self.transform_expr(left.clone(), tac_code);
                tac_code.push(new_node!(JumpOnZero {
                    expr: c,
                    label: e2_label.clone()
                }));
                let e1 = self.transform_expr(middle.clone(), tac_code);
                let res = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: e1.clone(),
                    dst: res.clone()
                }));
                let end_label = new_node!(Label(self.incr_label_index()));
                tac_code.push(new_node!(Jump(end_label.clone())));
                tac_code.push(e2_label.clone());
                let e2 = self.transform_expr(right.clone(), tac_code);
                tac_code.push(new_node!(Copy {
                    src: e2.clone(),
                    dst: res.clone()
                }));
                tac_code.push(end_label.clone());
                res
            }
            ASTKind::Identifier { name: _, sym } => {
                new_node!(Var(sym.as_ref().unwrap().0, 4))
            }
            _ => unreachable!(),
        }
    }

    fn transform(&mut self, node: Rc<AST>, tac_code: &mut Vec<Rc<TAC>>) {
        let binding = node;
        match &binding.kind {
            ASTKind::Function {
                name,
                params: _,
                block,
                rtype: _,
                scope,
            } => {
                let mut func_code: Vec<Rc<TAC>> = vec![];
                match &block.clone().unwrap().kind {
                    ASTKind::Block { body } => {
                        for stmt in body {
                            self.transform(stmt.clone(), &mut func_code);
                        }
                    }
                    _ => {}
                }
                func_code.push(new_node!(Return(new_node!(ConstInt(0)))));
                tac_code.push(new_node!(Function {
                    name: name.clone(),
                    code: func_code,
                    depth: scope.borrow().depth,
                }));
            }
            ASTKind::Block { body } => {
                for stmt in body {
                    self.transform(stmt.clone(), tac_code);
                }
            }
            ASTKind::If {
                cond,
                then,
                otherwise,
            } => {
                let else_label = if otherwise.is_some() {
                    Some(new_node!(Label(self.incr_label_index())))
                } else {
                    None
                };
                let end_label = new_node!(Label(self.incr_label_index()));
                let c = self.transform_expr(cond.clone(), tac_code);
                tac_code.push(new_node!(JumpOnZero {
                    expr: c,
                    label: if else_label.is_some() {
                        else_label.as_ref().unwrap().clone()
                    } else {
                        end_label.clone()
                    }
                }));
                self.transform(then.clone(), tac_code);
                if otherwise.is_some() {
                    tac_code.push(new_node!(Jump(end_label.clone())));
                    tac_code.push(else_label.unwrap());
                    self.transform(
                        otherwise.as_ref().unwrap().clone(),
                        tac_code,
                    );
                }
                tac_code.push(end_label);
            }
            ASTKind::Switch { cond, body, cases } => {
                let cond = self.transform_expr(cond.clone(), tac_code);
                let dst = self.create_tmp_var(&binding.scope);
                tac_code.push(new_node!(Copy {
                    src: cond,
                    dst: dst.clone()
                }));
                for c in cases.iter() {
                    match &c.kind {
                        ASTKind::Case {
                            expr,
                            stmt: _,
                            idx: _,
                        } => {
                            let e = self.transform_expr(expr.clone(), tac_code);
                            let tmp = self.create_tmp_var(&binding.scope);
                            tac_code.push(new_node!(Equal {
                                lhs: dst.clone(),
                                rhs: e,
                                dst: tmp.clone()
                            }));
                            let label_idx = self.add_case_label(&c.scope, c.id);
                            let case_label =
                                new_node!(Label(label_idx.unwrap()));
                            tac_code.push(new_node!(JumpOnNotZero {
                                expr: tmp,
                                label: case_label
                            }));
                        }
                        _ => {}
                    }
                }
                let mut has_default = false;
                for c in cases.iter() {
                    match &c.kind {
                        ASTKind::Default { .. } => {
                            let label_idx = self.add_case_label(&c.scope, c.id);
                            let case_label =
                                new_node!(Label(label_idx.unwrap()));
                            tac_code.push(new_node!(Jump(case_label)));
                            has_default = true;
                        }
                        _ => {}
                    }
                }
                let break_label = new_node!(Label(
                    self.add_break_label(&body.scope).unwrap()
                ));
                if !has_default {
                    tac_code.push(new_node!(Jump(break_label.clone())));
                }
                if cases.len() > 0 {
                    self.transform(body.clone(), tac_code);
                }
                tac_code.push(break_label);
            }
            ASTKind::DoWhile { cond, body } => {
                let start_label = new_node!(Label(self.incr_label_index()));
                tac_code.push(start_label.clone());
                self.transform(body.clone(), tac_code);
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.scope).unwrap()
                ));
                tac_code.push(continue_label);
                let c = self.transform_expr(cond.clone(), tac_code);
                tac_code.push(new_node!(JumpOnNotZero {
                    expr: c,
                    label: start_label
                }));
                let break_label = new_node!(Label(
                    self.add_break_label(&body.scope).unwrap()
                ));
                tac_code.push(break_label);
            }
            ASTKind::While { cond, body } => {
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.scope).unwrap()
                ));
                tac_code.push(continue_label.clone());
                let c = self.transform_expr(cond.clone(), tac_code);
                let break_label = new_node!(Label(
                    self.add_break_label(&body.scope).unwrap()
                ));
                tac_code.push(new_node!(JumpOnZero {
                    expr: c,
                    label: break_label.clone()
                }));
                self.transform(body.clone(), tac_code);
                tac_code.push(new_node!(Jump(continue_label)));
                tac_code.push(break_label);
            }
            ASTKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if init.is_some() {
                    self.transform(init.as_ref().unwrap().clone(), tac_code);
                }
                let start_label = new_node!(Label(self.incr_label_index()));
                tac_code.push(start_label.clone());
                let break_label = new_node!(Label(
                    self.add_break_label(&body.scope).unwrap()
                ));
                if cond.is_some() {
                    let c = self.transform_expr(
                        cond.as_ref().unwrap().clone(),
                        tac_code,
                    );
                    tac_code.push(new_node!(JumpOnZero {
                        expr: c,
                        label: break_label.clone()
                    }));
                }
                self.transform(body.clone(), tac_code);
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.scope).unwrap()
                ));
                tac_code.push(continue_label.clone());
                if post.is_some() {
                    self.transform(post.as_ref().unwrap().clone(), tac_code);
                }
                tac_code.push(new_node!(Jump(start_label.clone())));
                tac_code.push(break_label);
            }
            ASTKind::Return { expr } => {
                let e = self.transform_expr(expr.clone(), tac_code);
                tac_code.push(new_node!(Return(e)));
            }
            ASTKind::GoTo { label } => {
                let idx = self.add_named_label(label);
                let l = new_node!(Label(idx.unwrap()));
                tac_code.push(new_node!(Jump(l.clone())));
            }
            ASTKind::Label { name, stmt } => {
                let idx = self.add_named_label(name);
                let l = new_node!(Label(idx.unwrap()));
                tac_code.push(l);
                self.transform(stmt.clone(), tac_code);
            }
            ASTKind::Case { stmt, .. } => {
                let label_idx = self.add_case_label(&binding.scope, binding.id);
                tac_code.push(new_node!(Label(label_idx.unwrap())));
                self.transform(stmt.clone(), tac_code);
            }
            ASTKind::Default { stmt, .. } => {
                let label_idx = self.add_case_label(&binding.scope, binding.id);
                tac_code.push(new_node!(Label(label_idx.unwrap())));
                self.transform(stmt.clone(), tac_code);
            }
            ASTKind::Break { .. } => {
                let idx = self.add_break_label(&binding.scope);
                let l = new_node!(Label(idx.unwrap()));
                tac_code.push(new_node!(Jump(l.clone())));
            }
            ASTKind::Continue { .. } => {
                let idx = self.add_continue_label(&binding.scope);
                let l = new_node!(Label(idx.unwrap()));
                tac_code.push(new_node!(Jump(l.clone())));
            }
            ASTKind::ExprStmt { expr } => {
                _ = self.transform_expr(expr.clone(), tac_code);
            }
            ASTKind::Variable { name, typ: _, init } => {
                if init.is_some() {
                    let e = self.transform_expr(
                        init.as_ref().unwrap().clone(),
                        tac_code,
                    );
                    let scope = &binding.scope;
                    let v = new_node!(Var(
                        scope.borrow().find_off(&name).unwrap(),
                        4
                    ));
                    tac_code.push(new_node!(Copy { src: e, dst: v }));
                }
            }
            ASTKind::EmptyStmt => {}
            _ => {
                unreachable!();
            }
        }
    }

    pub fn generate(&mut self, ast: Vec<Rc<AST>>) -> Vec<Rc<TAC>> {
        let mut code: Vec<Rc<TAC>> = vec![];
        for node in ast {
            self.transform(node, &mut code);
        }
        code
    }
}
