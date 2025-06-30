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

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::scope::*;
use crate::types::*;

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
        params: Vec<Rc<TAC>>,
        code: Vec<Rc<TAC>>,
        depth: usize,
    },
    Call {
        ty: TypeRef,
        func: Rc<TAC>,
        args: Vec<Rc<TAC>>,
        dst: Rc<TAC>,
    },
    ConstInt(i64),
    Var(TypeRef, usize),
    Inv {
        ty: TypeRef,
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Neg {
        ty: TypeRef,
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Not {
        ty: TypeRef,
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Mul {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    IDiv {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Mod {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Add {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Sub {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LShift {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    RShift {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    And {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Or {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Xor {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Equal {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    NotEq {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LessThan {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    LessOrEq {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    GreaterThan {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    GreaterOrEq {
        ty: TypeRef,
        lhs: Rc<TAC>,
        rhs: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Copy {
        ty: TypeRef,
        src: Rc<TAC>,
        dst: Rc<TAC>,
    },
    Jump(Rc<TAC>),
    JumpOnZero {
        ty: TypeRef,
        expr: Rc<TAC>,
        label: Rc<TAC>,
    },
    JumpOnNotZero {
        ty: TypeRef,
        expr: Rc<TAC>,
        label: Rc<TAC>,
    },
    Label(usize),
    FunctionRef(String, bool),
    Return(Rc<TAC>),
}

pub struct IrGenerator {
    label_idx: usize,
    label_map: HashMap<String, usize>,
    tac_code: Vec<Rc<TAC>>,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            label_idx: 0,
            label_map: HashMap::new(),
            tac_code: Vec::new(),
        }
    }

    pub fn from(parent: &IrGenerator) -> Self {
        Self {
            label_idx: parent.label_idx,
            label_map: HashMap::new(),
            tac_code: Vec::new(),
        }
    }

    pub fn join(&mut self, child: &IrGenerator) {
        self.label_idx = child.label_idx;
    }

    fn incr_label_idx(&mut self) -> usize {
        let idx = self.label_idx;
        self.label_idx += 1;
        idx
    }

    fn add_named_label(&mut self, name: &str) -> Option<usize> {
        if !self.label_map.contains_key(name) {
            let idx = self.incr_label_idx();
            self.label_map.insert(name.to_string(), idx);
        }
        self.label_map.get(name).cloned()
    }

    fn add_continue_label(&mut self, scope: &ScopeRef) -> Option<usize> {
        let id = loop_scope(scope.clone()).unwrap().as_ref().borrow().id;
        let label = format!(".continue.{}", id);
        self.add_named_label(&label)
    }

    fn add_break_label(&mut self, scope: &ScopeRef) -> Option<usize> {
        let id = loop_or_switch_scope(scope.clone())
            .unwrap()
            .as_ref()
            .borrow()
            .id;
        let label = format!(".break.{}", id);
        self.add_named_label(&label)
    }

    fn add_case_label(
        &mut self,
        scope: &ScopeRef,
        case_idx: usize,
    ) -> Option<usize> {
        let id = switch_scope(scope.clone()).unwrap().as_ref().borrow().id;
        let label = format!(".case.{}.{}", case_idx, id);
        self.add_named_label(&label)
    }

    fn new_tmp_var(&mut self, ty: TypeRef, scope: &ScopeRef) -> Rc<TAC> {
        let ty_val = ty.as_ref().borrow();
        let off = scope
            .borrow_mut()
            .add_tmp_var(ty_val.size, ty_val.alignment);
        new_node!(Var(ty.clone(), off))
    }

    #[allow(unused_variables)]
    fn transform_expr(&mut self, expr: ASTRef) -> Rc<TAC> {
        let binding = expr.borrow();
        match &binding.kind {
            ASTKind::ConstInt(val) => new_node!(ConstInt(*val)),
            ASTKind::Complement { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Inv {
                    ty: binding.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Negate { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Neg {
                    ty: binding.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Not { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Not {
                    ty: binding.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::PreIncr { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let tmp = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                self.emit(new_node!(Add {
                    ty: binding.ty.clone(),
                    lhs: tmp.clone(),
                    rhs: src.clone(),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: dst.clone(),
                    dst: src.clone()
                }));
                src
            }
            ASTKind::PostIncr { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let tmp = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: src.clone(),
                    dst: tmp.clone()
                }));
                self.emit(new_node!(Add {
                    ty: binding.ty.clone(),
                    lhs: new_node!(ConstInt(1)),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: dst.clone(),
                    dst: src.clone()
                }));
                tmp
            }
            ASTKind::PreDecr { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let tmp = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                self.emit(new_node!(Sub {
                    ty: binding.ty.clone(),
                    lhs: src.clone(),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: dst.clone(),
                    dst: src.clone()
                }));
                src
            }
            ASTKind::PostDecr { expr: inner } => {
                let src = self.transform_expr(inner.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let tmp = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let res = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: src.clone(),
                    dst: res.clone()
                }));
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(1)),
                    dst: tmp.clone()
                }));
                self.emit(new_node!(Sub {
                    ty: binding.ty.clone(),
                    lhs: src.clone(),
                    rhs: tmp.clone(),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: dst.clone(),
                    dst: src.clone()
                }));
                res
            }
            ASTKind::Multiply { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Mul {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Divide { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(IDiv {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Modulo { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Mod {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Add { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Add {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Subtract { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Sub {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LShift { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(LShift {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::RShift { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(RShift {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::And { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(And {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Or { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Or {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Xor { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Xor {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::Equal { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Equal {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::NotEq { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(NotEq {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LessThan { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(LessThan {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LessOrEq { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(LessOrEq {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::GreaterThan { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(GreaterThan {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::GreaterOrEq { left, right } => {
                let lhs = self.transform_expr(left.clone());
                let rhs = self.transform_expr(right.clone());
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(GreaterOrEq {
                    ty: binding.ty.clone(),
                    lhs: lhs,
                    rhs: rhs,
                    dst: dst.clone()
                }));
                dst
            }
            ASTKind::LogicAnd { left, right } => {
                let false_label = new_node!(Label(self.incr_label_idx()));
                let end_label = new_node!(Label(self.incr_label_idx()));
                let x = self.transform_expr(left.clone());
                self.emit(new_node!(JumpOnZero {
                    ty: binding.ty.clone(),
                    expr: x,
                    label: false_label.clone()
                }));
                let y = self.transform_expr(right.clone());
                self.emit(new_node!(JumpOnZero {
                    ty: binding.ty.clone(),
                    expr: y,
                    label: false_label.clone()
                }));
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(1)),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Jump(end_label.clone())));
                self.emit(false_label);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(0)),
                    dst: dst.clone()
                }));
                self.emit(end_label);
                dst
            }
            ASTKind::LogicOr { left, right } => {
                let true_label = new_node!(Label(self.incr_label_idx()));
                let end_label = new_node!(Label(self.incr_label_idx()));
                let x = self.transform_expr(left.clone());
                self.emit(new_node!(JumpOnNotZero {
                    ty: binding.ty.clone(),
                    expr: x,
                    label: true_label.clone()
                }));
                let y = self.transform_expr(right.clone());
                self.emit(new_node!(JumpOnNotZero {
                    ty: binding.ty.clone(),
                    expr: y,
                    label: true_label.clone()
                }));
                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(0)),
                    dst: dst.clone()
                }));
                self.emit(new_node!(Jump(end_label.clone())));
                self.emit(true_label);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: new_node!(ConstInt(1)),
                    dst: dst.clone()
                }));
                self.emit(end_label);
                dst
            }
            ASTKind::Assign { left, right } => {
                let exp = self.transform_expr(right.clone());
                let dst = self.transform_expr(left.clone());
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
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
                let e2_label = new_node!(Label(self.incr_label_idx()));
                let c = self.transform_expr(left.clone());
                self.emit(new_node!(JumpOnZero {
                    ty: binding.ty.clone(),
                    expr: c,
                    label: e2_label.clone()
                }));
                let e1 = self.transform_expr(middle.clone());
                let res = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: e1.clone(),
                    dst: res.clone()
                }));
                let end_label = new_node!(Label(self.incr_label_idx()));
                self.emit(new_node!(Jump(end_label.clone())));
                self.emit(e2_label.clone());
                let e2 = self.transform_expr(right.clone());
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: e2.clone(),
                    dst: res.clone()
                }));
                self.emit(end_label.clone());
                res
            }
            ASTKind::Identifier { .. } => {
                let node_ref = expr.as_ref().borrow();
                let sym_rc = node_ref.resolve().unwrap();
                let sym = sym_rc.borrow();
                let sym_node = node_ref.resolve_node();

                match &sym_node.unwrap().borrow().kind {
                    ASTKind::Function { name, .. } => {
                        let defined = get_sym(
                            global_scope(binding.scope.clone()),
                            name,
                            None,
                        )
                        .unwrap()
                        .borrow()
                        .defined;
                        new_node!(FunctionRef(name.clone(), defined))
                    }
                    _ => {
                        new_node!(Var(binding.ty.clone(), sym.offset))
                    }
                }
            }

            ASTKind::Call { expr, args } => {
                let mut arg_exprs: Vec<Rc<TAC>> = vec![];

                for arg in args {
                    arg_exprs.push(self.transform_expr(arg.clone()));
                }

                let dst = self.new_tmp_var(binding.ty.clone(), &binding.scope);
                let fun = self.transform_expr(expr.clone());

                self.emit(new_node!(Call {
                    ty: binding.ty.clone(),
                    func: fun,
                    args: arg_exprs,
                    dst: dst.clone()
                }));

                dst
            }

            _ => {
                unreachable!()
            }
        }
    }

    fn transform(&mut self, node: ASTRef) {
        let binding = node.borrow();
        match &binding.kind {
            ASTKind::Function {
                name,
                params,
                block,
                ty: _,
                scope,
            } => {
                let mut irgen = IrGenerator::from(self);

                if block.is_some() {
                    let mut tac_params: Vec<Rc<TAC>> = vec![];

                    for param in params {
                        if let ASTKind::Parameter {
                            name,
                            idx: _,
                            ty: _,
                        } = &param.borrow().kind
                        {
                            if name.is_some() {
                                // otherwise 'void'
                                let ty = param.as_ref().borrow().ty.clone();
                                let offset = find_offset(
                                    param.as_ref().borrow().scope.clone(),
                                    &name.as_ref().unwrap(),
                                    None,
                                )
                                .unwrap();
                                let p = new_node!(Var(ty, offset));
                                tac_params.push(p);
                            }
                        }
                    }

                    match &block.as_ref().unwrap().borrow().kind {
                        ASTKind::Block { body } => {
                            for stmt in body {
                                irgen.transform(stmt.clone());
                            }
                        }
                        _ => {}
                    }

                    irgen.emit(new_node!(Return(new_node!(ConstInt(0)))));

                    self.join(&irgen);

                    let func = new_node!(Function {
                        name: name.clone(),
                        params: tac_params,
                        code: irgen.out(),
                        depth: scope.borrow().depth,
                    });

                    self.emit(func);
                }
            }

            ASTKind::Block { body } => {
                for stmt in body {
                    self.transform(stmt.clone());
                }
            }
            ASTKind::If {
                cond,
                then,
                otherwise,
            } => {
                let else_label = if otherwise.is_some() {
                    Some(new_node!(Label(self.incr_label_idx())))
                } else {
                    None
                };
                let end_label = new_node!(Label(self.incr_label_idx()));
                let c = self.transform_expr(cond.clone());
                self.emit(new_node!(JumpOnZero {
                    ty: binding.ty.clone(),
                    expr: c,
                    label: if else_label.is_some() {
                        else_label.as_ref().unwrap().clone()
                    } else {
                        end_label.clone()
                    }
                }));
                self.transform(then.clone());
                if otherwise.is_some() {
                    self.emit(new_node!(Jump(end_label.clone())));
                    self.emit(else_label.unwrap());
                    self.transform(otherwise.as_ref().unwrap().clone());
                }
                self.emit(end_label);
            }
            ASTKind::Switch { cond, body, cases } => {
                let src = self.transform_expr(cond.clone());
                let dst =
                    self.new_tmp_var(cond.borrow().ty.clone(), &binding.scope);
                self.emit(new_node!(Copy {
                    ty: binding.ty.clone(),
                    src: src,
                    dst: dst.clone()
                }));
                for c in cases.iter() {
                    match &c.borrow().kind {
                        ASTKind::Case {
                            expr,
                            stmt: _,
                            idx: _,
                        } => {
                            let e = self.transform_expr(expr.clone());
                            let tmp = self.new_tmp_var(
                                expr.borrow().ty.clone(),
                                &binding.scope,
                            );
                            self.emit(new_node!(Equal {
                                ty: binding.ty.clone(),
                                lhs: dst.clone(),
                                rhs: e,
                                dst: tmp.clone()
                            }));
                            let label_idx = self.add_case_label(
                                &c.borrow().scope,
                                c.borrow().id,
                            );
                            let case_label =
                                new_node!(Label(label_idx.unwrap()));
                            self.emit(new_node!(JumpOnNotZero {
                                ty: binding.ty.clone(),
                                expr: tmp,
                                label: case_label
                            }));
                        }
                        _ => {}
                    }
                }
                let mut has_default = false;
                for c in cases.iter() {
                    match &c.borrow().kind {
                        ASTKind::Default { .. } => {
                            let label_idx = self.add_case_label(
                                &c.borrow().scope,
                                c.borrow().id,
                            );
                            let case_label =
                                new_node!(Label(label_idx.unwrap()));
                            self.emit(new_node!(Jump(case_label)));
                            has_default = true;
                        }
                        _ => {}
                    }
                }
                let break_label = new_node!(Label(
                    self.add_break_label(&body.borrow().scope).unwrap()
                ));
                if !has_default {
                    self.emit(new_node!(Jump(break_label.clone())));
                }
                if cases.len() > 0 {
                    self.transform(body.clone());
                }
                self.emit(break_label);
            }
            ASTKind::DoWhile { cond, body } => {
                let start_label = new_node!(Label(self.incr_label_idx()));
                self.emit(start_label.clone());
                self.transform(body.clone());
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.borrow().scope).unwrap()
                ));
                self.emit(continue_label);
                let c = self.transform_expr(cond.clone());
                self.emit(new_node!(JumpOnNotZero {
                    ty: binding.ty.clone(),
                    expr: c,
                    label: start_label
                }));
                let break_label = new_node!(Label(
                    self.add_break_label(&body.borrow().scope).unwrap()
                ));
                self.emit(break_label);
            }
            ASTKind::While { cond, body } => {
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.borrow().scope).unwrap()
                ));
                self.emit(continue_label.clone());
                let c = self.transform_expr(cond.clone());
                let break_label = new_node!(Label(
                    self.add_break_label(&body.borrow().scope).unwrap()
                ));
                self.emit(new_node!(JumpOnZero {
                    ty: binding.ty.clone(),
                    expr: c,
                    label: break_label.clone()
                }));
                self.transform(body.clone());
                self.emit(new_node!(Jump(continue_label)));
                self.emit(break_label);
            }
            ASTKind::For {
                init,
                cond,
                post,
                body,
            } => {
                if init.is_some() {
                    self.transform(init.as_ref().unwrap().clone());
                }
                let start_label = new_node!(Label(self.incr_label_idx()));
                self.emit(start_label.clone());
                let break_label = new_node!(Label(
                    self.add_break_label(&body.borrow().scope).unwrap()
                ));
                if cond.is_some() {
                    let c = self.transform_expr(cond.as_ref().unwrap().clone());
                    self.emit(new_node!(JumpOnZero {
                        ty: binding.ty.clone(),
                        expr: c,
                        label: break_label.clone()
                    }));
                }
                self.transform(body.clone());
                let continue_label = new_node!(Label(
                    self.add_continue_label(&body.borrow().scope).unwrap()
                ));
                self.emit(continue_label.clone());
                if post.is_some() {
                    self.transform(post.as_ref().unwrap().clone());
                }
                self.emit(new_node!(Jump(start_label.clone())));
                self.emit(break_label);
            }
            ASTKind::Return { expr } => {
                let e = self.transform_expr(expr.clone());
                self.emit(new_node!(Return(e)));
            }
            ASTKind::GoTo { label } => {
                let idx = self.add_named_label(label);
                let l = new_node!(Label(idx.unwrap()));
                self.emit(new_node!(Jump(l.clone())));
            }
            ASTKind::Label { name, stmt } => {
                let idx = self.add_named_label(name);
                let l = new_node!(Label(idx.unwrap()));
                self.emit(l);
                self.transform(stmt.clone());
            }
            ASTKind::Case { stmt, .. } => {
                let label_idx = self.add_case_label(&binding.scope, binding.id);
                self.emit(new_node!(Label(label_idx.unwrap())));
                self.transform(stmt.clone());
            }
            ASTKind::Default { stmt, .. } => {
                let label_idx = self.add_case_label(&binding.scope, binding.id);
                self.emit(new_node!(Label(label_idx.unwrap())));
                self.transform(stmt.clone());
            }
            ASTKind::Break { .. } => {
                let idx = self.add_break_label(&binding.scope);
                let l = new_node!(Label(idx.unwrap()));
                self.emit(new_node!(Jump(l.clone())));
            }
            ASTKind::Continue { .. } => {
                let idx = self.add_continue_label(&binding.scope);
                let l = new_node!(Label(idx.unwrap()));
                self.emit(new_node!(Jump(l.clone())));
            }
            ASTKind::ExprStmt { expr } => {
                _ = self.transform_expr(expr.clone());
            }
            ASTKind::Variable { name, ty: _, init } => {
                if init.is_some() {
                    let e = self.transform_expr(init.as_ref().unwrap().clone());
                    let scope = &binding.scope;
                    let v = new_node!(Var(
                        binding.ty.clone(),
                        find_offset(scope.clone(), &name, None).unwrap(),
                    ));
                    self.emit(new_node!(Copy {
                        ty: binding.ty.clone(),
                        src: e,
                        dst: v
                    }));
                }
            }
            ASTKind::EmptyStmt => {}
            _ => {
                unreachable!();
            }
        }
    }

    fn emit(&mut self, ir: Rc<TAC>) {
        self.tac_code.push(ir.clone());
    }

    fn out(&mut self) -> Vec<Rc<TAC>> {
        std::mem::take(&mut self.tac_code)
    }

    pub fn generate(&mut self, ast: Vec<ASTRef>) -> Vec<Rc<TAC>> {
        self.tac_code.clear();
        for node in ast {
            self.transform(node);
        }
        self.tac_code.clone()
    }
}
