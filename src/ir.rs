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
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
use std::sync::OnceLock;

use crate::parser::{ASTKind, ASTRef, ASTVec};
use crate::scope::Scope;

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
        code: TACVec,
        depth: usize,
    },
    ConstInt(i64),
    Var(usize, usize),
    Inv {
        src: TACRef,
        dst: TACRef,
    },
    Neg {
        src: TACRef,
        dst: TACRef,
    },
    Not {
        src: TACRef,
        dst: TACRef,
    },
    Mul {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    IDiv {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Mod {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Add {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Sub {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    LShift {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    RShift {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    And {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Or {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Xor {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Equal {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    NotEq {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    LessThan {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    LessOrEq {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    GreaterThan {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    GreaterOrEq {
        lhs: TACRef,
        rhs: TACRef,
        dst: TACRef,
    },
    Copy {
        src: TACRef,
        dst: TACRef,
    },
    Jump(TACRef),
    JumpOnZero {
        expr: TACRef,
        label: TACRef,
    },
    JumpOnNotZero {
        expr: TACRef,
        label: TACRef,
    },
    Label(usize),
    Return(TACRef),
}

type TACRef = Rc<TAC>;
type TACVec = Vec<Rc<TAC>>;

fn get_label_map() -> &'static Mutex<HashMap<String, usize>> {
    static LABEL_MAP: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();
    LABEL_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

fn add_named_label(name: &str) {
    let map = get_label_map();
    let mut map_guard = map.lock().unwrap();
    map_guard
        .entry(name.to_string())
        .or_insert(incr_label_index());
}

fn get_named_label(name: &str) -> Option<usize> {
    let map = get_label_map();
    let map_guard = map.lock().unwrap();
    map_guard.get(name).cloned()
}

static LABEL_INDEX: AtomicUsize = AtomicUsize::new(0);

fn incr_label_index() -> usize {
    LABEL_INDEX.fetch_add(1, Ordering::SeqCst)
}

fn create_tmp_var(scope: &mut Scope) -> TACRef {
    let off = scope.add_tmp(4, 4);
    new_node!(Var(off, 4))
}

fn transform_expr(expr: ASTRef, tac_code: &mut TACVec) -> TACRef {
    let binding = expr.borrow();

    match &binding.kind {
        ASTKind::ConstInt(val) => new_node!(ConstInt(*val)),

        ASTKind::Complement { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Inv {
                src: src,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Negate { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Neg {
                src: src,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Not { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Not {
                src: src,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::PreIncr { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            let tmp = create_tmp_var(scope);
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

        ASTKind::PreDecr { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            let tmp = create_tmp_var(scope);
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

        ASTKind::PostIncr { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            let tmp = create_tmp_var(scope);
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

        ASTKind::PostDecr { expr: inner } => {
            let src = transform_expr(inner.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            let tmp = create_tmp_var(scope);
            let res = create_tmp_var(scope);
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
                dst: dst.clone(),
            }));
            tac_code.push(new_node!(Copy {
                src: dst.clone(),
                dst: src.clone()
            }));

            res
        }

        ASTKind::Multiply { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Mul {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Divide { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(IDiv {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Modulo { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Mod {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Add { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Add {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Subtract { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Sub {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::LShift { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(LShift {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::RShift { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(RShift {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::And { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(And {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Or { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Or {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Xor { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Xor {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::Equal { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(Equal {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::NotEq { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(NotEq {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::LessThan { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(LessThan {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));

            dst
        }

        ASTKind::LessOrEq { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(LessOrEq {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::GreaterThan { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(GreaterThan {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::GreaterOrEq { left, right } => {
            let lhs = transform_expr(left.clone(), tac_code);
            let rhs = transform_expr(right.clone(), tac_code);
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
            tac_code.push(new_node!(GreaterOrEq {
                lhs: lhs,
                rhs: rhs,
                dst: dst.clone()
            }));
            dst
        }

        ASTKind::LogicAnd { left, right } => {
            let false_label = new_node!(Label(incr_label_index()));
            let end_label = new_node!(Label(incr_label_index()));
            let x = transform_expr(left.clone(), tac_code);
            tac_code.push(new_node!(JumpOnZero {
                expr: x,
                label: false_label.clone()
            }));
            let y = transform_expr(right.clone(), tac_code);
            tac_code.push(new_node!(JumpOnZero {
                expr: y,
                label: false_label.clone()
            }));
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
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
            let true_label = new_node!(Label(incr_label_index()));
            let end_label = new_node!(Label(incr_label_index()));
            let x = transform_expr(left.clone(), tac_code);
            tac_code.push(new_node!(JumpOnNotZero {
                expr: x,
                label: true_label.clone()
            }));
            let y = transform_expr(right.clone(), tac_code);
            tac_code.push(new_node!(JumpOnNotZero {
                expr: y,
                label: true_label.clone()
            }));
            let scope: &mut Scope = &mut binding.scope.borrow_mut();
            let dst = create_tmp_var(scope);
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
            let exp = transform_expr(right.clone(), tac_code);
            let dst = transform_expr(left.clone(), tac_code);
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
            let e2_label = new_node!(Label(incr_label_index()));
            let c = transform_expr(left.clone(), tac_code);
            tac_code.push(new_node!(JumpOnZero {
                expr: c,
                label: e2_label.clone()
            }));
            let e1 = transform_expr(middle.clone(), tac_code);
            let res = create_tmp_var(&mut binding.scope.borrow_mut());
            tac_code.push(new_node!(Copy {
                src: e1.clone(),
                dst: res.clone()
            }));
            let end_label = new_node!(Label(incr_label_index()));
            tac_code.push(new_node!(Jump(end_label.clone())));
            tac_code.push(e2_label.clone());
            let e2 = transform_expr(right.clone(), tac_code);
            tac_code.push(new_node!(Copy {
                src: e2.clone(),
                dst: res.clone()
            }));

            tac_code.push(end_label.clone());

            res
        }

        ASTKind::Identifier { name: _, sym } => {
            new_node!(Var(sym.0, 4))
        }

        _ => unreachable!(),
    }
}

fn transform(node: ASTRef, tac_code: &mut TACVec) {
    let binding = node.borrow();

    match &binding.kind {
        ASTKind::Function {
            name,
            params: _,
            block,
            rtype: _,
            scope,
        } => {
            let mut func_code: TACVec = vec![];

            match &block.clone().unwrap().borrow().kind {
                ASTKind::Block { body } => {
                    for stmt in body {
                        transform(stmt.clone(), &mut func_code);
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
                transform(stmt.clone(), tac_code);
            }
        }

        ASTKind::If {
            cond,
            then,
            otherwise,
        } => {
            let else_label = if otherwise.is_some() {
                Some(new_node!(Label(incr_label_index())))
            } else {
                None
            };
            let end_label = new_node!(Label(incr_label_index()));
            let c = transform_expr(cond.clone(), tac_code);
            tac_code.push(new_node!(JumpOnZero {
                expr: c,
                label: if else_label.is_some() {
                    else_label.as_ref().unwrap().clone()
                } else {
                    end_label.clone()
                }
            }));

            transform(then.clone(), tac_code);

            if otherwise.is_some() {
                tac_code.push(new_node!(Jump(end_label.clone())));
                tac_code.push(else_label.unwrap());
                transform(otherwise.as_ref().unwrap().clone(), tac_code);
            }

            tac_code.push(end_label);
        }

        ASTKind::Return { expr } => {
            let e = transform_expr(expr.clone(), tac_code);
            tac_code.push(new_node!(Return(e)));
        }

        ASTKind::GoTo { label } => {
            add_named_label(label);
            let idx = get_named_label(label);
            let l = new_node!(Label(idx.unwrap()));
            tac_code.push(new_node!(Jump(l.clone())));
        }

        ASTKind::Label { name, stmt } => {
            add_named_label(name);
            let idx = get_named_label(name);
            let l = new_node!(Label(idx.unwrap()));
            tac_code.push(l);
            transform(stmt.clone(), tac_code);
        }

        ASTKind::ExprStmt { expr } => {
            _ = transform_expr(expr.clone(), tac_code);
        }

        ASTKind::Variable { name, typ: _, init } => {
            if init.is_some() {
                let e =
                    transform_expr(init.as_ref().unwrap().clone(), tac_code);
                let scope = binding.scope.borrow_mut();
                let v = new_node!(Var(scope.find_off(&name).unwrap(), 4));
                tac_code.push(new_node!(Copy { src: e, dst: v }));
            }
        }

        ASTKind::EmptyStmt => {}

        _ => {
            unreachable!();
        }
    }
}

pub fn generate(ast: ASTVec) -> TACVec {
    let mut code: TACVec = vec![];

    for node in ast {
        transform(node, &mut code);
    }

    return code;
}
