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
use std::rc::Rc;

use crate::ast::*;
use crate::scope::*;
use crate::types::*;

fn is_callable(expr: AstRef) -> bool {
    if let AstKind::Identifier { .. } = &expr.borrow().kind {
        if let Some(sym) = resolve(&expr) {
            if let Some(node) = sym_as_node(sym.clone()) {
                match &node.borrow().kind {
                    AstKind::Function { .. } => return true,
                    _ => return false,
                }
            }
        }
    }

    false
}

fn is_lvalue(expr: AstRef) -> bool {
    matches!(&expr.borrow().kind, AstKind::Identifier { .. })
}

pub fn eval(expr: AstRef) -> Result<AstRef, String> {
    let node = expr.borrow();
    match &node.kind {
        AstKind::Identifier { name, .. } => {
            if resolve(&expr).is_some() {
                Ok(expr.clone())
            } else {
                Err(format!("{} not found", name))
            }
        }

        AstKind::Assign { left, right } => {
            if is_lvalue(left.clone()) {
                eval(left.clone())?;
                eval(right.clone())?;
                Ok(expr.clone())
            } else {
                Err("not an lvalue".to_string())
            }
        }

        AstKind::Add { left, right }
        | AstKind::Subtract { left, right }
        | AstKind::Multiply { left, right }
        | AstKind::Divide { left, right }
        | AstKind::Modulo { left, right }
        | AstKind::LShift { left, right }
        | AstKind::RShift { left, right }
        | AstKind::And { left, right }
        | AstKind::Or { left, right }
        | AstKind::Xor { left, right } => {
            let left_eval = eval(left.clone())?;
            let right_eval = eval(right.clone())?;

            if let (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) =
                (&left_eval.borrow().kind, &right_eval.borrow().kind)
            {
                let result = match &node.kind {
                    AstKind::Add { .. } => lhs + rhs,
                    AstKind::Subtract { .. } => lhs - rhs,
                    AstKind::Multiply { .. } => lhs * rhs,
                    AstKind::Divide { .. } => {
                        if *rhs == 0 {
                            return Err("attempt to divide by 0".to_string());
                        }
                        lhs / rhs
                    }
                    AstKind::Modulo { .. } => {
                        if *rhs == 0 {
                            return Err("attempt to divide by 0".to_string());
                        }
                        lhs % rhs
                    }
                    AstKind::LShift { .. } => lhs << rhs,
                    AstKind::RShift { .. } => lhs >> rhs,
                    AstKind::And { .. } => lhs & rhs,
                    AstKind::Or { .. } => lhs | rhs,
                    AstKind::Xor { .. } => lhs ^ rhs,
                    _ => unreachable!(),
                };

                return Ok(Rc::new(RefCell::new(Ast {
                    id: 0,
                    ty: int_type(),
                    kind: AstKind::ConstInt(result),
                    scope: node.scope.clone(),
                })));
            }

            Ok(expr.clone())
        }

        AstKind::LogicAnd { left, right }
        | AstKind::LogicOr { left, right } => {
            let left_eval = eval(left.clone())?;

            if let AstKind::ConstInt(lhs) = left_eval.borrow().kind {
                let result = match &node.kind {
                    AstKind::LogicAnd { .. } => {
                        if lhs == 0 {
                            return Ok(Rc::new(RefCell::new(Ast {
                                id: 0,
                                ty: int_type(),
                                kind: AstKind::ConstInt(0),
                                scope: node.scope.clone(),
                            })));
                        }
                        let right_eval = eval(right.clone())?;
                        let rhs = if let AstKind::ConstInt(rhs) =
                            right_eval.borrow().kind
                        {
                            rhs
                        } else {
                            return Ok(expr.clone());
                        };
                        ((lhs != 0 && rhs != 0) as i32).into()
                    }
                    AstKind::LogicOr { .. } => {
                        if lhs != 0 {
                            return Ok(Rc::new(RefCell::new(Ast {
                                id: 0,
                                ty: int_type(),
                                kind: AstKind::ConstInt(1),
                                scope: node.scope.clone(),
                            })));
                        }
                        let right_eval = eval(right.clone())?;
                        let rhs = if let AstKind::ConstInt(rhs) =
                            right_eval.borrow().kind
                        {
                            rhs
                        } else {
                            return Ok(expr.clone());
                        };
                        ((lhs != 0 || rhs != 0) as i32).into()
                    }
                    _ => unreachable!(),
                };

                return Ok(Rc::new(RefCell::new(Ast {
                    id: 0,
                    ty: int_type(),
                    kind: AstKind::ConstInt(result),
                    scope: node.scope.clone(),
                })));
            }

            Ok(expr.clone())
        }

        AstKind::Equal { left, right }
        | AstKind::NotEq { left, right }
        | AstKind::LessThan { left, right }
        | AstKind::LessOrEq { left, right }
        | AstKind::GreaterThan { left, right }
        | AstKind::GreaterOrEq { left, right } => {
            let left_eval = eval(left.clone())?;
            let right_eval = eval(right.clone())?;

            if let (AstKind::ConstInt(lhs), AstKind::ConstInt(rhs)) =
                (&left_eval.borrow().kind, &right_eval.borrow().kind)
            {
                let result = match &node.kind {
                    AstKind::Equal { .. } => (lhs == rhs) as i64,
                    AstKind::NotEq { .. } => (lhs != rhs) as i64,
                    AstKind::LessThan { .. } => (lhs < rhs) as i64,
                    AstKind::LessOrEq { .. } => (lhs <= rhs) as i64,
                    AstKind::GreaterThan { .. } => (lhs > rhs) as i64,
                    AstKind::GreaterOrEq { .. } => (lhs >= rhs) as i64,
                    _ => unreachable!(),
                };

                return Ok(Rc::new(RefCell::new(Ast {
                    id: 0,
                    ty: int_type(),
                    kind: AstKind::ConstInt(result),
                    scope: node.scope.clone(),
                })));
            }

            Ok(expr.clone())
        }

        AstKind::Ternary {
            left,
            middle,
            right,
        } => {
            let cond_eval = eval(left.clone())?;

            if let AstKind::ConstInt(cond_value) = cond_eval.borrow().kind {
                let branch = if cond_value != 0 {
                    middle.clone()
                } else {
                    right.clone()
                };

                return eval(branch);
            }

            Ok(expr.clone())
        }

        AstKind::Negate { expr: inner }
        | AstKind::Complement { expr: inner }
        | AstKind::Not { expr: inner } => {
            let eval_expr = eval(inner.clone())?;

            if let AstKind::ConstInt(value) = eval_expr.borrow().kind {
                let result = match &node.kind {
                    AstKind::Negate { .. } => -value,
                    AstKind::Complement { .. } => !value,
                    AstKind::Not { .. } => ((value == 0) as i32).into(),
                    _ => unreachable!(),
                };

                return Ok(Rc::new(RefCell::new(Ast {
                    id: 0,
                    ty: int_type(),
                    kind: AstKind::ConstInt(result),
                    scope: node.scope.clone(),
                })));
            }

            Ok(expr.clone())
        }

        AstKind::PostIncr { expr: inner }
        | AstKind::PostDecr { expr: inner } => {
            if !is_lvalue(eval(inner.clone())?) {
                return Err("not an lvalue".to_string());
            }

            Ok(expr.clone())
        }

        AstKind::Call {
            expr: callee,
            args: _,
        } => {
            if !is_callable(eval(callee.clone())?) {
                return Err("not a function".to_string());
            }

            Ok(expr.clone())
        }

        AstKind::ConstInt(_) => Ok(expr.clone()),

        _ => unreachable!(),
    }
}
