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

use std::rc::Rc;

use crate::ast::{ASTKind, AST};

fn is_lvalue(expr: Rc<AST>) -> bool {
    match &expr.kind {
        ASTKind::Identifier { .. } => true,
        _ => false,
    }
}

pub fn eval(expr: Rc<AST>, constexpr: bool) -> Result<Rc<AST>, ()> {
    let node = expr.clone();
    match &node.kind {
        ASTKind::Identifier { name: _, sym } => {
            if constexpr || sym.is_none() {
                Err(())
            } else {
                Ok(expr)
            }
        }

        ASTKind::Assign { left, right } => {
            if is_lvalue(left.clone()) {
                if eval(left.clone(), constexpr).is_ok()
                    && eval(right.clone(), constexpr).is_ok()
                {
                    return Ok(expr);
                }
            };

            return Err(());
        }

        ASTKind::Add { left, right }
        | ASTKind::Subtract { left, right }
        | ASTKind::Multiply { left, right }
        | ASTKind::Divide { left, right }
        | ASTKind::Modulo { left, right }
        | ASTKind::LShift { left, right }
        | ASTKind::RShift { left, right }
        | ASTKind::And { left, right }
        | ASTKind::Or { left, right }
        | ASTKind::Xor { left, right } => {
            let left_eval = eval(left.clone(), constexpr)?;
            let right_eval = eval(right.clone(), constexpr)?;

            if let (ASTKind::ConstInt(lhs), ASTKind::ConstInt(rhs)) =
                (&left_eval.kind, &right_eval.kind)
            {
                let result = match &node.kind {
                    ASTKind::Add { .. } => lhs + rhs,
                    ASTKind::Subtract { .. } => lhs - rhs,
                    ASTKind::Multiply { .. } => lhs * rhs,
                    ASTKind::Divide { .. } => {
                        if *rhs == 0 {
                            return Err(());
                        }
                        lhs / rhs
                    }
                    ASTKind::Modulo { .. } => {
                        if *rhs == 0 {
                            return Err(());
                        }
                        lhs % rhs
                    }
                    ASTKind::LShift { .. } => lhs << rhs,
                    ASTKind::RShift { .. } => lhs >> rhs,
                    ASTKind::And { .. } => lhs & rhs,
                    ASTKind::Or { .. } => lhs | rhs,
                    ASTKind::Xor { .. } => lhs ^ rhs,
                    _ => unreachable!(),
                };

                return Ok(Rc::new(AST {
                    id: 0,
                    kind: ASTKind::ConstInt(result),
                    scope: node.scope.clone(),
                }));
            }

            Ok(expr)
        }

        ASTKind::LogicAnd { left, right }
        | ASTKind::LogicOr { left, right } => {
            let left_eval = eval(left.clone(), constexpr)?;

            if let ASTKind::ConstInt(lhs) = left_eval.kind {
                let result = match &node.kind {
                    ASTKind::LogicAnd { .. } => {
                        if lhs == 0 {
                            return Ok(Rc::new(AST {
                                id: 0,
                                kind: ASTKind::ConstInt(0),
                                scope: node.scope.clone(),
                            }));
                        }
                        let right_eval = eval(right.clone(), constexpr)?;
                        let rhs =
                            if let ASTKind::ConstInt(rhs) = right_eval.kind {
                                rhs
                            } else {
                                return Ok(expr);
                            };
                        ((lhs != 0 && rhs != 0) as i32).into()
                    }
                    ASTKind::LogicOr { .. } => {
                        if lhs != 0 {
                            return Ok(Rc::new(AST {
                                id: 0,
                                kind: ASTKind::ConstInt(1),
                                scope: node.scope.clone(),
                            }));
                        }
                        let right_eval = eval(right.clone(), constexpr)?;
                        let rhs =
                            if let ASTKind::ConstInt(rhs) = right_eval.kind {
                                rhs
                            } else {
                                return Ok(expr);
                            };
                        ((lhs != 0 || rhs != 0) as i32).into()
                    }
                    _ => unreachable!(),
                };

                return Ok(Rc::new(AST {
                    id: 0,
                    kind: ASTKind::ConstInt(result),
                    scope: node.scope.clone(),
                }));
            }

            Ok(expr)
        }

        ASTKind::Equal { left, right }
        | ASTKind::NotEq { left, right }
        | ASTKind::LessThan { left, right }
        | ASTKind::LessOrEq { left, right }
        | ASTKind::GreaterThan { left, right }
        | ASTKind::GreaterOrEq { left, right } => {
            let left_eval = eval(left.clone(), constexpr)?;
            let right_eval = eval(right.clone(), constexpr)?;

            if let (ASTKind::ConstInt(lhs), ASTKind::ConstInt(rhs)) =
                (&left_eval.kind, &right_eval.kind)
            {
                let result = match &node.kind {
                    ASTKind::Equal { .. } => (lhs == rhs) as i64,
                    ASTKind::NotEq { .. } => (lhs != rhs) as i64,
                    ASTKind::LessThan { .. } => (lhs < rhs) as i64,
                    ASTKind::LessOrEq { .. } => (lhs <= rhs) as i64,
                    ASTKind::GreaterThan { .. } => (lhs > rhs) as i64,
                    ASTKind::GreaterOrEq { .. } => (lhs >= rhs) as i64,
                    _ => unreachable!(),
                };

                return Ok(Rc::new(AST {
                    id: 0,
                    kind: ASTKind::ConstInt(result),
                    scope: node.scope.clone(),
                }));
            }

            Ok(expr)
        }

        ASTKind::Conditional {
            left,
            middle,
            right,
        } => {
            let cond_eval = eval(left.clone(), constexpr)?;

            if let ASTKind::ConstInt(cond_value) = cond_eval.kind {
                let branch = if cond_value != 0 {
                    middle.clone()
                } else {
                    right.clone()
                };

                return eval(branch, constexpr);
            }

            Ok(expr)
        }

        ASTKind::Negate { expr: inner }
        | ASTKind::Complement { expr: inner }
        | ASTKind::Not { expr: inner } => {
            let eval_expr = eval(inner.clone(), constexpr)?;

            if let ASTKind::ConstInt(value) = eval_expr.kind {
                let result = match &node.kind {
                    ASTKind::Negate { .. } => -value,
                    ASTKind::Complement { .. } => !value,
                    ASTKind::Not { .. } => ((value == 0) as i32).into(),
                    _ => unreachable!(),
                };

                return Ok(Rc::new(AST {
                    id: 0,
                    kind: ASTKind::ConstInt(result),
                    scope: node.scope.clone(),
                }));
            }

            Ok(expr)
        }

        ASTKind::PreIncr { expr: inner }
        | ASTKind::PostIncr { expr: inner }
        | ASTKind::PreDecr { expr: inner }
        | ASTKind::PostDecr { expr: inner } => {
            if !is_lvalue(eval(inner.clone(), constexpr)?) {
                return Err(());
            }

            Ok(expr)
        }

        ASTKind::ConstInt(_) => Ok(expr),

        _ => Err(()),
    }
}
