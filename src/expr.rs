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

use crate::as_a;
use crate::parser::{ASTKind, ASTRef};

fn is_lvalue(expr: ASTRef) -> bool {
    let node = expr.borrow();

    match &node.kind {
        ASTKind::Identifier { .. } => true,
        _ => false,
    }
}

pub fn eval(expr: ASTRef) -> Result<ASTRef, ()> {
    let node = expr.borrow();
    let scope = node.scope.clone();

    match &node.kind {
        ASTKind::Identifier { name, sym: _ } => {
            let s = scope.borrow().find_sym(name).and_then(|outer_rc| {
                outer_rc
                    .downcast::<ASTRef>()
                    .ok()
                    .map(|inner_rc| (*inner_rc).clone())
            });

            if s.is_some() {
                Ok(expr.clone())
            } else {
                Err(())
            }
        }

        ASTKind::Assign { left, right } => {
            if is_lvalue(left.clone()) {
                if eval(left.clone()).is_ok() && eval(right.clone()).is_ok() {
                    return Ok(expr.clone());
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
        | ASTKind::Xor { left, right }
        | ASTKind::LogicAnd { left, right }
        | ASTKind::LogicOr { left, right }
        | ASTKind::Equal { left, right }
        | ASTKind::NotEq { left, right }
        | ASTKind::LessThan { left, right }
        | ASTKind::LessOrEq { left, right }
        | ASTKind::GreaterThan { left, right }
        | ASTKind::GreaterOrEq { left, right } => {
            if eval(left.clone()).is_ok() && eval(right.clone()).is_ok() {
                Ok(expr.clone())
            } else {
                Err(())
            }
        }

        ASTKind::Conditional {
            left,
            middle,
            right,
        } => {
            if eval(left.clone()).is_err() {
                Err(())
            } else if eval(middle.clone()).is_err() {
                Err(())
            } else if eval(right.clone()).is_err() {
                Err(())
            } else {
                Ok(expr.clone())
            }
        }

        ASTKind::Negate { expr }
        | ASTKind::Complement { expr }
        | ASTKind::Not { expr } => {
            if eval(expr.clone()).is_ok() {
                Ok(expr.clone())
            } else {
                Err(())
            }
        }

        ASTKind::PreIncr { expr }
        | ASTKind::PreDecr { expr }
        | ASTKind::PostIncr { expr }
        | ASTKind::PostDecr { expr } => {
            if eval(expr.clone()).is_ok() {
                as_a!(Some(expr), ASTKind::Identifier {
                    name: _,
                    sym: _
                } => {
                    if eval(expr.clone()).is_ok() {
                        return Ok(expr.clone());
                    } else {
                        return Err(());
                    }
                } else {
                    Err(())
                })
            } else {
                Err(())
            }
        }

        ASTKind::ConstInt(_) => Ok(expr.clone()),

        _ => Err(()),
    }
}
